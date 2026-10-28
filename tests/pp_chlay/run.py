"""Run PP_CHLAY regression tests in a NEW, disposable AutoCAD process (Windows).

Python 3.10+, stdlib only. Never connects to an existing AutoCAD session.
"""
from __future__ import annotations

import argparse
from collections import defaultdict
from datetime import datetime
import hashlib
import json
import math
import os
from pathlib import Path
import re
import subprocess
import sys
import time

from terminal import Terminal, configure_output, number, seconds

HERE = Path(__file__).resolve().parent
ROOT = HERE.parent.parent
REFERENCE = "d0b5415"
BASELINE = HERE / "baseline.json"


def sha(data: bytes) -> str:
    return hashlib.sha256(data).hexdigest()


def lisp(value):
    """Serialize only fixture data, never interpolate shell command text."""
    if value is None:
        return "nil"
    if isinstance(value, str):
        return '"' + value.replace("\\", "\\\\").replace('"', '\\"') + '"'
    if isinstance(value, list):
        return "(" + " ".join(lisp(item) for item in value) + ")"
    raise TypeError(type(value))


def parse_lisp(text: str):
    """Read the proper lists/strings/numbers emitted by the real AutoCAD."""
    tokens = re.findall(r'"(?:\\.|[^"\\])*"|[()]|[^\s()]+', text)
    index = 0

    def parse():
        nonlocal index
        if index >= len(tokens):
            raise ValueError("Truncated AutoCAD record")
        token = tokens[index]
        index += 1
        if token == "(":
            items = []
            while index < len(tokens) and tokens[index] != ")":
                items.append(parse())
            if index == len(tokens):
                raise ValueError("Unclosed AutoCAD record")
            index += 1
            return items
        if token == ")":
            raise ValueError("Unexpected closing parenthesis")
        if token.startswith('"'):
            # AutoLISP escapes, rather than Python/JSON's Unicode escapes.
            return re.sub(r'\\(.)', lambda m: {"n": "\n", "r": "\r", "t": "\t"}.get(m[1], m[1]), token[1:-1])
        if token.lower() == "nil":
            return None
        if token.lower() in ("t", ":vlax-true"):
            return True
        if token.lower() == ":vlax-false":
            return False
        try:
            return float(token) if any(c in token.lower() for c in ".e") else int(token)
        except ValueError:
            raise ValueError(f"Unsupported AutoCAD value: {token!r}") from None

    result = parse()
    if index != len(tokens):
        raise ValueError("Trailing data in AutoCAD record")
    return result


def compare(expected, actual, path="") -> list[dict]:
    """Structural diffs, with numeric tolerance for geometry/ActiveX floats."""
    if isinstance(expected, dict) and isinstance(actual, dict):
        changes = []
        for key in sorted(expected.keys() | actual.keys()):
            location = f"{path}/{key}"
            if key not in expected or key not in actual:
                changes.append({"path": location, "expected": expected.get(key), "actual": actual.get(key)})
            else:
                changes.extend(compare(expected[key], actual[key], location))
        return changes
    if isinstance(expected, list) and isinstance(actual, list):
        if len(expected) != len(actual):
            return [{"path": path, "expected": expected, "actual": actual}]
        changes = []
        for index, (left, right) in enumerate(zip(expected, actual)):
            changes.extend(compare(left, right, f"{path}[{index}]"))
        return changes
    if (isinstance(expected, (float, int)) and not isinstance(expected, bool)
            and isinstance(actual, (float, int)) and not isinstance(actual, bool)):
        same = math.isclose(expected, actual, rel_tol=1e-9, abs_tol=1e-7)
    else:
        same = type(expected) is type(actual) and expected == actual
    return [] if same else [{"path": path, "expected": expected, "actual": actual}]


def read_output(path: Path) -> dict:
    groups = defaultdict(list)
    for line in path.read_text(encoding="utf-8-sig").splitlines():
        if not line.strip():
            continue
        record = parse_lisp(line)
        if not isinstance(record, list) or len(record) != 2 or not isinstance(record[0], str):
            raise ValueError(f"Invalid AutoCAD record: {line[:120]}")
        value = record[1]
        if (isinstance(value, list) and value and isinstance(value[0], list)
                and value[0] and value[0][0] == 0):
            value = snapshot_fields(value)
        groups[record[0]].append(value)
    if "fatal" in groups:
        raise RuntimeError(f"Falha durante os cenários no AutoCAD: {groups['fatal']}")
    if groups.pop("complete", []) != [True]:
        raise RuntimeError("O AutoCAD não concluiu todas as etapas. Resultados parciais não significam aprovação.")
    environment = groups.pop("environment")
    timings = {key: groups.pop(key)[0] for key in list(groups) if key.startswith("timing/")}
    # Copies inherit fixture IDs; compare multisets, including multiplicity.
    records = {key: sorted(values, key=lambda v: json.dumps(v, ensure_ascii=False))
               for key, values in sorted(groups.items())}
    return {"environment": environment, "timings_ms": timings, "records": records}


def snapshot_fields(fields: list) -> dict:
    names = {0: "type", 8: "layer", 62: "color", 6: "linetype", 48: "linetype_scale",
             370: "lineweight", 440: "transparency", 70: "flags", 2: "pattern",
             41: "pattern_scale_dxf", 52: "pattern_angle_dxf", 10: "vertices",
             11: "endpoint", 38: "elevation", 39: "thickness", 210: "normal",
             67: "paperspace", 410: "layout"}
    result = {}
    for field in fields:
        if field[0] == "hatch":
            result["hatch"] = dict(zip(("pattern", "scale", "angle", "area"), field[1:]))
        else:
            result[names[field[0]]] = field[1]
    return result


def locate_engine(args) -> tuple[Path, Path]:
    if os.name != "nt":
        raise RuntimeError("Os testes exigem Windows e AutoCAD completo com ActiveX.")
    acad = Path(args.acad) if args.acad else next(iter(sorted(
        Path(os.environ.get("ProgramFiles", "C:/Program Files")).glob("Autodesk/AutoCAD 20*/acad.exe"), reverse=True)), None)
    if acad is None or not acad.is_file():
        raise RuntimeError("AutoCAD não encontrado. Informe o executável com --acad CAMINHO.")
    templates = sorted(Path(os.environ["LOCALAPPDATA"]).glob(
        f"Autodesk/{acad.parent.name}/R*/*/Template/acadiso.dwt"))
    template = Path(args.template) if args.template else next(iter(templates), None)
    if template is None or not template.is_file():
        raise RuntimeError("acadiso.dwt não encontrado. Informe o arquivo com --template CAMINHO.")
    return acad.resolve(), template.resolve()


def execute(source: bytes, args, terminal=None) -> tuple[dict, Path]:
    terminal = Terminal() if terminal is None else terminal
    acad, template = locate_engine(args)
    folder = HERE / "runs" / datetime.now().strftime("%Y%m%d-%H%M%S-%f")
    folder.mkdir(parents=True)
    (folder / "source.lsp").write_bytes(source)
    harness = (HERE / "harness.lsp").read_bytes()
    (folder / "harness.lsp").write_bytes(harness)
    cases_bytes = (HERE / "cases.json").read_bytes()
    cases = json.loads(cases_bytes)
    fixtures = "\n".join(f"(setq {symbol} '{lisp(cases[name])})" for symbol, name in (
        ("*pp-test-cases*", "entities"), ("*pp-test-matches*", "matches"),
        ("*pp-test-helpers*", "helpers"), ("*pp-test-manual*", "manual")))
    (folder / "fixtures.lsp").write_text(fixtures + "\n", encoding="utf-8-sig")
    output = folder / "actual.sexp"
    # Trust ONLY this run's own directory while loading, and restore immediately.
    # No SECURELOAD change and no changes to any existing drawing/session.
    quoted_folder = lisp(folder.as_posix())
    loads = " ".join(f"(load {lisp((folder / name).as_posix())})"
                     for name in ("source.lsp", "harness.lsp", "fixtures.lsp"))
    script = [
        '(setq pp-bootstrap-trusted (getvar "TRUSTEDPATHS"))',
        f'(setvar "TRUSTEDPATHS" (strcat pp-bootstrap-trusted ";" {quoted_folder}))',
        f"(setq pp-bootstrap-result (vl-catch-all-apply '(lambda () {loads})))",
        '(setvar "TRUSTEDPATHS" pp-bootstrap-trusted)',
        f'(if (vl-catch-all-error-p pp-bootstrap-result) (progn (setq pp-bootstrap-file (open {lisp(output.as_posix())} "w")) (prin1 (list "fatal" (vl-catch-all-error-message pp-bootstrap-result)) pp-bootstrap-file) (close pp-bootstrap-file)) (pp-test:run {lisp(output.as_posix())}))',
        '_.QUIT',
        '_N',
        '',
    ]
    script_path = folder / "run.scr"
    script_path.write_text("\n".join(script), encoding="utf-8-sig")
    info = subprocess.STARTUPINFO()
    info.dwFlags = subprocess.STARTF_USESHOWWINDOW
    info.wShowWindow = 0
    command = [str(acad), "/nologo", "/t", str(template), "/b", str(script_path)]
    terminal.activity(f"Executando os cenários no {acad.parent.name}...")
    terminal.line(f"  Entradas: {number(len(cases['entities']))} objetos por lote e "
                  f"{number(len(cases['matches']))} comparações de nomes.")
    started = time.monotonic()
    process = subprocess.Popen(command, cwd=folder, startupinfo=info)
    try:
        completed = False
        next_notice = started + 30
        while time.monotonic() - started < args.timeout:
            if output.exists():
                text = output.read_text(encoding="utf-8-sig", errors="replace")
                if '("complete" T)' in text or '("fatal" ' in text:
                    completed = True
                    break
            if process.poll() is not None:
                break
            if time.monotonic() >= next_notice:
                terminal.activity(f"Aguardando o AutoCAD concluir... {seconds(time.monotonic() - started)}")
                next_notice += 30
            time.sleep(0.5)
        if not completed:
            raise RuntimeError(f"O AutoCAD encerrou ou excedeu o tempo limite sem concluir os testes. "
                               f"Arquivos da execução: {folder}")
        # The completion record is written just before closing the stream.
        time.sleep(1)
        result = read_output(output)
    finally:
        try:
            process.wait(timeout=6)
        except subprocess.TimeoutExpired:
            # Only the child we started; NEVER taskkill /IM acad.exe.
            process.terminate()
            process.wait(timeout=15)
    result.update({"schema": 1, "source_sha256": sha(source),
                   "cases_sha256": sha(cases_bytes), "harness_sha256": sha(harness),
                   "acad": str(acad), "elapsed_seconds": round(time.monotonic() - started, 2)})
    (folder / "actual.json").write_text(json.dumps(result, ensure_ascii=False, indent=2) + "\n", encoding="utf-8")
    return result, folder


def main() -> int:
    configure_output()
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("mode", choices=("record", "check"))
    parser.add_argument("--source", type=Path, default=ROOT / "PP_CHLAY.lsp")
    parser.add_argument("--ref", default=REFERENCE, help="Frozen git reference used only by record")
    parser.add_argument("--baseline", type=Path, default=BASELINE)
    parser.add_argument("--replace-baseline", action="store_true")
    parser.add_argument("--acad")
    parser.add_argument("--template")
    parser.add_argument("--timeout", type=int, default=600)
    parser.add_argument("--color", choices=("auto", "always", "never"), default="auto",
                        help="Cores no terminal: auto (padrão), always ou never")
    args = parser.parse_args()
    terminal = Terminal(args.color)
    terminal.heading("CAPTURA DA REFERÊNCIA" if args.mode == "record" else "TESTES DE REGRESSÃO")
    try:
        if args.mode == "record":
            if args.baseline.exists() and not args.replace_baseline:
                raise RuntimeError("A referência já existe. Revise as mudanças antes de usar --replace-baseline.")
            terminal.activity(f"Preparando a captura do commit {args.ref}...")
            source = subprocess.run(["git", "show", f"{args.ref}:PP_CHLAY.lsp"], cwd=ROOT,
                                    capture_output=True, check=True).stdout
            result, folder = execute(source, args, terminal)
            result["reference"] = args.ref
            # One record per line makes baseline changes reviewable in Git.
            metadata = {key: value for key, value in result.items() if key != "records"}
            lines = [json.dumps(metadata, ensure_ascii=False, indent=2)[:-2] + ',\n  "records": {']
            entries = list(result["records"].items())
            for index, (key, value) in enumerate(entries):
                suffix = "," if index < len(entries) - 1 else ""
                lines.append("    " + json.dumps(key, ensure_ascii=False) + ": "
                             + json.dumps(value, ensure_ascii=False) + suffix)
            lines.append("  }\n}\n")
            args.baseline.write_text("\n".join(lines), encoding="utf-8")
            terminal.recorded(result, args.ref, args.baseline)
            return 0
        terminal.activity("Conferindo a referência e os arquivos de teste...")
        baseline = json.loads(args.baseline.read_text(encoding="utf-8"))
        for field, filename in (("cases_sha256", "cases.json"), ("harness_sha256", "harness.lsp")):
            if baseline[field] != sha((HERE / filename).read_bytes()):
                raise RuntimeError(f"{filename} mudou desde a captura da referência. "
                                   "Revise os testes e capture novamente o commit de referência.")
        result, folder = execute(args.source.read_bytes(), args, terminal)
        if result["environment"] != baseline["environment"]:
            raise RuntimeError(f"AutoCAD/LISPSYS diferente da referência: "
                               f"{baseline['environment']} -> {result['environment']}")
        terminal.activity("Comparando os resultados com a referência...")
        changes = compare(baseline["records"], result["records"])
        report = {"passed": not changes, "differences": changes,
                  "baseline_source_sha256": baseline["source_sha256"],
                  "candidate_source_sha256": result["source_sha256"],
                  "timings_ms": result["timings_ms"]}
        report_path = folder / "comparison.json"
        report_path.write_text(json.dumps(report, ensure_ascii=False, indent=2) + "\n", encoding="utf-8")
        terminal.summary(baseline, result, changes, report_path)
        return 1 if changes else 0
    except (OSError, ValueError, KeyError, RuntimeError, subprocess.SubprocessError) as error:
        Terminal(args.color, stream=sys.stderr).error(error)
        return 2


if __name__ == "__main__":
    raise SystemExit(main())
