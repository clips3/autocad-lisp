"""Prove that the integration suite catches three deliberately planted changes.

Uses the frozen original source, never edits PP_CHLAY.lsp or baseline.json.
The mutation anchors intentionally refer to that original implementation.
"""
import argparse
import json
from pathlib import Path
import subprocess

from run import BASELINE, ROOT, compare, execute, sha
from terminal import Terminal, configure_output, number


def main():
    configure_output()
    terminal = Terminal()
    terminal.heading("VALIDAÇÃO DO DETECTOR DE REGRESSÕES")
    baseline = json.loads(BASELINE.read_text(encoding="utf-8"))
    source = subprocess.run(["git", "show", f"{baseline['reference']}:PP_CHLAY.lsp"],
                            cwd=ROOT, capture_output=True, check=True).stdout
    if sha(source) != baseline["source_sha256"]:
        raise RuntimeError("Reference does not match captured baseline")
    mutations = [
        (b'"NOMEPILAR" . "PP_TEXTO PILAR"', b'"NOMEPILAR" . "PP_LAYOUT"'),
        (b'(entdel ent)', b'(princ)'),
        (b'(vla-put-color vla-obj 256)', b'(vla-put-color vla-obj 1)'),
    ]
    for original, replacement in mutations:
        if original not in source:
            raise RuntimeError("Mutation anchor missing; review this validation for the new baseline")
        source = source.replace(original, replacement)
    args = argparse.Namespace(acad=None, template=None, timeout=600)
    result, folder = execute(source, args, terminal)
    differences = compare(baseline["records"], result["records"])
    checks = {
        "layer_remapping": any(d["path"].startswith("/clean/rule-")
                               and "/layer" in d["path"]
                               and d["expected"] == "PP_TEXTO PILAR"
                               and d["actual"] == "PP_LAYOUT" for d in differences),
        "deletion": "clean/matrix-02-TEXT" not in baseline["records"]
                    and "clean/matrix-02-TEXT" in result["records"],
        # AutoCAD omits DXF 62 when color is ByLayer.
        "bylayer_color": any(d["path"].startswith("/manual/PP_CUSTOM_TEST/LINE")
                             and "/color" in d["path"] and d["expected"] is None
                             and d["actual"] == [1] for d in differences),
    }
    report = {"checks": checks, "difference_count": len(differences), "differences": differences}
    (folder / "mutation-report.json").write_text(json.dumps(report, ensure_ascii=False, indent=2) + "\n", encoding="utf-8")
    terminal.rule()
    if all(checks.values()):
        terminal.line("  [APROVADO] As três regressões propositais foram detectadas.", "good")
    else:
        terminal.line("  [FALHOU] O detector não identificou todas as regressões propositais.", "bad")
    for key, label in (("layer_remapping", "Troca da camada de destino"),
                       ("deletion", "Exclusão de objetos desativada"),
                       ("bylayer_color", "Cor explícita no lugar de ByLayer")):
        terminal.line(f"  {label:<38} " + terminal.paint(
            "DETECTADA" if checks[key] else "NÃO DETECTADA", "good" if checks[key] else "bad"))
    terminal.line(f"  Diferenças provocadas: {number(len(differences))}")
    terminal.line("  Nesta validação, encontrar essas diferenças é o resultado esperado.")
    terminal.line(f"  Relatório: {folder / 'mutation-report.json'}")
    terminal.rule()
    return 0 if all(checks.values()) else 1


if __name__ == "__main__":
    raise SystemExit(main())
