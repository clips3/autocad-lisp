"""Compare repeated real AutoCAD workloads, keeping the original source frozen."""
import argparse
from datetime import datetime
import json
from pathlib import Path
from statistics import median
import subprocess

from expectations import expected_baseline
from run import BASELINE, HERE, ROOT, compare, execute, sha
from terminal import Terminal, configure_output, seconds


def main():
    configure_output()
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--runs", type=int, default=3)
    parser.add_argument("--hatches", type=int, default=500)
    parser.add_argument("--acad")
    parser.add_argument("--template")
    parser.add_argument("--timeout", type=int, default=600)
    args = parser.parse_args()
    if args.runs < 1 or args.hatches < 1:
        parser.error("--runs e --hatches precisam ser maiores que zero")
    terminal = Terminal()
    terminal.heading("COMPARAÇÃO DE DESEMPENHO")
    args.contracts = False
    args.benchmark_hatches = True
    args.hatch_count = args.hatches
    baseline = json.loads(BASELINE.read_text(encoding="utf-8"))
    expected, _ = expected_baseline(baseline, HERE / "expected_changes.json")
    original = subprocess.run(["git", "show", f"{baseline['reference']}:PP_CHLAY.lsp"],
                              cwd=ROOT, check=True, capture_output=True).stdout
    if sha(original) != baseline["source_sha256"]:
        raise RuntimeError("O fonte de referência não coincide com a captura original")
    candidate = (ROOT / "PP_CHLAY.lsp").read_bytes()
    samples = {"original": [], "optimized": []}
    runs = []
    original_hatches = None
    for index in range(args.runs):
        for name, label, source, oracle in (("original", "ORIGINAL", original, baseline),
                                           ("optimized", "OTIMIZADA", candidate, expected)):
            terminal.activity(f"Amostra {index + 1}/{args.runs} | versão {label}")
            result, folder = execute(source, args, terminal)
            differences = compare(oracle["records"], result["records"])
            if result["environment"] != baseline["environment"] or differences:
                raise RuntimeError(f"A amostra {name} mudou o comportamento ou o ambiente: {folder}")
            if original_hatches is None:
                original_hatches = result["benchmark_records"]
            differences = compare(original_hatches, result["benchmark_records"])
            if differences:
                raise RuntimeError(f"O cenário de hachuras mudou o resultado: {folder}")
            samples[name].append(result["timings_ms"])
            runs.append({"version": name, "folder": str(folder), "source_sha256": result["source_sha256"],
                         "timings_ms": result["timings_ms"]})
    summary = {}
    for key in samples["original"][0]:
        before = median(sample[key] for sample in samples["original"])
        after = median(sample[key] for sample in samples["optimized"])
        summary[key] = {"original_median_ms": before, "optimized_median_ms": after,
                        "reduction_percent": round((1 - after / before) * 100, 1) if before else None}
    folder = HERE / "runs" / ("benchmark-" + datetime.now().strftime("%Y%m%d-%H%M%S"))
    folder.mkdir()
    report = {"sample_count_per_version": args.runs, "hatch_count": args.hatches,
              "summary": summary, "runs": runs, "behavior_verified": True}
    path = folder / "benchmark.json"
    path.write_text(json.dumps(report, ensure_ascii=False, indent=2) + "\n", encoding="utf-8")
    terminal.rule()
    terminal.line("  [CONCLUÍDO] Resultados equivalentes em todas as amostras.", "good")
    terminal.line(f"  Mediana de {args.runs} execução(ões) por versão; {args.hatches} hachuras.")
    terminal.line(f"  {'CENÁRIO':<24} {'ORIGINAL':>12} {'OTIMIZADA':>12} {'REDUÇÃO':>10}")
    for key, label in (("timing/clean-ms", "CHLAUTO"), ("timing/delete-ms", "CHLDEL"),
                       ("timing/hatch-first-ms", "Hachuras: primeira vez"),
                       ("timing/hatch-repeat-ms", "Hachuras: repetição")):
        data = summary[key]
        reduction = data["reduction_percent"]
        terminal.line(f"  {label:<24} {seconds(data['original_median_ms'] / 1000):>12}"
                      f" {seconds(data['optimized_median_ms'] / 1000):>12} {str(reduction) + '%':>10}")
    terminal.line("  Medições destes cenários sintéticos; tempos variam conforme o DWG e a máquina.")
    terminal.line(f"  Relatório: {path}")
    terminal.rule()
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
