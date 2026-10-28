"""Readable terminal output, with optional ANSI colors and no dependencies."""
from __future__ import annotations

from collections import Counter
import json
import os
from pathlib import Path
import shutil
import sys


def configure_output():
    # Windows pipes may default to a legacy code page. Keep accents readable
    # both in the console and in UTF-8 logs captured by automation.
    for stream in (sys.stdout, sys.stderr):
        if hasattr(stream, "reconfigure"):
            stream.reconfigure(encoding="utf-8", errors="replace")


def number(value: int) -> str:
    return f"{value:,}".replace(",", ".")


def seconds(value: float) -> str:
    return f"{value:.2f}".replace(".", ",") + " s"


def enable_windows_colors(stream) -> bool:
    if os.name != "nt":
        return True
    try:
        import ctypes
        from ctypes import wintypes
        import msvcrt

        kernel = ctypes.WinDLL("kernel32", use_last_error=True)
        kernel.GetConsoleMode.argtypes = (wintypes.HANDLE, ctypes.POINTER(wintypes.DWORD))
        kernel.SetConsoleMode.argtypes = (wintypes.HANDLE, wintypes.DWORD)
        handle = msvcrt.get_osfhandle(stream.fileno())
        mode = wintypes.DWORD()
        return bool(kernel.GetConsoleMode(handle, ctypes.byref(mode))
                    and kernel.SetConsoleMode(handle, mode.value | 0x0004))
    except (AttributeError, OSError, ValueError):
        return False


GROUPS = (
    ("names", "Comparação de nomes de camada"),
    ("helpers", "Normalização e nomes canônicos"),
    ("clean", "CHLAUTO - limpeza e remapeamento"),
    ("delete", "CHLDEL - exclusão de objetos"),
    ("manual", "CHL - aplicação manual de camadas"),
    ("layers", "Propriedades das camadas PP"),
    ("hatches", "Hachuras, pilares e seleção"),
)


def group_for(path: str) -> str:
    prefix = path.lstrip("/").split("/", 1)[0]
    if prefix == "match":
        return "names"
    if prefix in ("base", "norm", "canonical"):
        return "helpers"
    if prefix.startswith("pillar-") or prefix == "hatch-command":
        return "hatches"
    return prefix


class Terminal:
    STYLES = {"good": "1;32", "bad": "1;31", "info": "1;36", "dim": "2", "title": "1;36"}

    def __init__(self, color="auto", stream=None):
        self.stream = sys.stdout if stream is None else stream
        self.color = color == "always" or (
            color == "auto" and "NO_COLOR" not in os.environ
            and self.stream.isatty() and enable_windows_colors(self.stream))
        self.width = min(88, max(64, shutil.get_terminal_size((88, 24)).columns - 2))

    def paint(self, text, style):
        return f"\033[{self.STYLES[style]}m{text}\033[0m" if self.color else text

    def line(self, text="", style=None):
        print(self.paint(text, style) if style else text, file=self.stream, flush=True)

    def rule(self):
        self.line("-" * self.width, "dim")

    def heading(self, title="TESTES DE REGRESSÃO"):
        self.line()
        self.rule()
        self.line(f"  PP_CHLAY | {title}", "title")
        self.rule()

    def activity(self, text):
        self.line(f"  [EM ANDAMENTO] {text}", "info")

    def error(self, error):
        self.rule()
        self.line("  [ERRO] A verificação não foi concluída.", "bad")
        self.line("  Não foi possível confirmar a aprovação dos testes.")
        self.line()
        self.line(f"  Motivo: {error}")
        self.line("  Código de saída: 2", "dim")
        self.rule()

    def summary(self, baseline, result, differences, report_path):
        changed_groups = {group_for(change["path"]) for change in differences}
        counts = Counter(group_for(key) for key in baseline["records"].keys() | result["records"].keys())
        self.rule()
        if differences:
            self.line("  [FALHOU] Foram encontradas mudanças de comportamento.", "bad")
            self.line("  A execução terminou; as diferenças precisam de revisão.")
        else:
            self.line("  [APROVADO] Todos os grupos passaram.", "good")
            self.line("  Nenhuma regressão detectada nos cenários testados.")
        self.line()
        self.line(f"  Registros verificados: {number(sum(counts.values()))}"
                  f"   |   Diferenças: {number(len(differences))}   |   Erros de execução: 0")
        self.line()
        self.line(f"  {'GRUPO':<38} {'REGISTROS':>9}  RESULTADO", "dim")
        for key, label in GROUPS:
            if counts[key]:
                status = "DIFERENTE" if key in changed_groups else "OK"
                self.line(f"  {label:<38} {number(counts[key]):>9}  "
                          + self.paint(status, "bad" if key in changed_groups else "good"))
        # Keep newly added groups visible even before a friendly label is added.
        known = {key for key, _ in GROUPS}
        for key in sorted(counts.keys() - known):
            self.line(f"  {key:<38} {number(counts[key]):>9}  "
                      + self.paint("DIFERENTE" if key in changed_groups else "OK",
                                   "bad" if key in changed_groups else "good"))
        self.line()
        timings = result["timings_ms"]
        parts = [f"{label}: {seconds(timings[key] / 1000)}" for key, label in (
            ("timing/clean-ms", "CHLAUTO"), ("timing/delete-ms", "CHLDEL")) if key in timings]
        parts.append(f"total: {seconds(result['elapsed_seconds'])}")
        self.line("  Tempos | " + "  |  ".join(parts))
        self.line("  O total inclui iniciar e encerrar a instância de teste do AutoCAD.", "dim")
        if differences:
            self.line()
            self.line("  AMOSTRA DAS DIFERENÇAS", "bad")
            for change in differences[:4]:
                self.line(f"  Local:    {change['path']}")
                for field, label in (("expected", "Esperado"), ("actual", "Obtido")):
                    value = json.dumps(change[field], ensure_ascii=False)
                    if len(value) > self.width - 14:
                        value = value[:self.width - 17] + "..."
                    self.line(f"    {label:<9} {value}")
            if len(differences) > 4:
                self.line(f"  Mais {number(len(differences) - 4)} diferença(s) no relatório completo.")
        self.line()
        self.line("  Relatório detalhado:", "dim")
        self.line(f"  {Path(report_path)}")
        self.line()
        self.line("  A aprovação compara com a referência, que inclui problemas já existentes.", "dim")
        self.rule()

    def recorded(self, result, reference, path):
        self.rule()
        self.line("  [REFERÊNCIA GRAVADA] Captura concluída sem erro.", "good")
        self.line(f"  Commit: {reference}   |   Registros: {number(len(result['records']))}")
        self.line("  Esta captura será usada pelas próximas comparações.")
        self.line(f"  Arquivo: {path}")
        self.rule()
