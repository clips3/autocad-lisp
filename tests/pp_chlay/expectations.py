"""Apply explicitly reviewed fixes without overwriting the original golden."""
from copy import deepcopy
import json
from pathlib import Path


def expected_baseline(baseline: dict, path: Path) -> tuple[dict, list]:
    manifest = json.loads(path.read_text(encoding="utf-8"))
    if manifest["baseline_source_sha256"] != baseline["source_sha256"]:
        raise ValueError("As correções esperadas pertencem a outra referência de produção.")
    result = deepcopy(baseline)
    reviewed = []
    seen = set()
    for change in manifest["changes"]:
        key = change["key"]
        if key in seen or baseline["records"].get(key) != change["before"]:
            raise ValueError(f"A referência original mudou no cenário da correção: {key}")
        seen.add(key)
        result["records"][key] = change["after"]
        reviewed.append({"key": key, "reason": change["reason"]})
    return result, reviewed
