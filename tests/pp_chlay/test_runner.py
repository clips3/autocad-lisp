"""Fast checks of the failure detection; no AutoCAD installation required."""
import io
import json
import os
import tempfile
from pathlib import Path
import unittest
from unittest.mock import patch

import run
from expectations import expected_baseline
from run import compare, parse_lisp, read_output
from terminal import Terminal


class DetectionTests(unittest.TestCase):
    def test_lisp_structure_validation_ignores_strings_and_comments(self):
        run.validate_lisp_forms(b'(princ "(escaped\\\"quote)") ; )\n(princ)', "test.lsp")
        for data in (b'(princ', b'(princ))', b'(princ "unterminated)'):
            with self.assertRaises(ValueError):
                run.validate_lisp_forms(data, "test.lsp")

    def test_reads_native_strings_numbers_and_com_booleans(self):
        self.assertEqual(parse_lisp(r'("x" (nil T :vlax-true :vlax-false 1.2e-03 "A\"B"))'),
                         ["x", [None, True, True, False, 0.0012, 'A"B']])

    def test_rejects_truncated_records(self):
        with self.assertRaises(ValueError):
            parse_lisp('( "x" (1 2)')

    def test_partial_or_fatal_output_cannot_pass(self):
        with tempfile.TemporaryDirectory() as folder:
            output = Path(folder) / "result.sexp"
            for content in ('("environment" ("26.0" 1))\n',
                            '("fatal" "creation failed")\n("complete" T)\n'):
                output.write_text(content, encoding="utf-8")
                with self.assertRaises(RuntimeError):
                    read_output(output)

    def test_property_change_and_deleted_object_are_detected(self):
        baseline = {"clean/a": [[[62, [256]]]], "clean/b": [[[8, ["PP_LAYOUT"]]]]}
        candidate = {"clean/a": [[[62, [1]]]]}
        differences = compare(baseline, candidate)
        self.assertEqual(len(differences), 2)
        self.assertTrue(any(d["path"] == "/clean/b" for d in differences))

    def test_duplicate_copies_are_not_discarded(self):
        with tempfile.TemporaryDirectory() as folder:
            output = Path(folder) / "result.sexp"
            output.write_text('( "environment" ("26.0" 1))\n("hatch/a" (1))\n'
                              '("hatch/a" (1))\n("complete" T)\n', encoding="utf-8")
            result = read_output(output)
            self.assertEqual(result["records"]["hatch/a"], [[1], [1]])
            self.assertTrue(compare({"hatch/a": [[1]]}, result["records"]))

    def test_float_noise_is_tolerated_but_geometry_change_is_not(self):
        self.assertFalse(compare([0.02], [0.02000000001]))
        self.assertTrue(compare([0.02], [0.03]))
        self.assertTrue(compare(True, 1))


class PresentationTests(unittest.TestCase):
    def summary(self, differences, color="never"):
        stream = io.StringIO()
        baseline = {"records": {"match/0": [True], "clean/a": ["PP_LAYOUT"]}}
        result = {**baseline, "timings_ms": {"timing/clean-ms": 1797}, "elapsed_seconds": 20.44}
        Terminal(color, stream).summary(baseline, result, differences, Path("comparison.json"))
        return stream.getvalue()

    def test_approval_has_clear_status_counts_and_readable_duration(self):
        output = self.summary([])
        self.assertIn("[APROVADO] Todos os grupos passaram.", output)
        self.assertIn("Registros verificados: 2", output)
        self.assertIn("Diferenças: 0", output)
        self.assertIn("Erros de execução: 0", output)
        self.assertIn("CHLAUTO: 1,80 s", output)
        self.assertIn("total: 20,44 s", output)
        self.assertNotIn("\033[", output)

    def test_changes_are_red_and_never_reported_as_approved(self):
        changes = [{"path": "/clean/a/layer", "expected": "PP_LAYOUT", "actual": "PP_PILAR"}]
        output = self.summary(changes, "always")
        self.assertIn("\033[1;31m", output)
        self.assertIn("[FALHOU]", output)
        self.assertIn("DIFERENTE", output)
        self.assertIn("Esperado", output)
        self.assertIn("Obtido", output)
        self.assertNotIn("[APROVADO]", output)

    def test_incomplete_execution_does_not_claim_zero_errors_or_approval(self):
        stream = io.StringIO()
        Terminal("never", stream).error("O AutoCAD não concluiu os cenários.")
        output = stream.getvalue()
        self.assertIn("[ERRO]", output)
        self.assertIn("não foi concluída", output)
        self.assertNotIn("Erros de execução: 0", output)
        self.assertNotIn("[APROVADO]", output)

    def test_no_color_environment_and_redirected_output_disable_auto_colors(self):
        stream = io.StringIO()
        with patch.dict(os.environ, {"NO_COLOR": "1"}):
            Terminal("auto", stream).heading()
        self.assertNotIn("\033[", stream.getvalue())
        self.assertFalse(Terminal("auto", io.StringIO()).color)

    def test_approval_color_can_be_forced_or_disabled(self):
        self.assertIn("\033[1;32m", self.summary([], "always"))
        with patch.dict(os.environ, {"NO_COLOR": "1"}):
            self.assertIn("\033[1;32m", self.summary([], "always"))


class CommandStatusTests(unittest.TestCase):
    def invoke(self, actual_records, execution_error=None, contract_records=None):
        with tempfile.TemporaryDirectory() as folder:
            folder = Path(folder)
            baseline_path = folder / "baseline.json"
            baseline = {
                "records": {"clean/a": ["PP_LAYOUT"]}, "environment": [["26.0", 1]],
                "source_sha256": "reference",
                "cases_sha256": run.sha((run.HERE / "cases.json").read_bytes()),
                "harness_sha256": run.sha((run.HERE / "harness.lsp").read_bytes()),
            }
            baseline_path.write_text(json.dumps(baseline), encoding="utf-8")
            expected_path = folder / "expected_changes.json"
            expected_path.write_text(json.dumps({"baseline_source_sha256": "reference", "changes": []}), encoding="utf-8")
            contract_names = json.loads((run.HERE / "contracts.json").read_text(encoding="utf-8"))
            if contract_records is None:
                contract_records = {"contracts/" + name: [True] for name in contract_names}
            result = {"records": actual_records, "environment": baseline["environment"],
                      "source_sha256": "candidate", "timings_ms": {}, "elapsed_seconds": 1.0,
                      "contract_records": contract_records}
            stdout, stderr = io.StringIO(), io.StringIO()
            args = ["run.py", "check", "--baseline", str(baseline_path),
                    "--expected-changes", str(expected_path), "--color", "never"]
            with patch("sys.argv", args), patch("sys.stdout", stdout), patch("sys.stderr", stderr), \
                    patch("run.execute", return_value=(result, folder), side_effect=execution_error):
                code = run.main()
            report_path = folder / "comparison.json"
            report = json.loads(report_path.read_text(encoding="utf-8")) if report_path.exists() else None
            return code, stdout.getvalue() + stderr.getvalue(), report

    def test_exit_zero_and_json_agree_with_approval(self):
        code, output, report = self.invoke({"clean/a": ["PP_LAYOUT"]})
        self.assertEqual(code, 0)
        self.assertIn("[APROVADO]", output)
        self.assertTrue(report["passed"])

    def test_exit_one_and_json_agree_with_regression(self):
        code, output, report = self.invoke({"clean/a": ["PP_PILAR"]})
        self.assertEqual(code, 1)
        self.assertIn("[FALHOU]", output)
        self.assertNotIn("[APROVADO]", output)
        self.assertFalse(report["passed"])

    def test_exit_two_does_not_write_a_success_report(self):
        code, output, report = self.invoke({}, RuntimeError("AutoCAD indisponível"))
        self.assertEqual(code, 2)
        self.assertIn("[ERRO]", output)
        self.assertNotIn("[APROVADO]", output)
        self.assertIsNone(report)

    def test_missing_contract_output_is_a_failure(self):
        code, output, report = self.invoke({"clean/a": ["PP_LAYOUT"]}, contract_records={})
        self.assertEqual(code, 1)
        self.assertFalse(report["passed"])
        self.assertTrue(any(d["path"].startswith("/contracts/") for d in report["differences"]))


class ExpectedFixTests(unittest.TestCase):
    def test_only_the_explicit_fix_is_accepted(self):
        original = {"source_sha256": "original", "records": {"a": [1], "b": [2]}}
        manifest = {"baseline_source_sha256": "original", "changes": [
            {"key": "a", "before": [1], "after": [3], "reason": "intentional fix"}]}
        with tempfile.TemporaryDirectory() as folder:
            path = Path(folder) / "fixes.json"
            path.write_text(json.dumps(manifest), encoding="utf-8")
            expected, reviewed = expected_baseline(original, path)
        self.assertEqual(original["records"]["a"], [1])
        self.assertEqual(len(reviewed), 1)
        self.assertFalse(compare(expected["records"], {"a": [3], "b": [2]}))
        self.assertTrue(compare(expected["records"], {"a": [3], "b": [99]}))

    def test_stale_reference_or_duplicate_fix_cannot_be_accepted(self):
        original = {"source_sha256": "original", "records": {"a": [1]}}
        for manifest in (
            {"baseline_source_sha256": "different", "changes": []},
            {"baseline_source_sha256": "original", "changes": [
                {"key": "a", "before": [99], "after": [3], "reason": "bad"}]},
            {"baseline_source_sha256": "original", "changes": [
                {"key": "a", "before": [1], "after": [3], "reason": "duplicate"}] * 2},
        ):
            with tempfile.TemporaryDirectory() as folder:
                path = Path(folder) / "fixes.json"
                path.write_text(json.dumps(manifest), encoding="utf-8")
                with self.assertRaises(ValueError):
                    expected_baseline(original, path)


if __name__ == "__main__":
    unittest.main()
