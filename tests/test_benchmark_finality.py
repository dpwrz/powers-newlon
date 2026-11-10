import importlib.util
import pathlib
import unittest

ROOT = pathlib.Path(__file__).resolve().parents[1]
spec = importlib.util.spec_from_file_location("finality", ROOT / "scripts" / "check_benchmark_finality.py")
finality = importlib.util.module_from_spec(spec)
spec.loader.exec_module(finality)

class FinalityTests(unittest.TestCase):
    def test_final_score_required(self):
        self.assertFalse(finality.numeric_result(""))
        self.assertFalse(finality.numeric_result("NA"))
        self.assertFalse(finality.numeric_result("nan"))
        self.assertFalse(finality.numeric_result("inf"))
        self.assertTrue(finality.numeric_result("0"))
        self.assertTrue(finality.numeric_result("-7"))

    def test_timestamps(self):
        self.assertLess(finality.timestamp("2026-11-08T16:00:00Z"),
                        finality.timestamp("2026-11-08T17:00:00+00:00"))
        self.assertIsNone(finality.timestamp("invalid"))

if __name__ == "__main__":
    unittest.main()
