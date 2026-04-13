import sys
import unittest
from pathlib import Path


REPO_ROOT = Path(__file__).resolve().parents[2]
if str(REPO_ROOT) not in sys.path:
    sys.path.insert(0, str(REPO_ROOT))

from watch_nudge_server import build_frontend_response, load_frontend_asset


class WatchNudgeFrontendTest(unittest.TestCase):
    def test_root_html_asset_is_available(self):
        asset = load_frontend_asset("/")
        self.assertIsNotNone(asset)
        self.assertEqual(asset.status, 200)
        self.assertEqual(asset.content_type, "text/html; charset=utf-8")
        html = asset.body.decode("utf-8")
        self.assertIn("Registration Manager", html)
        self.assertIn('id="registration-form"', html)
        self.assertIn('/assets/app.js', html)

    def test_javascript_asset_supports_crud_actions(self):
        asset = load_frontend_asset("/assets/app.js")
        self.assertIsNotNone(asset)
        self.assertEqual(asset.status, 200)
        self.assertEqual(
            asset.content_type,
            "application/javascript; charset=utf-8",
        )
        js = asset.body.decode("utf-8")
        self.assertIn('"GET"', js)
        self.assertIn('"POST"', js)
        self.assertIn('"PUT"', js)
        self.assertIn('"DELETE"', js)
        self.assertIn("refreshRows()", js)

    def test_missing_asset_returns_json_404(self):
        response = build_frontend_response("/does-not-exist")
        self.assertEqual(response.status, 404)
        self.assertEqual(response.content_type, "application/json; charset=utf-8")
        self.assertIn("not_found", response.body.decode("utf-8"))


if __name__ == "__main__":
    unittest.main()
