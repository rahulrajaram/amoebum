import http.client
import json
import sys
import threading
import unittest
from pathlib import Path


REPO_ROOT = Path(__file__).resolve().parents[2]
if str(REPO_ROOT) not in sys.path:
    sys.path.insert(0, str(REPO_ROOT))

from amoebum import watch_nudge_http_server


class WatchNudgeHTTPServerTest(unittest.TestCase):
    def setUp(self):
        self.store = watch_nudge_http_server.RegistrationStore()
        self.server = watch_nudge_http_server.WatchNudgeHTTPServer(("127.0.0.1", 0), self.store)
        self.host, self.port = self.server.server_address
        self.thread = threading.Thread(
            target=self.server.serve_forever,
            kwargs={"poll_interval": 0.01},
            daemon=True,
        )
        self.thread.start()

    def tearDown(self):
        self.server.shutdown()
        self.server.server_close()
        self.thread.join(timeout=2)

    def _request(self, method, path, payload=None, raw_body=None, headers=None):
        request_headers = {"Accept": "application/json"}
        if headers:
            request_headers.update(headers)

        body = None
        if payload is not None:
            body = json.dumps(payload).encode("utf-8")
            request_headers.setdefault("Content-Type", "application/json")
            request_headers["Content-Length"] = str(len(body))
        elif raw_body is not None:
            body = raw_body.encode("utf-8") if isinstance(raw_body, str) else raw_body
            request_headers.setdefault("Content-Type", "application/json")
            request_headers.setdefault("Content-Length", str(len(body)))

        connection = http.client.HTTPConnection(self.host, self.port, timeout=5)
        try:
            connection.request(method, path, body=body, headers=request_headers)
            response = connection.getresponse()
            response_body = response.read().decode("utf-8")
            payload = json.loads(response_body)
            return response.status, payload
        finally:
            connection.close()

    def test_watch_crud_round_trip_and_missing_watch(self):
        status, payload = self._request("GET", "/api/watches")
        self.assertEqual(status, 200)
        self.assertEqual(payload["data"]["items"], [])

        status, payload = self._request(
            "POST",
            "/api/watches",
            payload={"id": "watch-alpha", "watch_path": "/tmp/demo-watch", "nudge_minutes": 30},
        )
        self.assertEqual(status, 201)
        self.assertEqual(payload["data"]["id"], "watch-alpha")

        status, payload = self._request("GET", "/api/watches/watch-alpha")
        self.assertEqual(status, 200)
        self.assertEqual(payload["data"]["watch_path"], "/tmp/demo-watch")

        status, payload = self._request(
            "PUT",
            "/api/watches/watch-alpha",
            payload={"watch_path": "/tmp/demo-watch-updated", "notes": "updated"},
        )
        self.assertEqual(status, 200)
        self.assertEqual(payload["data"]["watch_path"], "/tmp/demo-watch-updated")

        status, _ = self._request("DELETE", "/api/watches/watch-alpha")
        self.assertEqual(status, 200)

        status, payload = self._request("GET", "/api/watches/watch-alpha")
        self.assertEqual(status, 404)
        self.assertEqual(payload["error"]["code"], "not_found")

    def test_missing_watch_put_returns_not_found(self):
        status, payload = self._request(
            "PUT",
            "/api/watches/missing-watch",
            payload={"watch_path": "/tmp/nowhere"},
        )
        self.assertEqual(status, 404)
        self.assertEqual(payload["error"]["code"], "not_found")

    def test_malformed_json_payload_returns_invalid_json(self):
        status, payload = self._request(
            "POST",
            "/api/watches",
            raw_body='{"id":"broken",',
            headers={"Content-Type": "application/json"},
        )
        self.assertEqual(status, 400)
        self.assertEqual(payload["error"]["code"], "invalid_json")

    def test_non_object_payload_returns_invalid_payload(self):
        status, payload = self._request(
            "POST",
            "/api/watches",
            payload=["watch-alpha"],
        )
        self.assertEqual(status, 400)
        self.assertEqual(payload["error"]["code"], "invalid_payload")


if __name__ == "__main__":
    unittest.main()
