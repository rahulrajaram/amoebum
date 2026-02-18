#!/usr/bin/env python3
"""Adapter proxy: converts yarli's string command to overwatch's list format."""
import json
import shlex
import sys
from http.server import HTTPServer, BaseHTTPRequestHandler
import urllib.request
import urllib.error

UPSTREAM = "http://127.0.0.1:8765"

class AdapterProxy(BaseHTTPRequestHandler):
    def do_POST(self):
        length = int(self.headers.get("Content-Length", 0))
        body = self.rfile.read(length) if length else b""

        if self.path == "/run":
            try:
                payload = json.loads(body)
                if isinstance(payload.get("command"), str):
                    # yarli sends command as shell string; overwatch wants a list
                    cmd_str = payload["command"]
                    try:
                        payload["command"] = shlex.split(cmd_str)
                    except ValueError:
                        # fallback: wrap in sh -c
                        payload["command"] = ["sh", "-c", cmd_str]
                body = json.dumps(payload).encode()
                print(f"Adapted command: {payload['command'][:3]}...", file=sys.stderr, flush=True)
            except (json.JSONDecodeError, KeyError) as exc:
                print(f"Adapter parse error: {exc}", file=sys.stderr, flush=True)

        self._proxy("POST", body)

    def do_GET(self):
        self._proxy("GET", None)

    def _proxy(self, method, body):
        headers = {"Content-Type": self.headers.get("Content-Type", "application/json")}
        req = urllib.request.Request(
            f"{UPSTREAM}{self.path}",
            data=body,
            headers=headers,
            method=method,
        )
        try:
            with urllib.request.urlopen(req) as resp:
                data = resp.read()
                self.send_response(resp.status)
                self.send_header("Content-Type", "application/json")
                self.end_headers()
                self.wfile.write(data)
        except urllib.error.HTTPError as e:
            data = e.read()
            print(f"Upstream error {e.code}: {data.decode()}", file=sys.stderr, flush=True)
            self.send_response(e.code)
            self.send_header("Content-Type", "application/json")
            self.end_headers()
            self.wfile.write(data)

    def log_message(self, format, *args):
        pass

if __name__ == "__main__":
    port = int(sys.argv[1]) if len(sys.argv) > 1 else 8089
    server = HTTPServer(("127.0.0.1", port), AdapterProxy)
    print(f"Adapter proxy on :{port} -> :8765", file=sys.stderr, flush=True)
    server.serve_forever()
