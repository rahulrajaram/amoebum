#!/usr/bin/env python3
"""Debug proxy to log what yarli sends to overwatch."""
import json
import sys
from http.server import HTTPServer, BaseHTTPRequestHandler
import urllib.request

UPSTREAM = "http://127.0.0.1:8765"

class Proxy(BaseHTTPRequestHandler):
    def do_POST(self):
        length = int(self.headers.get("Content-Length", 0))
        body = self.rfile.read(length) if length else b""
        print(f"=== {self.path} ===", file=sys.stderr)
        print(f"Headers: {dict(self.headers)}", file=sys.stderr)
        print(f"Body: {body.decode('utf-8', errors='replace')}", file=sys.stderr)
        sys.stderr.flush()

        req = urllib.request.Request(
            f"{UPSTREAM}{self.path}",
            data=body,
            headers={"Content-Type": self.headers.get("Content-Type", "application/json")},
            method="POST",
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
            print(f"Upstream error {e.code}: {data.decode()}", file=sys.stderr)
            sys.stderr.flush()
            self.send_response(e.code)
            self.send_header("Content-Type", "application/json")
            self.end_headers()
            self.wfile.write(data)

    def do_GET(self):
        req = urllib.request.Request(f"{UPSTREAM}{self.path}")
        try:
            with urllib.request.urlopen(req) as resp:
                data = resp.read()
                self.send_response(resp.status)
                self.send_header("Content-Type", "application/json")
                self.end_headers()
                self.wfile.write(data)
        except urllib.error.HTTPError as e:
            self.send_response(e.code)
            self.end_headers()
            self.wfile.write(e.read())

    def log_message(self, format, *args):
        pass  # suppress default logging

if __name__ == "__main__":
    server = HTTPServer(("127.0.0.1", 8089), Proxy)
    print("Debug proxy on :8089 -> :8765", file=sys.stderr)
    server.serve_forever()
