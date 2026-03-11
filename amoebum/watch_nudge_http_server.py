#!/usr/bin/env python3
"""Local HTTP server for watch/nudge registration CRUD endpoints.

This module intentionally uses only the Python standard library.
"""

from __future__ import annotations

import argparse
import json
import logging
import re
import threading
import uuid
from datetime import datetime, timezone
from http import HTTPStatus
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer
from typing import Any, Dict, List, Optional, Tuple
from urllib.parse import urlparse

LOGGER = logging.getLogger("watch_nudge_http_server")

MAX_BODY_BYTES = 1_000_000
VALID_ID_RE = re.compile(r"^[A-Za-z0-9][A-Za-z0-9._-]{0,127}$")
COLLECTIONS = ("watches", "nudges")


class ApiError(Exception):
    """Represents a structured API error response."""

    def __init__(
        self,
        status: int,
        code: str,
        message: str,
        details: Optional[Dict[str, Any]] = None,
    ) -> None:
        super().__init__(message)
        self.status = status
        self.code = code
        self.message = message
        self.details = details or {}


class RegistrationStore:
    """Thread-safe in-memory storage for watch/nudge registrations."""

    def __init__(self) -> None:
        self._lock = threading.Lock()
        self._collections: Dict[str, Dict[str, Dict[str, Any]]] = {
            "watches": {},
            "nudges": {},
        }

    def list(self, collection: str) -> List[Dict[str, Any]]:
        with self._lock:
            items = list(self._collections[collection].values())
        return sorted(items, key=lambda item: item.get("id", ""))

    def get(self, collection: str, item_id: str) -> Dict[str, Any]:
        with self._lock:
            item = self._collections[collection].get(item_id)
        if item is None:
            raise ApiError(
                status=HTTPStatus.NOT_FOUND,
                code="not_found",
                message=f"No {collection[:-1]} registration found for id '{item_id}'.",
            )
        return item

    def create(self, collection: str, payload: Dict[str, Any]) -> Dict[str, Any]:
        item_id = payload.get("id") or str(uuid.uuid4())
        _validate_id(item_id)

        now = _utc_timestamp()
        item = dict(payload)
        item["id"] = item_id
        item.setdefault("created_at", now)
        item["updated_at"] = now

        with self._lock:
            collection_map = self._collections[collection]
            if item_id in collection_map:
                raise ApiError(
                    status=HTTPStatus.CONFLICT,
                    code="already_exists",
                    message=f"{collection[:-1].capitalize()} registration '{item_id}' already exists.",
                )
            collection_map[item_id] = item

        return item

    def update(self, collection: str, item_id: str, payload: Dict[str, Any]) -> Dict[str, Any]:
        if "id" in payload and payload["id"] != item_id:
            raise ApiError(
                status=HTTPStatus.BAD_REQUEST,
                code="id_mismatch",
                message="Payload id does not match route id.",
                details={"route_id": item_id, "payload_id": payload.get("id")},
            )

        with self._lock:
            collection_map = self._collections[collection]
            existing = collection_map.get(item_id)
            if existing is None:
                raise ApiError(
                    status=HTTPStatus.NOT_FOUND,
                    code="not_found",
                    message=f"No {collection[:-1]} registration found for id '{item_id}'.",
                )

            merged = dict(existing)
            merged.update(payload)
            merged["id"] = item_id
            merged["created_at"] = existing.get("created_at", _utc_timestamp())
            merged["updated_at"] = _utc_timestamp()
            collection_map[item_id] = merged

        return merged

    def delete(self, collection: str, item_id: str) -> Dict[str, Any]:
        with self._lock:
            collection_map = self._collections[collection]
            existing = collection_map.pop(item_id, None)

        if existing is None:
            raise ApiError(
                status=HTTPStatus.NOT_FOUND,
                code="not_found",
                message=f"No {collection[:-1]} registration found for id '{item_id}'.",
            )

        return existing


class WatchNudgeRequestHandler(BaseHTTPRequestHandler):
    """HTTP handler with JSON parsing + watch/nudge CRUD routes."""

    server: "WatchNudgeHTTPServer"

    def do_GET(self) -> None:  # noqa: N802
        self._handle_request("GET")

    def do_POST(self) -> None:  # noqa: N802
        self._handle_request("POST")

    def do_PUT(self) -> None:  # noqa: N802
        self._handle_request("PUT")

    def do_PATCH(self) -> None:  # noqa: N802
        self._handle_request("PATCH")

    def do_DELETE(self) -> None:  # noqa: N802
        self._handle_request("DELETE")

    def _handle_request(self, method: str) -> None:
        try:
            status, payload = self._dispatch(method)
            self._send_json(status, payload)
        except ApiError as err:
            self._send_json(
                err.status,
                {
                    "ok": False,
                    "error": {
                        "code": err.code,
                        "message": err.message,
                        "details": err.details,
                    },
                },
            )
        except Exception:
            LOGGER.exception("Unhandled request failure")
            self._send_json(
                HTTPStatus.INTERNAL_SERVER_ERROR,
                {
                    "ok": False,
                    "error": {
                        "code": "internal_error",
                        "message": "Internal server error.",
                        "details": {},
                    },
                },
            )

    def _dispatch(self, method: str) -> Tuple[int, Dict[str, Any]]:
        parsed = urlparse(self.path)
        path_parts = [part for part in parsed.path.split("/") if part]

        if path_parts == ["health"]:
            return HTTPStatus.OK, {"ok": True, "data": {"status": "ok"}}

        if path_parts and path_parts[0] == "api":
            path_parts = path_parts[1:]

        if not path_parts:
            raise ApiError(
                status=HTTPStatus.NOT_FOUND,
                code="route_not_found",
                message="Route not found.",
            )

        collection = path_parts[0]
        if collection not in COLLECTIONS:
            raise ApiError(
                status=HTTPStatus.NOT_FOUND,
                code="route_not_found",
                message="Route not found.",
            )

        if len(path_parts) == 1:
            return self._handle_collection_route(method, collection)
        if len(path_parts) == 2:
            return self._handle_item_route(method, collection, path_parts[1])

        raise ApiError(
            status=HTTPStatus.NOT_FOUND,
            code="route_not_found",
            message="Route not found.",
        )

    def _handle_collection_route(self, method: str, collection: str) -> Tuple[int, Dict[str, Any]]:
        if method == "GET":
            items = self.server.store.list(collection)
            return HTTPStatus.OK, {"ok": True, "data": {"items": items}}

        if method == "POST":
            payload = self._read_json_body()
            item = self.server.store.create(collection, payload)
            return HTTPStatus.CREATED, {"ok": True, "data": item}

        raise ApiError(
            status=HTTPStatus.METHOD_NOT_ALLOWED,
            code="method_not_allowed",
            message=f"Method {method} is not allowed for /{collection}.",
            details={"allowed": ["GET", "POST"]},
        )

    def _handle_item_route(
        self,
        method: str,
        collection: str,
        item_id: str,
    ) -> Tuple[int, Dict[str, Any]]:
        _validate_id(item_id)

        if method == "GET":
            item = self.server.store.get(collection, item_id)
            return HTTPStatus.OK, {"ok": True, "data": item}

        if method in ("PUT", "PATCH"):
            payload = self._read_json_body()
            item = self.server.store.update(collection, item_id, payload)
            return HTTPStatus.OK, {"ok": True, "data": item}

        if method == "DELETE":
            deleted = self.server.store.delete(collection, item_id)
            return HTTPStatus.OK, {"ok": True, "data": deleted}

        raise ApiError(
            status=HTTPStatus.METHOD_NOT_ALLOWED,
            code="method_not_allowed",
            message=f"Method {method} is not allowed for /{collection}/<id>.",
            details={"allowed": ["GET", "PUT", "PATCH", "DELETE"]},
        )

    def _read_json_body(self) -> Dict[str, Any]:
        content_length_header = self.headers.get("Content-Length")
        if content_length_header is None:
            raise ApiError(
                status=HTTPStatus.BAD_REQUEST,
                code="missing_content_length",
                message="Content-Length header is required for JSON requests.",
            )

        try:
            content_length = int(content_length_header)
        except ValueError as exc:
            raise ApiError(
                status=HTTPStatus.BAD_REQUEST,
                code="invalid_content_length",
                message="Content-Length must be an integer.",
            ) from exc

        if content_length < 0:
            raise ApiError(
                status=HTTPStatus.BAD_REQUEST,
                code="invalid_content_length",
                message="Content-Length cannot be negative.",
            )

        if content_length > MAX_BODY_BYTES:
            raise ApiError(
                status=HTTPStatus.REQUEST_ENTITY_TOO_LARGE,
                code="payload_too_large",
                message=f"Request body exceeds {MAX_BODY_BYTES} bytes.",
            )

        body_bytes = self.rfile.read(content_length)
        if not body_bytes:
            raise ApiError(
                status=HTTPStatus.BAD_REQUEST,
                code="empty_body",
                message="JSON body is required.",
            )

        try:
            body_text = body_bytes.decode("utf-8")
        except UnicodeDecodeError as exc:
            raise ApiError(
                status=HTTPStatus.BAD_REQUEST,
                code="invalid_encoding",
                message="Request body must be UTF-8 JSON.",
            ) from exc

        try:
            payload = json.loads(body_text)
        except json.JSONDecodeError as exc:
            raise ApiError(
                status=HTTPStatus.BAD_REQUEST,
                code="invalid_json",
                message="Request body is not valid JSON.",
                details={"line": exc.lineno, "column": exc.colno},
            ) from exc

        if not isinstance(payload, dict):
            raise ApiError(
                status=HTTPStatus.BAD_REQUEST,
                code="invalid_payload",
                message="JSON payload must be an object.",
            )

        return payload

    def _send_json(self, status: int, payload: Dict[str, Any]) -> None:
        body = json.dumps(payload, separators=(",", ":")).encode("utf-8")
        self.send_response(status)
        self.send_header("Content-Type", "application/json; charset=utf-8")
        self.send_header("Content-Length", str(len(body)))
        self.end_headers()
        self.wfile.write(body)

    def log_message(self, fmt: str, *args: Any) -> None:
        LOGGER.info("%s - %s", self.address_string(), fmt % args)


class WatchNudgeHTTPServer(ThreadingHTTPServer):
    """Threading HTTP server carrying registration store state."""

    def __init__(self, server_address: Tuple[str, int], store: RegistrationStore):
        super().__init__(server_address, WatchNudgeRequestHandler)
        self.store = store


def _utc_timestamp() -> str:
    return datetime.now(timezone.utc).isoformat().replace("+00:00", "Z")


def _validate_id(item_id: str) -> None:
    if not isinstance(item_id, str) or not VALID_ID_RE.fullmatch(item_id):
        raise ApiError(
            status=HTTPStatus.BAD_REQUEST,
            code="invalid_id",
            message="Resource id must match ^[A-Za-z0-9][A-Za-z0-9._-]{0,127}$.",
        )


def run_server(host: str, port: int) -> None:
    store = RegistrationStore()
    server = WatchNudgeHTTPServer((host, port), store)

    LOGGER.info("Serving watch/nudge HTTP API on http://%s:%d", host, port)
    try:
        server.serve_forever(poll_interval=0.5)
    except KeyboardInterrupt:
        LOGGER.info("Stopping server")
    finally:
        server.server_close()


def parse_args(argv: Optional[List[str]] = None) -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Run local watch/nudge registration HTTP server."
    )
    parser.add_argument("--host", default="127.0.0.1", help="Bind host (default: 127.0.0.1)")
    parser.add_argument("--port", type=int, default=8098, help="Bind port (default: 8098)")
    parser.add_argument(
        "--log-level",
        default="INFO",
        choices=["DEBUG", "INFO", "WARNING", "ERROR"],
        help="Log verbosity",
    )
    return parser.parse_args(argv)


def main(argv: Optional[List[str]] = None) -> int:
    args = parse_args(argv)
    logging.basicConfig(level=getattr(logging, args.log_level), format="%(levelname)s %(message)s")

    if args.port <= 0 or args.port > 65535:
        raise SystemExit("--port must be between 1 and 65535")

    run_server(args.host, args.port)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
