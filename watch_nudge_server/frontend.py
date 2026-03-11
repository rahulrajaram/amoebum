"""Package-served frontend assets for watch/nudge registration CRUD."""

from __future__ import annotations

from dataclasses import dataclass
from importlib import resources
from pathlib import PurePosixPath
from typing import Optional


@dataclass(frozen=True)
class FrontendAsset:
    """HTTP-friendly asset payload."""

    status: int
    content_type: str
    body: bytes


_ASSET_INDEX = {
    "/": ("registrations.html", "text/html; charset=utf-8"),
    "/index.html": ("registrations.html", "text/html; charset=utf-8"),
    "/app.js": ("registrations.js", "application/javascript; charset=utf-8"),
    "/assets/app.js": ("registrations.js", "application/javascript; charset=utf-8"),
}


def _normalize_request_path(request_path: str) -> str:
    text = request_path or "/"
    text = text.split("?", 1)[0].split("#", 1)[0]
    if not text.startswith("/"):
        text = "/" + text
    return str(PurePosixPath(text))


def load_frontend_asset(request_path: str) -> Optional[FrontendAsset]:
    """Return bundled HTML/JS asset for a request path when available."""

    normalized = _normalize_request_path(request_path)
    lookup = _ASSET_INDEX.get(normalized)
    if lookup is None:
        return None
    filename, content_type = lookup
    body = resources.files(__package__).joinpath("static", filename).read_bytes()
    return FrontendAsset(status=200, content_type=content_type, body=body)


def build_frontend_response(request_path: str) -> FrontendAsset:
    """Return either a bundled asset or a deterministic JSON 404 payload."""

    asset = load_frontend_asset(request_path)
    if asset is not None:
        return asset
    return FrontendAsset(
        status=404,
        content_type="application/json; charset=utf-8",
        body=b'{"error":"not_found","detail":"unknown frontend asset"}',
    )
