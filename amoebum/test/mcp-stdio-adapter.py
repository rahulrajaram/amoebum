#!/usr/bin/env python3
"""Protocol adapter: Content-Length framed JSON-RPC <-> newline-delimited JSON-RPC.

Amoebum's MCP client sends/receives Content-Length framed messages.
Some MCP servers (e.g. haake) use newline-delimited JSON.
This adapter bridges the two protocols.

Usage: python3 mcp-stdio-adapter.py <command> [args...]
"""
import sys
import subprocess
import threading


def read_content_length_message(raw_in):
    """Read one Content-Length framed message from a raw binary stream."""
    content_length = None
    while True:
        line = raw_in.readline()
        if not line:
            return None
        text = line.decode("utf-8", errors="replace").strip()
        if not text:
            if content_length is not None:
                break
            continue
        if text.lower().startswith("content-length:"):
            content_length = int(text.split(":", 1)[1].strip())
    if content_length is None:
        return None
    body = b""
    while len(body) < content_length:
        chunk = raw_in.read(content_length - len(body))
        if not chunk:
            break
        body += chunk
    return body.decode("utf-8", errors="replace")


def write_content_length_message(raw_out, payload):
    """Write one Content-Length framed message to a raw binary stream.

    Uses character count (not byte count) because amoebum's SBCL reader
    uses read-sequence on a character stream, reading N characters not N bytes.
    For pure ASCII this is identical; for multi-byte UTF-8 this prevents
    read-sequence from blocking waiting for bytes that decode to fewer chars.
    """
    encoded = payload.encode("utf-8")
    # Use character count for Content-Length since consumer reads characters
    header = f"Content-Length: {len(payload)}\r\n\r\n".encode("utf-8")
    raw_out.write(header)
    raw_out.write(encoded)
    raw_out.flush()


def forward_to_subprocess(raw_in, proc):
    """Read Content-Length framed messages from our stdin, forward as newlines to subprocess."""
    try:
        while proc.poll() is None:
            body = read_content_length_message(raw_in)
            if body is None:
                break
            line = body.strip() + "\n"
            proc.stdin.write(line.encode("utf-8"))
            proc.stdin.flush()
    except (BrokenPipeError, OSError):
        pass
    finally:
        try:
            proc.stdin.close()
        except Exception:
            pass


def forward_from_subprocess(raw_out, proc):
    """Read newline-delimited JSON from subprocess stdout, frame with Content-Length to our stdout."""
    try:
        for raw_line in proc.stdout:
            line = raw_line.decode("utf-8", errors="replace").strip()
            if not line:
                continue
            write_content_length_message(raw_out, line)
    except (BrokenPipeError, OSError):
        pass


def main():
    if len(sys.argv) < 2:
        print("Usage: mcp-stdio-adapter.py <command> [args...]", file=sys.stderr)
        sys.exit(1)

    raw_in = sys.stdin.buffer
    raw_out = sys.stdout.buffer

    proc = subprocess.Popen(
        sys.argv[1:],
        stdin=subprocess.PIPE,
        stdout=subprocess.PIPE,
        stderr=sys.stderr,
    )

    writer_thread = threading.Thread(
        target=forward_to_subprocess, args=(raw_in, proc), daemon=True
    )
    reader_thread = threading.Thread(
        target=forward_from_subprocess, args=(raw_out, proc), daemon=True
    )

    writer_thread.start()
    reader_thread.start()

    proc.wait()
    writer_thread.join(timeout=2)
    reader_thread.join(timeout=2)
    sys.exit(proc.returncode or 0)


if __name__ == "__main__":
    main()
