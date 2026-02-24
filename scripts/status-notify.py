#!/usr/bin/env python3
"""Send a session status notification via WebSocket.

Minimal RFC 6455 WebSocket client using only the Python standard library.
Fire-and-forget: all errors are silently ignored.

Reads port from $CLAUDE_CODE_SSE_PORT and hook data from stdin.
"""

import base64
import json
import os
import socket
import struct
import sys


def websocket_key():
    """Generate a random Sec-WebSocket-Key."""
    return base64.b64encode(os.urandom(16)).decode("ascii")


def build_upgrade_request(host, port, key):
    """Build the HTTP upgrade request for WebSocket."""
    return (
        f"GET / HTTP/1.1\r\n"
        f"Host: {host}:{port}\r\n"
        f"Upgrade: websocket\r\n"
        f"Connection: Upgrade\r\n"
        f"Sec-WebSocket-Key: {key}\r\n"
        f"Sec-WebSocket-Version: 13\r\n"
        f"\r\n"
    ).encode("ascii")


def read_http_response(sock):
    """Read the HTTP 101 upgrade response, consuming all headers."""
    buf = b""
    while b"\r\n\r\n" not in buf:
        chunk = sock.recv(4096)
        if not chunk:
            raise ConnectionError("Connection closed during handshake")
        buf += chunk
    status_line = buf.split(b"\r\n", 1)[0]
    if b"101" not in status_line:
        raise ConnectionError(f"Unexpected response: {status_line!r}")


def make_frame(opcode, payload):
    """Build a masked WebSocket frame (client must mask all frames)."""
    mask_key = os.urandom(4)
    masked = bytes(b ^ mask_key[i % 4] for i, b in enumerate(payload))

    header = bytes([0x80 | opcode])  # FIN + opcode

    length = len(payload)
    if length < 126:
        header += bytes([0x80 | length])  # MASK bit set
    elif length < 65536:
        header += bytes([0x80 | 126]) + struct.pack("!H", length)
    else:
        header += bytes([0x80 | 127]) + struct.pack("!Q", length)

    return header + mask_key + masked


def send_notification(port, payload):
    """Open WebSocket, send text payload, close gracefully."""
    host = "127.0.0.1"
    key = websocket_key()

    sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    sock.settimeout(3)
    try:
        sock.connect((host, port))
        sock.sendall(build_upgrade_request(host, port, key))
        read_http_response(sock)

        # Send text frame (opcode 0x1)
        sock.sendall(make_frame(0x1, payload.encode("utf-8")))

        # Send close frame (opcode 0x8)
        sock.sendall(make_frame(0x8, b""))

        # Try to read close response, but don't block long
        try:
            sock.recv(256)
        except (socket.timeout, OSError):
            pass
    finally:
        sock.close()


def main():
    port_str = os.environ.get("CLAUDE_CODE_SSE_PORT", "")
    if not port_str:
        return

    port = int(port_str)
    if port <= 0:
        return

    # Read hook data from stdin
    try:
        hook_data = json.load(sys.stdin)
    except (json.JSONDecodeError, ValueError):
        return

    event = hook_data.get("hook_event_name", "")
    if not event:
        return

    # Map event to status
    if event in ("Stop", "PreToolUse"):
        status = "idle"
    else:
        status = "working"

    # Map event to message
    message = None
    if event == "Stop":
        message = hook_data.get("last_assistant_message")
    elif event == "PreToolUse":
        message = hook_data.get("tool_name")
    elif event == "PostToolUse":
        message = hook_data.get("tool_name")

    # Build JSON-RPC notification
    params = {"status": status, "event": event}
    if message is not None:
        params["message"] = message

    notification = {
        "jsonrpc": "2.0",
        "method": "session/statusChanged",
        "params": params,
    }

    send_notification(port, json.dumps(notification))


if __name__ == "__main__":
    try:
        main()
    except Exception:
        # Fire-and-forget: silently ignore all errors
        pass
