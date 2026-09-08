#!/usr/bin/env python3
"""Small external OTLP/HTTP JSON receiver used only by the case-study gate."""

import json
import pathlib
import sys
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer

capture = pathlib.Path(sys.argv[1])
port_file = pathlib.Path(sys.argv[2])


class Handler(BaseHTTPRequestHandler):
    def do_POST(self):
        size = int(self.headers.get("content-length", "0"))
        body = self.rfile.read(size).decode("utf-8")
        parsed = json.loads(body)
        with capture.open("a", encoding="utf-8") as out:
            out.write(json.dumps({"path": self.path, "body": parsed},
                                 separators=(",", ":")) + "\n")
            out.flush()
        self.send_response(200)
        self.send_header("content-type", "application/json")
        self.end_headers()
        self.wfile.write(b"{}")

    def log_message(self, _format, *_args):
        pass


server = ThreadingHTTPServer(("127.0.0.1", 0), Handler)
port_file.write_text(str(server.server_port), encoding="ascii")
server.serve_forever()
