"""A test-only HTTPS Git server: git http-backend behind Basic auth.

Usage: python3 git_https_server.py PORT PROJECT_ROOT CERT KEY USER:PASSWORD LOG

Every request is appended to LOG (method, path, whether it was authorised),
so tests can assert how many network requests Git made.
"""

import base64
import os
import ssl
import subprocess
import sys
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer

PORT, ROOT, CERT, KEY, CREDENTIALS, LOG = sys.argv[1:7]
EXPECTED = "Basic " + base64.b64encode(CREDENTIALS.encode()).decode()


class Handler(BaseHTTPRequestHandler):
    def log_message(self, *args):
        pass

    def _serve(self):
        authorised = self.headers.get("Authorization") == EXPECTED
        with open(LOG, "a") as log:
            log.write(f"{self.command} {self.path} {'auth' if authorised else 'anon'}\n")
        if not authorised:
            self.send_response(401)
            self.send_header("WWW-Authenticate", 'Basic realm="test"')
            self.send_header("Content-Length", "0")
            self.end_headers()
            return
        path, _, query = self.path.partition("?")
        length = int(self.headers.get("Content-Length") or 0)
        body = self.rfile.read(length) if length else b""
        env = {
            "PATH": os.environ.get("PATH", ""),
            "GIT_PROJECT_ROOT": ROOT,
            "GIT_HTTP_EXPORT_ALL": "1",
            "PATH_INFO": path,
            "QUERY_STRING": query,
            "REQUEST_METHOD": self.command,
            "CONTENT_TYPE": self.headers.get("Content-Type", ""),
            "CONTENT_LENGTH": str(len(body)),
            "REMOTE_USER": "tester",
            "REMOTE_ADDR": "127.0.0.1",
        }
        if self.headers.get("Content-Encoding"):
            env["HTTP_CONTENT_ENCODING"] = self.headers["Content-Encoding"]
        proc = subprocess.run(["git", "http-backend"], input=body, env=env, capture_output=True)
        head, _, payload = proc.stdout.partition(b"\r\n\r\n")
        status = 200
        headers = []
        for line in head.decode("latin-1").split("\r\n"):
            if not line:
                continue
            name, _, value = line.partition(":")
            if name.lower() == "status":
                status = int(value.strip().split()[0])
            else:
                headers.append((name, value.strip()))
        self.send_response(status)
        for name, value in headers:
            self.send_header(name, value)
        self.send_header("Content-Length", str(len(payload)))
        self.end_headers()
        self.wfile.write(payload)

    do_GET = _serve
    do_POST = _serve


server = ThreadingHTTPServer(("127.0.0.1", int(PORT)), Handler)
context = ssl.SSLContext(ssl.PROTOCOL_TLS_SERVER)
context.load_cert_chain(CERT, KEY)
server.socket = context.wrap_socket(server.socket, server_side=True)
print("READY", flush=True)
server.serve_forever()
