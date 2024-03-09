#!/usr/bin/env fish

set -l key key.pem
set -l cert cert.pem

openssl req -x509 -newkey rsa:4096 -keyout $key -out $cert -sha256 \
  -days 365 -nodes -subj "/CN=localhost"

# curl --cacert cert.pem https://localhost:7512/random
