#!/bin/bash

ssh -o ExitOnForwardFailure=yes -f -N -D 9999 dell0

curl -v -x socks5h://localhost:9999 --upload-file "{$(find public/ -type f | paste -s -d ',')}" -u "$CREDS" "$ADDR"
