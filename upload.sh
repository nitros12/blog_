#!/bin/bash

function get_unused_port() {
  for port in $(seq 4444 65000);
  do
    echo -ne "\035" | telnet 127.0.0.1 "$port" > /dev/null 2>&1;
    [ $? -eq 1 ] && echo "$port" && break;
  done
}
port="$(get_unused_port)"

echo "title; Ben Simms" > public/webindex.txt

ssh -o ExitOnForwardFailure=yes -f -N -L "$port":www.lancaster.ac.uk:445 dell0
smbclient -I localhost -p "$port" "$ADDR" -W "$WORKSPACE" -U "$CREDS" -c \
  'prompt OFF; recurse ON; cd public_html; lcd public; mput *'

pkill -P $$
