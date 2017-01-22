#!/bin/bash

# [ ! -f "HOOK" ] && echo "HOOK not found" && exit 1

PING_URL=${1:?"PING_URL is not set"}

AUTH=${AUTH:?"AUTH is not set"}

# ###############################################

SUFFIX="test"

http $AUTH \
  POST \
  "$PING_URL/$SUFFIX"
