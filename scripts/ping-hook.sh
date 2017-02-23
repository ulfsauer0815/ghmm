#!/bin/bash

# [ ! -f "HOOK" ] && echo "HOOK not found" && exit 1

PING_URL=${1:?"PING_URL is not set"}

AUTH=${AUTH:?"AUTH is not set"}

# ###############################################

WEBHOOK_PING_SUFFIX=${WEBHOOK_PING_SUFFIX:-"test"}

http $AUTH \
  POST \
  "$PING_URL/$WEBHOOK_PING_SUFFIX"
