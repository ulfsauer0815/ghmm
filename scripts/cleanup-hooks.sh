#!/bin/bash

WEBHOOK_URL_MATCH=${WEBHOOK_URL_MATCH:-"ngrok.io|pagekite.me|localtunnel.me|beameio.net"}

AUTH=${AUTH:?"AUTH is not set"}

# ###############################################

for repo in $(cat "$1");do

  OUTPUT=$(http $AUTH \
    GET \
    "https://api.github.com/repos/$repo/hooks")

  IDS=$(echo "$OUTPUT" | jq -r ".[] | select(.config.url | match(\"$WEBHOOK_URL_MATCH\")) | .id")

  echo "$OUTPUT" | jq
  echo "$IDS"
  for id in $IDS;do
    echo ""
    http $AUTH \
      DELETE \
      "https://api.github.com/repos/$repo/hooks/$id"
  done
done
