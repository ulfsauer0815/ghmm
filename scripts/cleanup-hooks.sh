#!/bin/bash

WEBHOOK_URL_MATCH=${WEBHOOK_URL_MATCH:-"ngrok.io|pagekite.me|localtunnel.me|beameio.net|ghmm.*herokuapp.com"}

AUTH=${AUTH:?"AUTH is not set"}

# ###############################################

RETURN_CODE=0

for repo in $(cat "$1");do

  OUTPUT=$(http --check-status $AUTH \
    GET \
    "https://api.github.com/repos/$repo/hooks")
  
  return_code=$?
  [ $return_code -gt $RETURN_CODE ] && RETURN_CODE=$return_code
  
  if [ $return_code -gt 0 ] ;then
    echo "cannot get hooks of $repo, skipping..."
    continue
  fi

  IDS=$(echo "$OUTPUT" | jq -r ".[] | select(.config.url) | select(.config.url | match(\"$WEBHOOK_URL_MATCH\")) | .id")

  for id in $IDS;do
    echo
    http $AUTH \
      DELETE \
      "https://api.github.com/repos/$repo/hooks/$id"
  done
done
