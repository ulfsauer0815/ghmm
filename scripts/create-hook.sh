#!/bin/bash

WEBHOOK_URL=${WEBHOOK_URL:?"WEBHOOK_URL is not set"}
GITHUB_SECRET=${GITHUB_SECRET:?"GITHUB_SECRET is not set"}

AUTH=${AUTH:?"AUTH is not set"}

# ###############################################

# https://developer.github.com/webhooks/#events
# https://developer.github.com/v3/activity/events/types
BODY="{
  \"name\": \"web\",
  \"active\": true,
  \"events\": [
    \"*\"
  ],
  \"config\": {
    \"url\": \"$WEBHOOK_URL\",
    \"content_type\": \"json\",
    \"secret\": \"$GITHUB_SECRET\"
  }
}"

RETURN_CODE=0

for repo in $(cat "$1");do

  OUTPUT=$(http --check-status $AUTH \
    POST \
    "https://api.github.com/repos/$repo/hooks" <<< "$BODY")

  echo bla

  return_code=$?
  [ $return_code -gt $RETURN_CODE ] && RETURN_CODE=$return_code

  echo "$OUTPUT" | jq -r ".url"

done

exit $RETURN_CODE
