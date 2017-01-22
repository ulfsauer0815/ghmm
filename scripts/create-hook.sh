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
    \"insecure_ssl\": \"1\",
    \"secret\": \"$GITHUB_SECRET\"
  }
}"

for repo in $(cat "$1");do

  OUTPUT=$(http $AUTH \
    POST \
    "https://api.github.com/repos/$repo/hooks" <<< "$BODY")

  RETURN_CODE="$?"

  echo "$OUTPUT" | jq -r ".url"

done

exit "$RETURN_CODE"
