#! /bin/bash

CURL="curl --location --silent --user 00000000-0000-0000-0000-000000000000:secret"

echo "Health check..."
echo ""
echo "checking CAPI..."
$CURL http://capi:8080/health/ | jq > capi-health.json
echo "checking IAPI..."
$CURL http://iapi:8080/health/ | jq > iapi-health.json
echo ""

CAPI_STATUS=$(jq < capi-health.json -r '.status')
IAPI_STATUS=$(jq < iapi-health.json -r '.status') 
if [ "$CAPI_STATUS" != "ok" ]; then
	echo "NOT healthy CAPI on service name health check!"
fi
if [ "$IAPI_STATUS" != "ok" ]; then
	echo "NOT healthy IAPI on service name health check!"
fi

echo "CAPI : ${CAPI_STATUS}"
echo "IAPI : ${IAPI_STATUS}"
