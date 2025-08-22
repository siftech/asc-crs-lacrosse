#!/bin/bash

CURL="curl --location --silent --user 00000000-0000-0000-0000-000000000000:secret"
CAPI="http://capi:8080"
echo "Running health check prior to submission attempt." 
/bin/bash ./hc.sh
echo""
echo "Testing VDS Submission..."
echo ""
echo "Submitting VDS to CAPI via service name..."
$CURL -X POST -H "Content-Type: application/json" ${CAPI}/submission/vds/ -d @vds-test-sub.json > vds-out.json
jq < vds-out.json
echo ""

VDS_UUID=$(jq < vds-out.json -r '.vd_uuid')
VDS_STATUS=$(jq < vds-out.json -r '.status')
CP_NAME=$(jq < vds-out.json -r '.cp_name' | sed 's/ //g')

while [ "$VDS_STATUS" == "pending" ]; do
	sleep 10
	echo "VDS status: "
	$CURL "${CAPI}/submission/vds/${VDS_UUID}" > vds-out.json
	jq < vds-out.json
	echo ""
	VDS_STATUS=$(jq < vds-out.json -r '.status')
done

echo "VDS Submission status: ${VDS_STATUS}"
if [ "$VDS_STATUS" == "rejected" ]; then
	echo "VDS rejected"
	exit 1
fi

if [ "$VDS_STATUS" == "accepted" ]; then 
	echo "Getting cpv_uuid..."
	$CURL "${CAPI}/submission/vds/${VDS_UUID}" > vds-out.json
	CPV_UUID=$(jq < vds-out.json -r '.cpv_uuid')
	echo "CPV_UUID: ${CPV_UUID}"
	
fi

echo "Saving vds accepted submission output as ${CP_NAME}-vds-out.json"
cp "vds-out.json" "${CP_NAME}-vds-out.json"
echo ""

echo "Submitting GP to CAPI via service name..."

#######
# TODO convert .patch to base64 string.  
# DATA=$(base64 cp.patch)
#
#
#DATA="ZGlmZiAtLWdpdCBhL21vY2tfdnAuYyBiL21vY2tfdnAuYwppbmRleCA1NmNmOGZkLi5hYmI3M2NkIDEwMDY0NAotLS0gYS9tb2NrX3ZwLmMKKysrIGIvbW9ja192cC5jCkBAIC0xMSw3ICsxMSw4IEBAIGludCBtYWluKCkKICAgICAgICAgcHJpbnRmKCJpbnB1dCBpdGVtOiIpOwogICAgICAgICBidWZmID0gJml0ZW1zW2ldWzBdOwogICAgICAgICBpKys7Ci0gICAgICAgIGZnZXRzKGJ1ZmYsIDQwLCBzdGRpbik7CisgICAgICAgIGZnZXRzKGJ1ZmYsIDksIHN0ZGluKTsKKyAgICAgICAgaWYgKGk9PTMpe2J1ZmZbMF09IDA7fQogICAgICAgICBidWZmW3N0cmNzcG4oYnVmZiwgIlxuIildID0gMDsKICAgICB9d2hpbGUoc3RybGVuKGJ1ZmYpIT0wKTsKICAgICBpLS07Cg=="
#
#
#######
echo "Converting .patch to base64 string for submission."
DATA=$(base64 -w 0 mockcp-test-patch.patch)
#echo "Base64 patch: ${DATA}"

$CURL -X POST -H "Content-Type: application/json" ${CAPI}/submission/gp/ -d "{\"cpv_uuid\": \"${CPV_UUID}\", \"data\": \"${DATA}\"}" > gp-out.json

jq < gp-out.json
echo ""

GP_UUID=$(jq < gp-out.json -r '.gp_uuid')
GP_STATUS=$(jq < gp-out.json -r '.status')

while [ "$GP_STATUS" == "pending" ]; do
	sleep 10
	echo "GP Status: ${GP_STATUS}"
	$CURL "${CAPI}/submission/vds/${GP_UUID}" > gp-out.json
	jq < gp-out.json
	echo ""
	GP_STATUS=$(jq < gp-out.json -r '.status')
done

echo "Final GP Status: ${GP_STATUS}"
echo "Saving GP output as: ${CP_NAME}-gp-out.json"
cp "gp-out.json" "${CP_NAME}-gp-out.json"

# TODO - Clean up. (remove gp-out.json, vds-out.json?) 

sleep 1










