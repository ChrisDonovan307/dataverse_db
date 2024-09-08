# Testing out pulling metadata from ASU dataverse

export API_TOKEN=xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx
export SERVER_URL=https://dataverse.asu.edu
export PERSISTENT_ID=doi:10.48349/ASU/WW9UMD

curl -L -O -J -H "X-Dataverse-key:$API_TOKEN" $SERVER_URL/api/access/dataset/:persistentId/?persistentId=$PERSISTENT_ID
RESPONSE=$(curl $SERVER_URL/api/datasets/:persistentId/?persistentId=$PERSISTENT_ID)
echo $RESPONSE
echo $RESPONSE | grep -l "download"
echo "$RESPONSE" | wsl jq .


# Extract download counts from the response
DOWNLOADS=$(echo "$RESPONSE" | jq '.data.latestVersion.fileMetadatas[] | {label: .label, downloadCount: .downloadCount}')

# Print the formatted result
echo "$downloads"