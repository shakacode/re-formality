#!/bin/bash

set -euo pipefail

export $(cat scripts/aws.env | xargs)

ARCH="aarch64"
PLATFORM="linux"

BIN="$LIB-$PLATFORM-$ARCH.exe"
REMOTE_BIN="~/re-formality/$RELEASE_DIR/$BIN"
RELEASE_BIN="$RELEASE_BIN_DIR/$BIN"

echo ""
echo "=== Preparing $PLATFORM $ARCH binary"

echo "Creating EC2 instance"
INSTANCE_ID=$(
  aws ec2 run-instances \
    --image-id ami-0f69dd1d0d03ad669 \
    --count 1 \
    --instance-type m6g.medium \
    --key-name re-formality \
    --user-data file://scripts/user-data.linux-arm64.sh \
    --tag-specifications="ResourceType=instance,Tags=[{Key=re-formality,Value=''}]" \
    | jq -r ".Instances[0].InstanceId"
)
echo "EC2 instance $INSTANCE_ID created"

echo "Getting public IP"
while :
do
  INSTANCE_IP=$(
    aws ec2 describe-instances --instance-ids $INSTANCE_ID | jq -r ".Reservations[0].Instances[0].PublicIpAddress"
  )
  if [ -n "$INSTANCE_IP" ];
  then
    echo "Instance is available at $INSTANCE_IP"
    break;
  else
    sleep 5;
  fi
done

echo "Waiting for the build to complete"

sleep 300

USER="ubuntu"

while :
do
  if ssh -o StrictHostKeyChecking=no -q -i scripts/aws.pem $USER@$INSTANCE_IP test -f "$REMOTE_BIN";
  then
    echo "Binary is ready. Downloading."
    break
  else
    sleep 5
  fi
done

scp -i scripts/aws.pem $USER@$INSTANCE_IP:$REMOTE_BIN $RELEASE_BIN
echo "Downloaded."

chmod $CHMOD $RELEASE_BIN

echo "Terminating instance."
aws ec2 terminate-instances --instance-ids $INSTANCE_ID
echo "Instance terminated."
