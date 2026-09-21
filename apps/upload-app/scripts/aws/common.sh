# Shared by publish_policy.sh and provision_laptop.sh; source it.
#
# The sibling repo's order-upload has the same three scripts pointed at
# genscript-orders/. Same account, same shape, different prefix and policy:
# an order-upload key is denied on flow-cytometry/ and vice versa.
EXPECTED_ACCOUNT="503972965207"
PROFILE="${AWS_PROFILE:-antibody-explorer}"
REGION="us-east-1"
S3_PREFIX="flow-cytometry/"
CREDS_FILE="$HOME/.upload-app/aws-creds"

resolve_env() {
    case "$1" in
        dev)  BUCKET="sanavia-experiment-raw-data-dev"; POLICY_NAME="upload-app-laptop-dev"; USER_PREFIX="upload-app-laptop-dev" ;;
        prod) BUCKET="sanavia-experiment-raw-data";     POLICY_NAME="upload-app-laptop";     USER_PREFIX="upload-app-laptop" ;;
        *)    echo "unknown env: $1" >&2; exit 1 ;;
    esac
    POLICY_ARN="arn:aws:iam::${EXPECTED_ACCOUNT}:policy/${POLICY_NAME}"
}

require_sso() {
    if ! aws --profile "$PROFILE" sts get-caller-identity >/dev/null 2>&1; then
        aws sso login --profile "$PROFILE" || { echo "SSO login failed" >&2; exit 1; }
    fi
    local got
    got=$(aws --profile "$PROFILE" sts get-caller-identity --query Account --output text)
    [[ "$got" == "$EXPECTED_ACCOUNT" ]] || { echo "wrong AWS account $got, expected $EXPECTED_ACCOUNT" >&2; exit 1; }
}

# add-only under our prefix: Put + List there, no Get/Delete, nothing outside it; bucket versioning keeps overwritten copies
policy_json() {
    cat <<JSON
{
  "Version": "2012-10-17",
  "Statement": [
    {
      "Sid": "UploadFlowCytometry",
      "Effect": "Allow",
      "Action": ["s3:PutObject", "s3:AbortMultipartUpload"],
      "Resource": "arn:aws:s3:::${BUCKET}/${S3_PREFIX}*"
    },
    {
      "Sid": "ListForDeltaSync",
      "Effect": "Allow",
      "Action": "s3:ListBucket",
      "Resource": "arn:aws:s3:::${BUCKET}",
      "Condition": { "StringLike": { "s3:prefix": "${S3_PREFIX}*" } }
    }
  ]
}
JSON
}
