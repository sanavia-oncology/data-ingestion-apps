#!/bin/bash
# Wipe every object, version and delete marker under flow-cytometry/ in the
# prod bucket - a clean slate to watch a laptop's sync repopulate it. Lifts the
# bucket's delete-deny for the duration and puts it back after, then proves
# both. Touches nothing outside the prefix.
#
#     bash wipe_flow_cytometry_prefix.sh           # asks before deleting
#
# The data is not recoverable afterwards. Only run it when a copy exists
# elsewhere.
set -euo pipefail
export AWS_PROFILE="${AWS_PROFILE:-antibody-explorer}"
B=sanavia-experiment-raw-data
PREFIX=flow-cytometry/
HERE="$(cd "$(dirname "$0")" && pwd)"
POLICY="$HERE/../bucket-no-delete-policy.json"
[[ -f "$POLICY" ]] || { echo "no $POLICY" >&2; exit 1; }
aws sts get-caller-identity --query Account --output text | grep -q 503972965207 || { echo "wrong account / SSO not active" >&2; exit 1; }

echo "scope: s3://$B/$PREFIX"
aws s3 ls "s3://$B/$PREFIX" --recursive --summarize | tail -2 | sed 's/^/  /'
if [[ "${1:-}" == "--yes" ]]; then ans=WIPE; else read -r -p "type WIPE to delete all of it, versions included: " ans || true; fi
[[ "$ans" == "WIPE" ]] || { echo "aborted - nothing deleted"; exit 1; }

echo "--- lifting delete-deny"
aws s3api get-bucket-policy --bucket "$B" --query Policy --output text \
  | python3 -c 'import json,sys;p=json.load(sys.stdin);p["Statement"]=[s for s in p["Statement"] if "NoObjectDeletes" not in s["Sid"]];print(json.dumps(p))' \
  > /tmp/policy-nodeny.json
aws s3api put-bucket-policy --bucket "$B" --policy file:///tmp/policy-nodeny.json
restore() { echo "--- restoring delete-deny"; aws s3api put-bucket-policy --bucket "$B" --policy "file://$POLICY"; }
trap restore EXIT    # the deny goes back even if a delete batch fails

echo "--- deleting versions and markers in batches of 1000"
aws s3api list-object-versions --bucket "$B" --prefix "$PREFIX" --output json \
  | python3 -c '
import json,sys,subprocess
d=json.load(sys.stdin); B=sys.argv[1]; P=sys.argv[2]
items=[{"Key":x["Key"],"VersionId":x["VersionId"]} for x in (d.get("Versions") or [])+(d.get("DeleteMarkers") or []) if x["Key"].startswith(P)]
print(f"  {len(items)} to delete")
for i in range(0,len(items),1000):
    batch=items[i:i+1000]
    r=subprocess.run(["aws","s3api","delete-objects","--bucket",B,"--delete",json.dumps({"Objects":batch,"Quiet":True})],capture_output=True,text=True)
    errs=json.loads(r.stdout or "{}").get("Errors",[]) if r.returncode==0 else [{"Message":r.stderr}]
    print(f"  batch {i//1000+1}: {len(batch)-len(errs)} deleted, {len(errs)} errors"); [print("   ",e) for e in errs[:3]]
' "$B" "$PREFIX"
trap - EXIT; restore

echo "--- verify"
left=$(aws s3api list-object-versions --bucket "$B" --prefix "$PREFIX" --query '[length(Versions||`[]`),length(DeleteMarkers||`[]`)]' --output text)
echo "  versions/markers left under $PREFIX: $left  (want: 0 0)"
echo "  delete-deny active: $(aws s3api delete-object --bucket "$B" --key "${PREFIX}probe-nonexistent" 2>&1 | grep -o AccessDenied || echo 'NO - check the policy')"
echo "  genscript-orders/ still there: $(aws s3 ls "s3://$B/genscript-orders/" --recursive | wc -l | tr -d ' ') objects"
