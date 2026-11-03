#!/usr/bin/env bash
set -euo pipefail

if [[ $# -gt 1 ]]; then
  echo 'Usage: bash scala-plugin-testkit/sync.sh [branch]' >&2
  exit 2
fi

testkit_dir=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
origin_file="$testkit_dir/ORIGIN.md"
checkout_status=$(git -C "$testkit_dir" status --porcelain --untracked-files=all --ignore-submodules=none)
if [[ -n "$checkout_status" ]]; then
  echo 'Checkout is not clean. Commit or stash changes, including untracked files, before syncing.' >&2
  printf '%s\n' "$checkout_status" >&2
  exit 1
fi
echo 'Checkout is clean.'

synced_commit=$(sed -n 's/^- \*\*Synced from commit:\*\* `\([0-9a-f]*\)`.*/\1/p' "$origin_file")
if [[ ! "$synced_commit" =~ ^[0-9a-f]{40}$ ]]; then
  echo "Expected one full 40-character commit hash in ORIGIN.md; found '$synced_commit'." >&2
  echo 'The script fetches this commit directly, so an abbreviated hash cannot be used.' >&2
  exit 1
fi
echo "Testkit directory: $testkit_dir"
echo "Previously synced commit: $synced_commit"

work_dir=$(mktemp -d "${TMPDIR:-/tmp}/scala-plugin-testkit-sync.XXXXXX")
cleanup() {
  if [[ $? -eq 0 ]]; then
    echo "Removing temporary files from $work_dir"
    rm -rf "$work_dir"
  else
    echo "Sync stopped. The local patch and backups are in $work_dir" >&2
  fi
}
trap cleanup EXIT
echo "Temporary work directory: $work_dir"

# 1. Download the selected release branch and the previously synced commit.
echo '[1/5] Downloading the Scala plugin GitHub repository...'
upstream_url=https://github.com/JetBrains/intellij-scala.git
if [[ $# -eq 1 ]]; then
  latest_branch=$1
else
  default_branch=$(git ls-remote --symref "$upstream_url" HEAD |
    awk '$1 == "ref:" && $3 == "HEAD" { sub(/^refs\/heads\//, "", $2); print $2 }')
  if [[ -z "$default_branch" ]]; then
    echo 'Could not determine the upstream default branch. Pass a branch explicitly.' >&2
    exit 1
  fi
  latest_branch=${default_branch/%.x/.release}
  echo "Default branch: $default_branch; release branch: $latest_branch"
fi
echo "Upstream branch: $latest_branch"
# Keep on-demand blob fetches quiet too; clone --quiet only covers the initial download.
git clone --quiet --depth=1 --filter=blob:none --no-checkout --branch "$latest_branch" \
  --config promisor.quiet=true \
  "$upstream_url" "$work_dir/upstream"
latest_commit=$(git -C "$work_dir/upstream" rev-parse HEAD)
echo "Upstream commit: $latest_commit"
awk -v commit="$latest_commit" -v branch="$latest_branch" '
  /^- \*\*Synced from commit:\*\*/ { $0 = "- **Synced from commit:** `" commit "` (branch `" branch "`)" }
  { print }
' "$origin_file" > "$work_dir/ORIGIN.new.md"
if [[ "$synced_commit" == "$latest_commit" ]]; then
  echo "Commit unchanged; updating ORIGIN.md with branch $latest_branch."
  cp "$work_dir/ORIGIN.new.md" "$origin_file"
  echo 'Staging the updated ORIGIN.md...'
  git -C "$testkit_dir" add -- ORIGIN.md
  echo "Already synced from $latest_commit."
  exit 0
fi
echo "Fetching the previously synced commit: $synced_commit"
git -C "$work_dir/upstream" fetch --quiet --depth=1 origin "$synced_commit"
git -C "$work_dir/upstream" ls-tree -r --name-only "$synced_commit" > "$work_dir/synced-files"
git -C "$work_dir/upstream" ls-tree -r --name-only "$latest_commit" > "$work_dir/latest-files"

# The vendored sources come from several upstream modules. Locate each by its package path.
upstream_path() {
  local upstream_file
  upstream_file=$(awk -v suffix="/$1" 'substr($0, length($0) - length(suffix) + 1) == suffix' "$2")
  if [[ -z "$upstream_file" || "$upstream_file" == *$'\n'* ]]; then
    echo "Missing or ambiguous upstream file: $1 (see $2)" >&2
    return 1
  fi
  printf '%s\n' "$upstream_file"
}

# 2. Save the difference between the synced upstream sources and our current copies.
echo '[2/5] Saving local changes and preparing upstream files...'
mkdir "$work_dir/synced" "$work_dir/current" "$work_dir/latest"
cp "$origin_file" "$work_dir/ORIGIN.md"
find "$testkit_dir/src/test/scala" -type f \( -name '*.scala' -o -name '*.java' \) -print0 > "$work_dir/local-files"
if [[ ! -s "$work_dir/local-files" ]]; then
  echo 'No vendored source files found.' >&2
  exit 1
fi
file_count=0
copied_files=(ORIGIN.md)
while IFS= read -r -d '' local_file; do
  package_path=${local_file#"$testkit_dir/src/test/scala/"}
  relative_file="src/test/scala/$package_path"
  copied_files+=("$relative_file")
  file_count=$((file_count + 1))
  echo "  Preparing $package_path"
  old_path=$(upstream_path "$package_path" "$work_dir/synced-files")
  new_path=$(upstream_path "$package_path" "$work_dir/latest-files")
  for snapshot in synced current latest; do
    mkdir -p "$work_dir/$snapshot/$(dirname "$relative_file")"
  done
  git -C "$work_dir/upstream" show "$synced_commit:$old_path" > "$work_dir/synced/$relative_file"
  cp "$local_file" "$work_dir/current/$relative_file"
  git -C "$work_dir/upstream" show "$latest_commit:$new_path" > "$work_dir/latest/$relative_file"
done < "$work_dir/local-files"
# Keep the base blobs in a temporary Git repository for three-way patch application.
cp -R "$work_dir/synced" "$work_dir/merge"
git -C "$work_dir/merge" init -q
git -C "$work_dir/merge" add .
cp -R "$work_dir/current/." "$work_dir/merge/"
git -C "$work_dir/merge" diff --binary --full-index --no-ext-diff --no-textconv > "$work_dir/local.patch"
echo "Local patch saved to $work_dir/local.patch"

# 3. Copy the new upstream versions of all vendored files.
echo "[3/5] Copying $file_count upstream files into $testkit_dir..."
cp -R "$work_dir/latest/." "$testkit_dir/"

# 4. Record the new upstream commit and branch, then stage the upstream snapshot.
echo "[4/5] Updating ORIGIN.md: $latest_commit (branch $latest_branch)"
cp "$work_dir/ORIGIN.new.md" "$origin_file"
echo 'Staging the copied upstream files and ORIGIN.md...'
git -C "$testkit_dir" add -- "${copied_files[@]}"
echo 'Review upstream changes with git diff --cached; reapplied local changes will remain unstaged.'

# 5. Reapply our local changes, leaving Git conflict markers inside the files.
echo '[5/5] Reapplying local changes with a Git three-way merge...'
if [[ -s "$work_dir/local.patch" ]]; then
  cp -R "$work_dir/latest/." "$work_dir/merge/"
  git -C "$work_dir/merge" add .
  apply_status=0
  git -C "$work_dir/merge" -c merge.conflictStyle=merge apply --3way --whitespace=nowarn \
    "$work_dir/local.patch" || apply_status=$?
  echo 'Writing merged sources back to the testkit...'
  cp -R "$work_dir/merge/src/." "$testkit_dir/src/"
  if [[ $apply_status -ne 0 ]]; then
    echo 'Resolve the conflict markers in the source files before syncing again.' >&2
    exit 1
  fi
else
  echo 'No local changes to reapply.'
fi
echo "Synced from $latest_commit (branch $latest_branch). Review the diff and run: sbt check test"
