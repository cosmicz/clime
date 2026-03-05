#!/usr/bin/env bash
# Instantiate the skeleton project with a new name.
#
# Usage:
#   ./init.sh new-project          # rename skeleton -> new-project
#   ./init.sh my-lib "Your Name"   # also set author
#   ./init.sh my-lib --fresh       # also reset git history
#   ./init.sh --help               # show usage

set -euo pipefail

usage() {
  echo "Usage: $0 <project-name> [author] [--fresh]"
  echo ""
  echo "  Renames 'skeleton' -> <project-name> across all files."
  echo ""
  echo "Arguments:"
  echo "  project-name   Lowercase alphanumeric with hyphens (e.g. my-project)"
  echo "  author         Optional author name for file headers"
  echo "  --fresh        Reset git history (start with a clean initial commit)"
  echo ""
  echo "Examples:"
  echo "  $0 my-project"
  echo "  $0 my-project \"Jane Doe\""
  echo "  $0 my-project \"Jane Doe\" --fresh"
}

# Parse arguments
NAME=""
AUTHOR=""
FRESH=false

for arg in "$@"; do
  case "$arg" in
    --help|-h)
      usage
      exit 0
      ;;
    --fresh)
      FRESH=true
      ;;
    *)
      if [[ -z "$NAME" ]]; then
        NAME="$arg"
      elif [[ -z "$AUTHOR" ]]; then
        AUTHOR="$arg"
      else
        echo "Error: unexpected argument '$arg'"
        usage
        exit 1
      fi
      ;;
  esac
done

if [[ -z "$NAME" ]]; then
  usage
  exit 1
fi

# Validate project name (lowercase, hyphens, alphanumeric)
if [[ ! "$NAME" =~ ^[a-z][a-z0-9-]*$ ]]; then
  echo "Error: project name must be lowercase alphanumeric with hyphens (e.g. my-project)"
  exit 1
fi

ROOT="$(cd "$(dirname "$0")" && pwd)"
cd "$ROOT"

echo "Renaming skeleton -> $NAME"

# Compute capitalized form once (e.g. my-project -> My-Project)
CAPITALIZED="$(echo "$NAME" | awk '{for(i=1;i<=NF;i++) $i=toupper(substr($i,1,1)) substr($i,2)}1' FS=- OFS='-')"

# Rename files
for f in skeleton*.el; do
  new="${f//skeleton/$NAME}"
  git mv "$f" "$new"
done

for f in tests/skeleton*.el; do
  new="${f//skeleton/$NAME}"
  git mv "$f" "$new"
done

# Platform-appropriate sed in-place
if [[ "$(uname)" == "Darwin" ]]; then
  SED_I=(sed -i '')
else
  SED_I=(sed -i)
fi

# Replace content in all tracked text files
for f in *.el tests/*.el Makefile README.org CONTRIBUTING.org; do
  [ -f "$f" ] || continue
  "${SED_I[@]}" "s/skeleton/$NAME/g" "$f"
  "${SED_I[@]}" "s/Skeleton/$CAPITALIZED/g" "$f"
done

# Fix README title (s/skeleton/ would have turned emacs-skeleton into emacs-$NAME)
"${SED_I[@]}" "s/^#+title: emacs-$NAME$/#+title: $NAME/" README.org

# Set author if provided
if [[ -n "$AUTHOR" ]]; then
  for f in *.el; do
    "${SED_I[@]}" "s/^;; Author:.*$/;; Author: $AUTHOR/" "$f"
  done
  "${SED_I[@]}" "s/^#+author:.*$/#+author: $AUTHOR/" README.org
fi

# Reset git history if requested
if [[ "$FRESH" == true ]]; then
  echo "Resetting git history..."
  rm -rf .git
  git init
  git add -A
  git commit -m "Initial commit: $NAME from emacs-skeleton"
fi

echo "Done. Project renamed to '$NAME'."
echo ""
echo "Next steps:"
echo "  1. Update README.org with your project description"
echo "  2. Run 'make test' to verify everything works"
echo "  3. Delete this script: git rm init.sh"
if [[ "$FRESH" == true ]]; then
  echo "  4. Add remote: git remote add origin <url>"
else
  echo "  4. Commit: git commit -am 'Instantiate $NAME from skeleton'"
fi
