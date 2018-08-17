#!/usr/bin/env bash
set -e

function abspath {
  cd "$1"
  pwd
}

# Automatically install this script as a pre-commit hook
if [[ -d .git/ && ! -f .git/hooks/pre-commit ]]; then
  mkdir -p .git/hooks/
  cat <<EOF > .git/hooks/pre-commit
#!/usr/bin/env bash
set -e
$(abspath "$(dirname "$0")")/lint.sh
EOF
  chmod +x .git/hooks/pre-commit
fi

stack test :hlint
if [[ "$(stack exec scan -- -v)" = "" ]]; then
  stack install scan
fi
scan=(stack exec scan --)

# Haskell style scanner doesn't provide proper exit code ---
# it always exists with zero even if it found errors.
scanout="$(mktemp)"
(find src test -name '*.hs' -and -not -exec grep -q TemplateHaskell {} \; \
    -print0 | \
        xargs -0 "${scan[@]}" -t -j false -c false | \
        grep -v 'back slash at line end .may disturb cpp.' || true) > "$scanout"
cat "$scanout"
if [[ "$(cat "$scanout")" != "" ]]; then
  exit 1
fi

if command -v shellcheck > /dev/null; then
  shellcheck ./*.sh
else
  echo "Seems shellcheck is not installed; skipped linting shell scripts..."
  echo "Recommend to install shellcheck:"
  if command -v apt-get > /dev/null; then
    echo "  apt-get install shellcheck"
  elif command -v pacman > /dev/null; then
    echo "  pacman -S shellcheck"
  elif command -v brew > /dev/null; then
    echo "  brew install shellcheck"
  else
    echo "  https://github.com/koalaman/shellcheck"
  fi
fi

if command -v hadolint > /dev/null; then
  hadolint Dockerfile
else
  echo "Seems hadolint is not installed; skipped linting Dockerfile..."
  echo "Recommend to install hadolint:"
  if command -v pacman > /dev/null; then
    echo "  pacman -S hadolint-git"
  elif command -v brew > /dev/null; then
    echo "  brew install hadolint"
  else
    echo "  https://github.com/hadolint/hadolint"
  fi
fi
