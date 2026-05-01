#!/usr/bin/env bash
set -euo pipefail

bash "${PWD}/.devcontainer/load-env.sh"

GITEA_URL="${GITEA_URL:-https://gitea.local.vgerasimov.dev}"

if command -v tea >/dev/null 2>&1 && [[ -n "${GITEA_TOKEN:-}" ]]; then
  tea login add --name default --url "${GITEA_URL}" --token "${GITEA_TOKEN}" --non-interactive >/dev/null 2>&1 \
    || tea login edit --name default --url "${GITEA_URL}" --token "${GITEA_TOKEN}" --non-interactive >/dev/null 2>&1 \
    || printf 'Warning: failed to configure tea login for %s.\n' "${GITEA_URL}"
fi

if [[ -n "${GITHUB_TOKEN:-}" ]]; then
  sbt update
else
  printf 'Skipping sbt update because GITHUB_TOKEN is not set.\n'
fi
