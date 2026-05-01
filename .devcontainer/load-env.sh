#!/usr/bin/env bash
set -euo pipefail

env_file="${HOME}/.env"
exports_file="${HOME}/.devcontainer-env"
source_line='[ -f "${HOME}/.devcontainer-env" ] && source "${HOME}/.devcontainer-env"'

ensure_source_line() {
  local shell_rc="$1"
  touch "${shell_rc}"
  if ! grep -Fqx "${source_line}" "${shell_rc}"; then
    printf '\n%s\n' "${source_line}" >> "${shell_rc}"
  fi
}

ensure_source_line "${HOME}/.bashrc"
ensure_source_line "${HOME}/.profile"

if [[ ! -f "${env_file}" ]]; then
  exit 0
fi

tmp_file="$(mktemp)"
trap 'rm -f "${tmp_file}"' EXIT

while IFS= read -r line || [[ -n "${line}" ]]; do
  case "${line}" in
    ''|'#'*) continue ;;
  esac

  [[ "${line}" == *=* ]] || continue
  key="${line%%=*}"
  value="${line#*=}"

  key="${key#${key%%[![:space:]]*}}"
  key="${key%${key##*[![:space:]]}}"
  [[ "${key}" =~ ^[A-Za-z_][A-Za-z0-9_]*$ ]] || continue

  printf 'export %s=%q\n' "${key}" "${value}" >> "${tmp_file}"
done < "${env_file}"

mv "${tmp_file}" "${exports_file}"
chmod 600 "${exports_file}"
trap - EXIT

# shellcheck source=/dev/null
source "${exports_file}"
