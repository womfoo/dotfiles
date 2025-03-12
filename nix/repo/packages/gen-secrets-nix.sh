SYSTEM=$(nix eval --impure --expr builtins.currentSystem)
SYSTEM="${SYSTEM%\"}" # remove trailing quote
SYSTEM="${SYSTEM#\"}" # remove leading quote

nix eval "${PRJ_ROOT}#${SYSTEM}.homelab.secrets.secrets-nix" | nixfmt | tee "${PRJ_ROOT}/secrets.nix"
# nix build "file://${PRJ_ROOT}/\#${SYSTEM}.homelab.secrets.secrets-nix"
