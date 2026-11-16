nix eval "${PRJ_ROOT}#lib.secrets.secrets-nix" | nixfmt | tee "${PRJ_ROOT}/secrets.nix"
