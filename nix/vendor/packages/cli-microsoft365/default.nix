{
  lib,
  buildNpmPackage,
  fetchFromGitHub,
  nodejs_24,
  pkg-config,
  python3,
  libsecret,
  stdenv,
}:

buildNpmPackage {
  pname = "cli-microsoft365";
  version = "12.0.0-unstable-2026-09-22";

  src = fetchFromGitHub {
    owner = "pnp";
    repo = "cli-microsoft365";
    rev = "c9bd7f3ef1e54f2d2184ad1ba1c2793753f1e35a";
    hash = "sha256-YC8rZVmNYOFvTxFgbY5xwju2n0npPfBQjROA7dgnpiQ=";
  };

  nodejs = nodejs_24;

  # npm-shrinkwrap.json lacks resolved/integrity for some packages, which Nix
  # needs to prefetch them; use the filled-in copy from ./fill-lock.mjs
  postPatch = ''
    rm npm-shrinkwrap.json
    cp ${./package-lock.json} package-lock.json
  '';

  npmDepsHash = "sha256-0RCWu4E0picVzE75M9L4fZKvUXSLfIVku+jAyhYYyhQ=";

  # keytar (optional dep of @azure/msal-node-extensions) is built from source
  # since prebuild-install can't download binaries inside the sandbox
  nativeBuildInputs = [
    pkg-config
    python3
  ];
  buildInputs = lib.optionals stdenv.hostPlatform.isLinux [ libsecret ];

  # disable update-notifier / telemetry prompts during build-time command discovery
  env.CLIMICROSOFT365_NOUPDATE = "1";

  # `m365 cli completion sh setup` writes commands.json next to dist/ at
  # runtime, which fails (EROFS) in the store; generate it here instead and
  # ship the omelette completion scripts so no setup step is needed
  postInstall = ''
    export HOME=$TMPDIR
    $out/bin/m365 cli completion sh update
    test -s $out/lib/node_modules/@pnp/cli-microsoft365/commands.json

    mkdir -p $out/share/bash-completion/completions $out/share/fish/vendor_completions.d
    $out/bin/m365_comp --completion > $out/share/bash-completion/completions/m365
    ln -s m365 $out/share/bash-completion/completions/microsoft365
    $out/bin/m365_comp --completion-fish > $out/share/fish/vendor_completions.d/m365.fish
  '';

  meta = {
    description = "Manage Microsoft 365 and SharePoint Framework projects on any platform";
    homepage = "https://pnp.github.io/cli-microsoft365";
    license = lib.licenses.mit;
    mainProgram = "m365";
    platforms = lib.platforms.unix;
  };
}
