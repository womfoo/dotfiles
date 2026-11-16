{
  lib,
  buildNpmPackage,
  fetchFromGitHub,
  nodejs_24,
  makeWrapper,
  callPackage,
}:

let
  version = "0.3.0-rc.1";
  src = fetchFromGitHub {
    owner = "wyruweso";
    repo = "openp2s";
    rev = "v${version}";
    hash = "sha256-Sq68db5QLedZwwIfApJbhKSwHNI0NPDQMM2zJaWFkFk=";
  };
  openvpn-openp2s = callPackage ./openvpn.nix { openp2s-src = src; };
in
buildNpmPackage {
  pname = "openp2s";
  inherit version src;

  nodejs = nodejs_24;
  npmDepsHash = "sha256-dOCVp1XS3IRU7dBJkyvTYX0+cKqA5Km4vrKRWJU40/U=";

  # The CLI runs straight from the TypeScript sources (node type stripping),
  # so there is nothing to build; we skip the SEA single-binary step.
  dontNpmBuild = true;

  nativeBuildInputs = [ makeWrapper ];

  installPhase = ''
    runHook preInstall
    npm prune --omit=dev --no-save
    mkdir -p $out/lib/openp2s-cli $out/bin
    cp -r package.json src node_modules $out/lib/openp2s-cli/
    makeWrapper ${lib.getExe nodejs_24} $out/bin/openp2s \
      --add-flags $out/lib/openp2s-cli/src/cli/run.ts \
      --set-default OPENP2S_OPENVPN_BINARY ${openvpn-openp2s}/lib/openp2s/openvpn
    install -Dm0644 packaging/openp2s.1 $out/share/man/man1/openp2s.1
    runHook postInstall
  '';

  passthru = { inherit openvpn-openp2s; };

  meta = {
    description = "Linux client for Azure Point-to-Site VPN with Microsoft Entra ID authentication";
    homepage = "https://github.com/wyruweso/openp2s";
    license = lib.licenses.gpl2Only;
    platforms = lib.platforms.linux;
    mainProgram = "openp2s";
  };
}
