{
  lib,
  stdenv,
  fetchzip,
  rustPlatform,
  fetchFromGitHub,
  fetchPnpmDeps,

  # patches
  libayatana-appindicator,

  # nativeBuildInputs
  cargo-tauri,
  nodejs,
  pkg-config,
  pnpm_11,
  pnpmConfigHook,

  # buildInputs
  atk,
  glib,
  libsoup_3,
  onnxruntime,
  openssl,
  pango,
  # linux-only:
  alsa-lib,
  libappindicator,
  libx11,
  libxi,
  libxkbcommon,
  libxtst,
  webkitgtk_4_1,
  xdotool,

  # passthru
  nix-update-script,
}:

let
  pnpm = pnpm_11;

  parakeet-model = fetchzip {
    url = "https://github.com/Kieirra/murmure-model/releases/download/1.0.0/parakeet-tdt-0.6b-v3-int8.zip";
    hash = "sha256-rlV7mi5Y6qu/9jRWRPNBlABW8GxsvVAMCM6Ye2tVb2s=";
  };
in
rustPlatform.buildRustPackage (finalAttrs: {
  pname = "murmure";
  version = "2.0.3";

  __structuredAttrs = true;

  src = fetchFromGitHub {
    owner = "Kieirra";
    repo = "murmure";
    tag = finalAttrs.version;
    hash = "sha256-fmwBcJtb+6wXMHel0YHrwXiJVreDnLlcRLPUq26ZW8w=";
  };

  # The libappindicator_sys crate loads these libraries at runtime
  postPatch = lib.optionalString stdenv.hostPlatform.isLinux ''
    substituteInPlace $cargoDepsCopy/*/libappindicator-sys-*/src/lib.rs \
      --replace-fail \
        "libayatana-appindicator3.so.1" \
        "${libayatana-appindicator}/lib/libayatana-appindicator3.so.1" \
  '';

  pnpmDeps = fetchPnpmDeps {
    inherit (finalAttrs)
      pname
      version
      src
      ;
    inherit pnpm;
    fetcherVersion = 4;
    hash = "sha256-OEBI5Z4nA556NMdhv4iZ5JC32mm49det/K7KOUQHGMI=";
  };

  cargoRoot = "src-tauri";
  buildAndTestSubdir = finalAttrs.cargoRoot;

  cargoHash = "sha256-qcYu1OlkfdMEov+0TbD40tpDQLsSdTvqvPSBTfjcJCU=";

  env = {
    OPENSSL_NO_VENDOR = true;
    # Use onnxruntime from nixpkgs instead of downloading prebuilt binaries
    ORT_LIB_LOCATION = "${lib.getLib onnxruntime}/lib";
    ORT_PREFER_DYNAMIC_LINK = "1";
  };

  nativeBuildInputs = [
    cargo-tauri.hook
    nodejs
    pkg-config
    pnpm
    pnpmConfigHook
  ];

  buildInputs = [
    atk
    glib
    libsoup_3
    onnxruntime
    openssl
    pango
  ]
  ++ lib.optionals stdenv.hostPlatform.isLinux [
    alsa-lib
    libappindicator
    libx11
    libxi
    libxkbcommon
    libxtst
    webkitgtk_4_1
    xdotool
  ];

  checkFlags = [
    # Couldn't compile the test.
    # error[E0433]: failed to resolve: use of unresolved module or unlinked crate `transcribe_rs`
    "--skip=engine::engine::ParakeetEngine"
    "--skip=engine::engine::ParakeetEngine::new"
    "--skip=engine::engine::ParakeetModelParams::int8"
    "--skip=engine::transcription_engine::TranscriptionEngine"
    "--skip=engine::transcription_engine::TranscriptionEngine"
  ];

  postInstall = ''
    mkdir -p $out/lib/murmure/resources
    ln -s ${parakeet-model} $out/lib/murmure/resources/parakeet-tdt-0.6b-v3-int8
  '';

  passthru.updateScript = nix-update-script { };

  meta = {
    description = "Privacy-first and free Speech-to-Text";
    homepage = "https://murmure.al1x-ai.com";
    downloadPage = "https://github.com/Kieirra/murmure";
    changelog = "https://github.com/Kieirra/murmure/releases/tag/${finalAttrs.version}";
    license = lib.licenses.gpl3Only;
    maintainers = with lib.maintainers; [ GaetanLepage ];
    mainProgram = "murmure";
  };
})
