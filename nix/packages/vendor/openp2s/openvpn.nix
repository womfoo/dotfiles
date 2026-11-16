# OpenVPN fork used by openp2s: upstream 2.7.8 + openp2s's two patches
# (long-credentials, experimental-azure-compat), built with the same
# configure flags as scripts/build-openvpn.sh. Installed under the private
# lib/openp2s path so it never shadows the system openvpn.
{
  lib,
  stdenv,
  fetchurl,
  pkg-config,
  openssl,
  libcap_ng,
  openp2s-src,
}:

stdenv.mkDerivation (finalAttrs: {
  pname = "openvpn-openp2s";
  version = "2.7.8";

  src = fetchurl {
    url = "https://swupdate.openvpn.net/community/releases/openvpn-${finalAttrs.version}.tar.gz";
    hash = "sha256-wHDR0kQLWm/KbCxoZFyYzUkhFqw2708JRhdxFVMsjjY=";
  };

  patches = [
    "${openp2s-src}/patches/${finalAttrs.version}/long-credentials.patch"
    "${openp2s-src}/patches/${finalAttrs.version}/experimental-azure-compat.patch"
  ];

  nativeBuildInputs = [ pkg-config ];
  buildInputs = [
    openssl
    libcap_ng
  ];

  configureFlags = [
    "--with-crypto-library=openssl"
    "--disable-lzo"
    "--disable-lz4"
    "--disable-comp-stub"
    # load-bearing: with pkcs11 enabled USER_PASS_LEN comes from a different #ifdef
    "--disable-pkcs11"
    "--disable-plugins"
    "--disable-plugin-auth-pam"
    "--disable-plugin-down-root"
    "--disable-systemd"
    "--disable-selinux"
    "--disable-port-share"
    "--disable-async-push"
    "--disable-ntlm"
    "--disable-unit-tests"
    "--disable-dco"
    "--enable-fragment"
    "--enable-management"
    "--enable-debug"
  ];

  # Same assertion build-openvpn.sh makes: the patch must really have landed.
  postPatch = ''
    grep -Eq '^#define[[:space:]]+USER_PASS_LEN[[:space:]]+8192' src/openvpn/misc.h
    grep -Eq '^#define[[:space:]]+TLS_CHANNEL_BUF_SIZE[[:space:]]+16384' src/openvpn/common.h
  '';

  enableParallelBuilding = true;

  installPhase = ''
    runHook preInstall
    install -Dm0755 src/openvpn/openvpn $out/lib/openp2s/openvpn
    runHook postInstall
  '';

  # openp2s trusts user_pass_len only if BUILDINFO is beside the binary and
  # binary_sha256 matches, so write it after strip/patchelf.
  postFixup = ''
    cat > $out/lib/openp2s/BUILDINFO <<EOF
    openvpn_version=${finalAttrs.version}
    openvpn_tag=v${finalAttrs.version}
    openvpn_commit=dc1f569087135894ac2703aeabd7a960ee9452d4
    openvpn_tarball_sha256=c070d1d2440b5a6fca6c2c68645c98cd492116ac36ef4f0946177115532c8e36
    patch_stack=long-credentials experimental-azure-compat
    long_credentials_sha256=$(sha256sum ${builtins.elemAt finalAttrs.patches 0} | cut -d' ' -f1)
    experimental_azure_compat_sha256=$(sha256sum ${builtins.elemAt finalAttrs.patches 1} | cut -d' ' -f1)
    binary_sha256=$(sha256sum $out/lib/openp2s/openvpn | cut -d' ' -f1)
    user_pass_len=8192
    tls_channel_buf_size=16384
    azure_compat_available=1
    built_on=nix
    EOF
  '';

  meta = {
    description = "OpenVPN patched for Azure P2S Entra ID auth (openp2s)";
    homepage = "https://github.com/wyruweso/openp2s";
    license = lib.licenses.gpl2Only;
    platforms = lib.platforms.linux;
    mainProgram = "openvpn";
  };
})
