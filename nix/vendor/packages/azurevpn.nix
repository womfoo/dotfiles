{
  lib,
  stdenv,
  fetchurl,
  dpkg,
  autoPatchelfHook,
  makeWrapper,
  openssl,
  gtk3,
  libsecret,
  cairo,
  nss,
  nspr,
  libuuid,
  at-spi2-core,
  libdrm,
  mesa,
  gtk2,
  glib,
  pango,
  atk,
  curl,
  zenity,
  cacert,
  openvpn,
  buildFHSEnv,
  writeShellScript,
  libcap,
# libxcb,
# cairo-xcb,
# libX11,
# libXcomposite,
# libXdamage,
# libXext,
# libXfixes,
# libXrandr,
# libxkbcommon,
# libxshmfence,
}:
let
  pname = "microsoft-azurevpnclient";
  version = "3.0.0";

  unpacked = stdenv.mkDerivation rec {
    inherit pname version;

    src = fetchurl {
      url = "https://packages.microsoft.com/ubuntu/22.04/prod/pool/main/m/microsoft-azurevpnclient/microsoft-azurevpnclient_${version}_amd64.deb";
      hash = "sha256-nl02BDPR03TZoQUbspplED6BynTr6qNRVdHw6fyUV3s=";
    };

    runtimeDependencies = [ zenity ];

    nativeBuildInputs = [
      dpkg
      autoPatchelfHook
      makeWrapper
      libcap

    ];

    buildInputs = [
      zenity
      openssl
      gtk3
      libsecret
      cairo
      # libxcb
      nss
      nspr
      libuuid
      stdenv.cc.cc.lib
      at-spi2-core
      libdrm
      mesa
      gtk2
      glib
      pango
      atk
      curl
      cacert # Add this
      openvpn

      # cairo-xcb
      # libX11
      # libXcomposite
      # libXdamage
      # libXext
      # libXfixes
      # libXrandr
      # libxkbcommon
      # libxshmfence
    ];

    unpackPhase = ''
      dpkg-deb -x $src .
    '';

    # addAutoPatchelfSearchPath ${jre8}/lib/openjdk/jre/lib/
    # preBuild = ''
    #   addAutoPatchelfSearchPath opt/microsoft/microsoft-azurevpnclient/lib
    # '';

    # runtimeDependencies = [ "$out/lib" ];

    installPhase = ''
      mkdir -p $out
      cp -r opt $out
      cp -r usr/* $out

      mkdir -p $out/bin

      ln -s $out/opt/microsoft/microsoft-azurevpnclient/microsoft-azurevpnclient $out/bin/microsoft-azurevpnclient
      ln -s $out/opt/microsoft/microsoft-azurevpnclient/lib $out

      # TODO: Replace `prefix` with `set`?
      wrapProgram $out/bin/microsoft-azurevpnclient \
        --prefix SSL_CERT_DIR : "${cacert.unbundled}/etc/ssl/certs" \
        --prefix PATH : "${zenity}/bin" \
        --prefix LD_LIBRARY_PATH : ${lib.makeLibraryPath buildInputs} \
        --prefix LD_LIBRARY_PATH : "$out/lib"

      # TODO:
      # Fix desktop file location
      # mkdir -p $out/share/applications
      # mv $out/share/applications/azurevpnclient.desktop $out/share/applications/
    '';
  };

  certs = stdenv.mkDerivation {
    name = "${pname}-certs";

    dontUnpack = true;

    buildPhase = ''
      mkdir -p $out;

      ln -s ${cacert.unbundled}/etc/ssl/certs/*.crt $out

      for file in $out/*.crt; do
        mv -- "$file" "''${file%.crt}.pem"
      done

      ls $out
    '';

    installPhase = "true";
  };

  wrapped = buildFHSEnv {
    inherit pname version;

    runScript = writeShellScript "${pname}-wrapper.sh" ''
      echo --------------------------
      mkdir -p /etc/ssl/certs
      cat <<EOF >> /etc/ssl/certs/DigiCert_Global_Root_CA.pem
      -----BEGIN CERTIFICATE-----
      MIIDrzCCApegAwIBAgIQCDvgVpBCRrGhdWrJWZHHSjANBgkqhkiG9w0BAQUFADBh
      MQswCQYDVQQGEwJVUzEVMBMGA1UEChMMRGlnaUNlcnQgSW5jMRkwFwYDVQQLExB3
      d3cuZGlnaWNlcnQuY29tMSAwHgYDVQQDExdEaWdpQ2VydCBHbG9iYWwgUm9vdCBD
      QTAeFw0wNjExMTAwMDAwMDBaFw0zMTExMTAwMDAwMDBaMGExCzAJBgNVBAYTAlVT
      MRUwEwYDVQQKEwxEaWdpQ2VydCBJbmMxGTAXBgNVBAsTEHd3dy5kaWdpY2VydC5j
      b20xIDAeBgNVBAMTF0RpZ2lDZXJ0IEdsb2JhbCBSb290IENBMIIBIjANBgkqhkiG
      9w0BAQEFAAOCAQ8AMIIBCgKCAQEA4jvhEXLeqKTTo1eqUKKPC3eQyaKl7hLOllsB
      CSDMAZOnTjC3U/dDxGkAV53ijSLdhwZAAIEJzs4bg7/fzTtxRuLWZscFs3YnFo97
      nh6Vfe63SKMI2tavegw5BmV/Sl0fvBf4q77uKNd0f3p4mVmFaG5cIzJLv07A6Fpt
      43C/dxC//AH2hdmoRBBYMql1GNXRor5H4idq9Joz+EkIYIvUX7Q6hL+hqkpMfT7P
      T19sdl6gSzeRntwi5m3OFBqOasv+zbMUZBfHWymeMr/y7vrTC0LUq7dBMtoM1O/4
      gdW7jVg/tRvoSSiicNoxBN33shbyTApOB6jtSj1etX+jkMOvJwIDAQABo2MwYTAO
      BgNVHQ8BAf8EBAMCAYYwDwYDVR0TAQH/BAUwAwEB/zAdBgNVHQ4EFgQUA95QNVbR
      TLtm8KPiGxvDl7I90VUwHwYDVR0jBBgwFoAUA95QNVbRTLtm8KPiGxvDl7I90VUw
      DQYJKoZIhvcNAQEFBQADggEBAMucN6pIExIK+t1EnE9SsPTfrgT1eXkIoyQY/Esr
      hMAtudXH/vTBH1jLuG2cenTnmCmrEbXjcKChzUyImZOMkXDiqw8cvpOp/2PV5Adg
      06O/nVsJ8dWO41P0jmP6P6fbtGbfYmbW0W5BjfIttep3Sp+dWOIrWcBAI+0tKIJF
      PnlUkiaY4IBIqDfv8NZ5YBberOgOzW6sRBc4L0na4UU+Krk2U886UAb3LujEV0ls
      YSEY1QSteDwsOoBrp+uvFRTp2InBuThs4pFsiv9kuXclVzDAGySj4dzp30d8tbQk
      CAUw7C29C79Fv1C5qfPrmAESrciIxpg0X40KPMbp1ZWVbd4=
      -----END CERTIFICATE-----
      EOF
      exec ${unpacked}/bin/${pname}
    '';

    extraBwrapArgs = [
      "--tmpfs /etc/ssl"
    ];

    meta = {
      description = "Microsoft Azure VPN Client";
      homepage = "https://azure.microsoft.com/en-us/services/vpn-gateway/";
      # TODO:
      # license = licenses.unfree;
      platforms = [ "x86_64-linux" ];
      maintainers = [ ];
    };
  };
in
wrapped
