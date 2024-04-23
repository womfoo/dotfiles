# https://github.com/NixOS/nixpkgs/pull/209778
{
  lib,
  fetchurl,
  appimageTools,
  wrapGAppsHook,
}:

let
  pname = "openlens";
  version = "6.2.5";
  name = "${pname}-${version}";

  src = fetchurl {
    url = "https://github.com/MuhammedKalkan/OpenLens/releases/download/v${version}/OpenLens-${version}.x86_64.AppImage";
    sha256 = "sha256-7eWyqPUOOh03cQL9JVNx5XyEpCAN1URSTLSLvREWom0=";
    name = "${pname}.AppImage";
  };

  appimageContents = appimageTools.extractType2 { inherit name src; };
in
appimageTools.wrapType2 {
  inherit name src;

  extraInstallCommands = ''
    mv $out/bin/${name} $out/bin/${pname}

    install -m 444 -D ${appimageContents}/open-lens.desktop $out/share/applications/${pname}.desktop
    install -m 444 -D ${appimageContents}/usr/share/icons/hicolor/512x512/apps/open-lens.png \
       $out/share/icons/hicolor/512x512/apps/${pname}.png

    substituteInPlace $out/share/applications/${pname}.desktop \
      --replace 'Icon=open-lens' 'Icon=${pname}' \
      --replace 'Exec=AppRun' 'Exec=${pname}'
  '';

  meta = with lib; {
    description = "The Kubernetes IDE";
    homepage = "https://github.com/MuhammedKalkan/OpenLens";
    license = licenses.mit;
    maintainers = with maintainers; [ benwbooth ];
    platforms = [ "x86_64-linux" ];
  };
}
