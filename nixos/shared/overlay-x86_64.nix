(self: super: {
  /*
  hydra-unstable = super.hydra-unstable.overrideAttrs (o: rec{
    src = super.fetchFromGitHub {
      owner = "NixOS";
      repo = "hydra";
      rev = "9bce425c3304173548d8e822029644bb51d35263";
      sha256 = "sha256-tGzwKNW/odtAYcazWA9bPVSmVXMGKfXsqCA1UYaaxmU=";
    };
  });
  python3Packages = super.python3Packages.override {
    overrides = pythonSelf: pythonSuper: {
      capturer = pythonSuper.capturer.overrideAttrs(z: rec{doCheck=false;});
      websockets = pythonSuper.websockets.overrideAttrs(z: rec{doCheck=false;});
    };
  };
  */
  haskellPackages = super.haskellPackages.override {
    overrides = haskellSelf: haskellSuper: {
      reanimate = self.haskell.lib.dontCheck haskellSuper.reanimate;
      reanimate-svg = self.haskell.lib.dontCheck haskellSuper.reanimate-svg;
    };
  };
})
