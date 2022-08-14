(self: super: {
  python3Packages = super.python3Packages.override {
    overrides = pythonSelf: pythonSuper: {
      magic-wormhole = pythonSuper.magic-wormhole.overrideAttrs(z: rec{doCheck=false;doInstallCheck = false;});
    };
  };
  haskellPackages = super.haskellPackages.override {
    overrides = haskellSelf: haskellSuper: {
      reanimate = self.haskell.lib.dontCheck (self.haskell.lib.doJailbreak haskellSuper.reanimate); # aeson >=1.3.0.0 && <2
      reanimate-svg = self.haskell.lib.dontCheck haskellSuper.reanimate-svg; # https://github.com/NixOS/nixpkgs/issues/153078
    };
  };
})
