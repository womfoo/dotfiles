(self: super: {
  python3Packages = super.python3Packages.override {
    # overrides = pythonSelf: pythonSuper: {
    #   magic-wormhole = pythonSuper.magic-wormhole.overrideAttrs(z: rec{doCheck=false;doInstallCheck = false;});
    # };
  };
  haskellPackages = super.haskellPackages.override {
    overrides = haskellSelf: haskellSuper: {
      multistate = self.haskell.lib.doJailbreak haskellSuper.multistate; # hspec >=2 && <2.9
      # reanimate = self.haskell.lib.dontCheck (self.haskell.lib.doJailbreak haskellSuper.reanimate); # aeson >=1.3.0.0 && <2
      # reanimate-svg = self.haskell.lib.dontCheck haskellSuper.reanimate-svg; # https://github.com/NixOS/nixpkgs/issues/153078
      # bech32 = self.haskell.lib.dontCheck haskellSuper.bech32;
    };
  };
})
