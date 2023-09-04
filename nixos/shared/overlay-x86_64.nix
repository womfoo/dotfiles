(self: super: {
  python3Packages = super.python3Packages.override {
    # overrides = pythonSelf: pythonSuper: {
    #   magic-wormhole = pythonSuper.magic-wormhole.overrideAttrs(z: rec{doCheck=false;doInstallCheck = false;});
    # };
  };
  haskellPackages = super.haskellPackages.override {
    overrides = haskellSelf: haskellSuper: {
      # multistate = self.haskell.lib.doJailbreak haskellSuper.multistate; # hspec >=2 && <2.9
      pi-lcd = self.haskell.lib.doJailbreak haskellSuper.pi-lcd; # bytestring >=0.10.0.2 && <0.11
      # what4 = self.haskell.lib.dontCheck (self.haskell.lib.doJailbreak haskellSuper.what4); # > tasty-sugar >=2.0 && <2.1
      # sketch-frp-copilot = self.haskell.lib.doJailbreak haskellSuper.sketch-frp-copilot;
      # arduino-copilot = self.haskell.lib.doJailbreak haskellSuper.arduino-copilot;
      # copilot-theorem = self.haskell.lib.doJailbreak haskellSuper.copilot-theorem; # bimap >=0.3 && <0.4 || >=0.5 && <0.6
      # reanimate = self.haskell.lib.dontCheck (self.haskell.lib.doJailbreak haskellSuper.reanimate); # aeson >=1.3.0.0 && <2
      # reanimate-svg = self.haskell.lib.dontCheck haskellSuper.reanimate-svg; # https://github.com/NixOS/nixpkgs/issues/153078
      # bech32 = self.haskell.lib.dontCheck haskellSuper.bech32;
    };
  };
})
