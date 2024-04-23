(self: super: {
  python3Packages = super.python3Packages.override {
    # overrides = pythonSelf: pythonSuper: {
    #   magic-wormhole = pythonSuper.magic-wormhole.overrideAttrs(z: rec{doCheck=false;doInstallCheck = false;});
    # };
  };
  haskellPackages = super.haskellPackages.override {
    overrides = haskellSelf: haskellSuper: {
      pi-lcd = self.haskell.lib.doJailbreak haskellSuper.pi-lcd; # bytestring >=0.10.0.2 && <0.11
      brick = self.haskell.lib.enableCabalFlag haskellSuper.brick "demos";

      # multistate = self.haskell.lib.doJailbreak haskellSuper.multistate; # hspec >=2 && <2.9
      # what4 = self.haskell.lib.dontCheck (self.haskell.lib.doJailbreak haskellSuper.what4); # > tasty-sugar >=2.0 && <2.1
      # sketch-frp-copilot = self.haskell.lib.doJailbreak haskellSuper.sketch-frp-copilot;
      # arduino-copilot = self.haskell.lib.doJailbreak haskellSuper.arduino-copilot;
      # copilot-theorem = self.haskell.lib.doJailbreak haskellSuper.copilot-theorem; # bimap >=0.3 && <0.4 || >=0.5 && <0.6
      # reanimate = self.haskell.lib.dontCheck (self.haskell.lib.doJailbreak haskellSuper.reanimate); # aeson >=1.3.0.0 && <2
      # reanimate-svg = self.haskell.lib.dontCheck haskellSuper.reanimate-svg; # https://github.com/NixOS/nixpkgs/issues/153078
      # bech32 = self.haskell.lib.dontCheck haskellSuper.bech32;
      hpdft = self.haskell.lib.doJailbreak haskellSuper.hpdft; # bytestring >=0.10.0.2 && <0.11

#        > Using Parsec parser
#        > Configuring hpdft-0.1.1.3...
#        > CallStack (from HasCallStack):
#        >   withMetadata, called at libraries/Cabal/Cabal/src/Distribution/Simple/Utils.hs:370:14 in Cabal-3.8.1.0:Distribution.Simple.Utils
#        > Error: Setup: Encountered missing or private dependencies:
#        > base >=4.18.0 && <4.19,
#        > directory >=1.3.8 && <1.4,
#        > optparse-applicative >=0.18.1 && <0.19
#
    };
  };
})
