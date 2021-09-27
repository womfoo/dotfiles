(self: super: {
  haskellPackages = super.haskellPackages.override {
    overrides = haskellSelf: haskellSuper: {
      reanimate = self.haskell.lib.dontCheck haskellSuper.reanimate;
      reanimate-svg = self.haskell.lib.dontCheck haskellSuper.reanimate-svg;
    };
  };
})
