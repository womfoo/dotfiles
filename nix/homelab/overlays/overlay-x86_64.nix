(self: super: {
  haskellPackages = super.haskellPackages.override {
    overrides = haskellSelf: haskellSuper: {
      brick = self.haskell.lib.compose.addBuildDepend haskellSuper.random (self.haskell.lib.enableCabalFlag haskellSuper.brick "demos");
    };
  };
})
