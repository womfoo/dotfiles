(self: super: {
  # review actutal python version vs python3 alias
  # python312 = super.python312.override {
  #   packageOverrides = pyself: pysuper: {
  #     triton-llvm = pysuper.triton-llvm.override {doCheck = false;};
  #   };
  # };
  haskellPackages = super.haskellPackages.override {
    overrides = haskellSelf: haskellSuper: {
      brick = self.haskell.lib.compose.addBuildDepend haskellSuper.random (
        self.haskell.lib.enableCabalFlag haskellSuper.brick "demos"
      );
    };
  };
})
