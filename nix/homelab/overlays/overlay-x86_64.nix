(self: super: {
  # review actutal python version vs python3 alias
  # python312 = super.python312.override {
  #   packageOverrides = pyself: pysuper: {
  #     triton-llvm = pysuper.triton-llvm.override {doCheck = false;};
  #   };
  # };
  libzim = super.libzim.override { icu = super.icu75; };
  libkiwix = super.libkiwix.override { icu = super.icu75; };
  kiwix-tools = super.kiwix-tools.override { icu = super.icu75; };
  haskellPackages = super.haskellPackages.override {
    overrides = haskellSelf: haskellSuper: {
      brick = self.haskell.lib.compose.addBuildDepend haskellSuper.random (
        self.haskell.lib.enableCabalFlag haskellSuper.brick "demos"
      );
    };
  };
})
