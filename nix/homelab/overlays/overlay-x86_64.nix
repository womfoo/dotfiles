(self: super: {
  # review actutal python version vs python3 alias
  python312 = super.python312.override {
    packageOverrides = pyself: pysuper: {
      duckduckgo-search = pyself.callPackage ./python-modules/duckduckgo-search {};
      opentelemetry-proto = pysuper.opentelemetry-proto.overrideAttrs(z: rec{
        nativeBuildInputs = z.nativeBuildInputs ++ [ super.python3Packages.pythonRelaxDepsHook ];
        pythonRelaxDeps = [ "protobuf" ];});
      primp = pyself.callPackage ./python-modules/primp { SystemConfiguration = null; };
    };
  };
  haskellPackages = super.haskellPackages.override {
    overrides = haskellSelf: haskellSuper: {
      brick = self.haskell.lib.compose.addBuildDepend haskellSuper.random (self.haskell.lib.enableCabalFlag haskellSuper.brick "demos");
    };
  };
})
