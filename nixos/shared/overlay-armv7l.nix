(self: super: {
  collectd = super.collectd.override (o: {jdk = null;});
  ell = super.ell.overrideAttrs (o: {doCheck = false;});
  haskellPackages = super.haskellPackages.override {
    overrides = haskellSelf: haskellSuper: {
      arbtt = self.haskell.lib.dontCheck haskellSuper.arbtt;
      hint = self.haskell.lib.dontCheck haskellSuper.hint;
      vector = self.haskell.lib.dontCheck haskellSuper.vector;
      zip-archive = self.haskell.lib.dontCheck haskellSuper.zip-archive;
    };
  };
  python39Packages = super.python36Packages.override {
    overrides = pythonSelf: pythonSuper: {
      pyflakes = pythonSuper.pyflakes.overrideAttrs(z: rec{doCheck=false;doInstallCheck = false;});
      scipy = pythonSuper.scipy.overrideAttrs (z: rec{doCheck=false;doInstallCheck = false;});
      whoosh = pythonSuper.whoosh.overrideAttrs (z: rec{doCheck=false;doInstallCheck = false;});
    };
  };
})
