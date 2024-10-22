{
  x86_64 = import ./overlay-x86_64.nix;
  dt_ao_overlay = _final: prev: {
    deviceTree = prev.deviceTree // {
      applyOverlays = _final.callPackage ./apply-overlays-dtmerge.nix { };
    };
  };
}
