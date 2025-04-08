{
  inputs,
  cell,
}:
{
  microclaude = inputs.std.lib.ops.mkMicrovm (cell.nixosModules.claudevm);
}
