{
  lib,
  pkgs,
  config,
  ...
}:
{
  services.ollama.enable = true;
  services.ollama.package = pkgs.ollama-cuda;
  services.ollama.loadModels = [
    "gemma2:2b"
    "llama3.1"
    "deepseek-r1:1.5b"
    "deepseek-r1:7b"
  ];
  services.open-webui = {
    enable = true;
    environment = {
      WEBUI_AUTH = "False";
    };
    package = inputs.cells.vendor.packages.open-webui-25-05;
  };

  nix.settings.substituters = [
    "https://cache.nixos-cuda.org"
  ];
  nix.settings.trusted-public-keys = [
    "cache.nixos-cuda.org:74DUi4Ye579gUqzH4ziL9IyiJBlDpMRn9MBN8oNan9M="
  ];

}
