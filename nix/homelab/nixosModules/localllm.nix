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
    "deepseek-r1:7b"
    "gemma2:2b"
    "llama3.1"
    "qwen2.5-coder:7b"
    "qwen3:8b"
  ];

  services.ollama.host = "0.0.0.0"; # yolo

  services.open-webui = {
    enable = true;
    environment = {
      WEBUI_AUTH = "False";
    };
    # package = inputs.cells.vendor.packages.open-webui-25-05;
  };

  nix.settings.substituters = [
    "https://cache.nixos-cuda.org"
  ];
  nix.settings.trusted-public-keys = [
    "cache.nixos-cuda.org:74DUi4Ye579gUqzH4ziL9IyiJBlDpMRn9MBN8oNan9M="
  ];

}
