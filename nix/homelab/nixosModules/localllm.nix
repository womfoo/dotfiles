{
  lib,
  pkgs,
  config,
  ...
}:
{
  services.llama-cpp = {
    enable = true;
    package = pkgs.llama-cpp.override { cudaSupport = true; };
    extraFlags = [
      # "-m" "/data/hf_ext/hub/models--unsloth--gpt-oss-20b-GGUF/snapshots/d449b42d93e1c2c7bda5312f5c25c8fb91dfa9b4/gpt-oss-20b-Q4_K_M.gguf"
      # "--n-cpu-moe" "12"
      # "--jinja"
      # "-c" "32768"
      # "--flash-attn" "on"
      # "-ctk" "q8_0" "-ctv" "q8_0"
      # "--no-mmap"
      # "--ui-mcp-proxy"
      "-m"
      "/data/Qwen3.5-4B-Q4_K_M.gguf"
      "-ngl"
      "99"
      "-c"
      "131072"
      "-ctk"
      "q8_0"
      "-ctv"
      "q8_0"
      "--flash-attn"
      "on"
      "-b"
      " 2048"
      "-ub"
      "512"
      "-t"
      "6"
      "--no-mmap"
      "--ui-mcp-proxy"
    ];

  };

  # services.ollama.enable = true;
  # services.ollama.package = pkgs.ollama-cuda;
  # services.ollama.loadModels = [
  #   "deepseek-r1:7b"
  #   "gemma2:2b"
  #   "llama3.1"
  #   "qwen2.5-coder:7b"
  #   "qwen3:8b"
  # ];
  # services.ollama.home = "/armorydata/2tbtmp/var-lib-private-ollama";
  # services.ollama.host = "0.0.0.0"; # yolo

  # services.open-webui = {
  #   enable = true;
  #   environment = {
  #     WEBUI_AUTH = "False";
  #   };
  #   package = inputs.cells.vendor.packages.open-webui-25-11;
  # };

  nix.settings.substituters = [
    "https://cache.nixos-cuda.org"
  ];
  nix.settings.trusted-public-keys = [
    "cache.nixos-cuda.org:74DUi4Ye579gUqzH4ziL9IyiJBlDpMRn9MBN8oNan9M="
  ];

}
