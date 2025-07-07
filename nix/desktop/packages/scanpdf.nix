{
  pkgs,
  writeScript,
  ...
}:
writeScript {
  name = "scanpdf";
  runtimeInputs = with pkgs; [
    libtiff # tiffcp
    tesseract
    xsane
    expect
    feh
  ];
  text = ./scanpdf.sh;
}
