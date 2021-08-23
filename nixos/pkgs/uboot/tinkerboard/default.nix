{ pkgs, fetchurl }:
let
  firmwareBlobs = pkgs.fetchFromGitHub {
    owner = "armbian";
    repo = "rkbin";
    rev = "06c2e6ee99a87b914461d0e269e1e3c8adcdc188";
    sha256 = "19032nk31vhcdxja5scwdxavc51qwxw4f3x5ykpa0gs65h7qhjk8";
  };
  ubootVersion = "2018.11";
in
pkgs.buildUBoot {
  # fails on 2021.04
  # SPL image is too large
  defconfig = "tinker-rk3288_defconfig";
  extraMeta.platforms = ["armv7l-linux"];
  filesToInstall = ["rk3288_boot.bin" "u-boot-rockchip-with-spl.bin"];
  version = ubootVersion;
  src = fetchurl {
    url = "ftp://ftp.denx.de/pub/u-boot/u-boot-${ubootVersion}.tar.bz2";
    sha256 = "0znkwljfwwn4y7j20pzz4ilqw8znphrfxns0x1lwdzh3xbr96z3k";
  };
  extraPatches = [
    ./0001-fixing-dtc-error.patch
    # ./0017-Fix-HDMI-some-issues.patch.disabled
    ./0018-pmic-enable-LDO2-vcc33_mipi-at-bootup.patch
    ./0036-auto-enable-ums-mode-when-TinkerBoard-is-connected-t.patch
    ./0037-add-10ms-delay-after-re-enable-EMMC.patch
    ./0039-fixed-enter-ums-mode-fail-sometimes.patch
    ./0045-modify-UMS-name-of-uboot.patch
    ./0049-added-timeout-when-force-entering-UMS-mode.patch
    ./0050-USB-VID-PID.patch
    ./100-tinker-s-eMMC-bootable.patch
    # ./101-u-boot-0002-rockchip-tinker-enable-rockchip-video-driver.patch.disabled
    ./add-overlay-support.patch
  ];
  postBuild = ''
    cp ${firmwareBlobs}/rk32/rk3288_ubootloader_v1.01.06.bin rk3288_boot.bin
	  tools/mkimage -n rk3288 -T rksd -d spl/u-boot-spl-dtb.bin u-boot-rockchip-with-spl.bin
		cat u-boot-dtb.bin >> u-boot-rockchip-with-spl.bin
  '';
}
