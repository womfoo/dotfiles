{
  config,
  lib,
  pkgs,
  ...
}:
let
  inherit (inputs.cells) iot;
  inherit (inputs.lihim.lihim.constants.devices) wizdreadfort;

  china01 = builtins.readFile ./china01.lircd.conf;
  remoteName = "china01";

  mkAction =
    {
      button,
      hosts,
    }:
    let
      args = lib.concatStringsSep " " (
        map (h: inputs.lihim.lihim.constants.devices."${h}".interfaces.wlan.ip) hosts
      );
    in
    ''
      begin
       prog = irexec
       remote = ${remoteName}
       button = ${button}
       config = ${iot.packages.wizkell}/bin/wizkell toggle ${args}
      end
    '';

  wizkellActions = pkgs.writeText "wizkell-lircrc" (
    lib.strings.concatLines [
      (mkAction {
        button = "KEY_AUX";
        hosts = [
          "wizbulb1"
          "wizbulb2"
        ];
      })
      (mkAction {
        button = "KEY_4";
        hosts = [ "wizbulb1" ];
      })
      (mkAction {
        button = "KEY_5";
        hosts = [ "wizbulb2" ];
      })

      (mkAction {
        button = "KEY_1";
        hosts = [ "wizbulb3" ];
      })
      (mkAction {
        button = "KEY_7";
        hosts = [ "wizbulb3" ];
      })
    ]
  );
in
{
  services.udev.extraRules = ''
    SUBSYSTEM=="lirc", KERNEL=="lirc*", OWNER="lirc", GROUP="lirc", MODE="0660"
  '';

  services.lirc = {
    enable = true;
    configs = [
      china01
    ];
    options = ''
      [lircd]
      device = /dev/lirc1
      driver = default
    '';
  };

  systemd.services.wizkell = {
    description = "irexec wizkell script";
    wantedBy = [ "multi-user.target" ];
    after = [
      "network.target"
      "lirc.service"
    ];
    serviceConfig = {
      User = "lirc";
      Type = "forking";
      ExecStart = "${pkgs.lirc}/bin/irexec ${wizkellActions}";
      Restart = "always";
      # Requires = [ "sockets.target" ];
    };
  };

  hardware.deviceTree.filter = "bcm2711-rpi-4*.dtb";

  hardware.deviceTree = {
    overlays = [
      # https://github.com/raspberrypi/linux/blob/rpi-6.1.y/arch/arm/boot/dts/overlays/
      {
        name = "gpio-ir-overlay";
        # dtsText = builtins.readFile ./gpio-ir-overlay.dts;}
        dtsText = ''
          // Definitions for ir-gpio module
          /dts-v1/;
          /plugin/;

          / {
                  #compatible = "brcm,bcm2835";
                  compatible = "brcm,bcm2711";

                  fragment@0 {
                          target-path = "/";
                          __overlay__ {
                                  gpio_ir: ir-receiver@12 {
                                          compatible = "gpio-ir-receiver";
                                          pinctrl-names = "default";
                                          pinctrl-0 = <&gpio_ir_pins>;

                                          // pin number, high or low
                                          gpios = <&gpio 17 1>;

                                          // parameter for keymap name
                                          linux,rc-map-name = "rc-rc6-mce";

                                          status = "okay";
                                  };
                          };
                  };

                  fragment@1 {
                          target = <&gpio>;
                          __overlay__ {
                                  gpio_ir_pins: gpio_ir_pins@12 {
                                          brcm,pins = <17>;                       // pin 17
                                          brcm,function = <0>;                    // in
                                          brcm,pull = <2>;                        // up
                                  };
                          };
                  };

                  __overrides__ {
                          // parameters
                          gpio_pin =      <&gpio_ir>,"gpios:4",           // pin number
                                          <&gpio_ir>,"reg:0",
                                          <&gpio_ir_pins>,"brcm,pins:0",
                                          <&gpio_ir_pins>,"reg:0";
                          gpio_pull = <&gpio_ir_pins>,"brcm,pull:0";              // pull-up/down state
                          invert = <&gpio_ir>,"gpios:8";                          // 0 = active high input

                          rc-map-name = <&gpio_ir>,"linux,rc-map-name";           // default rc map
                  };
          };
        '';
      }
      {
        name = "gpio-ir-tx-overlay";
        # dtsText = builtins.readFile ./gpio-ir-tx-overlay.dts;}
        dtsText = ''
          /dts-v1/;
          /plugin/;

          / {
          	#compatible = "brcm,bcm2835";
            compatible = "brcm,bcm2711";

          	fragment@0 {
          		target = <&gpio>;
          		__overlay__ {
          			gpio_ir_tx_pins: gpio_ir_tx_pins@12 {
          				brcm,pins = <27>;
          				brcm,function = <1>;	// out
          			};
          		};
          	};

          	fragment@1 {
          		target-path = "/";
          		__overlay__ {
          			gpio_ir_tx: gpio-ir-transmitter@12 {
          				compatible = "gpio-ir-tx";
          				pinctrl-names = "default";
          				pinctrl-0 = <&gpio_ir_tx_pins>;
          				gpios = <&gpio 27 0>;
          			};
          		};
          	};

          	__overrides__ {
          		gpio_pin = <&gpio_ir_tx>, "gpios:4",           	// pin number
          			   <&gpio_ir_tx>, "reg:0",
          			   <&gpio_ir_tx_pins>, "brcm,pins:0",
          			   <&gpio_ir_tx_pins>, "reg:0";
          		invert = <&gpio_ir_tx>, "gpios:8";		// 1 = active low
          	};
          };
        '';
      }
    ];
  };
}
