{ pkgs, ... }:
let
  fan_p6 = "/sys/devices/platform/fan1/hwmon/hwmon0/pwm1";
  #fan_p6 = "/sys/devices/platform/fan1/hwmon/hwmon1/pwm1";
  #fan_p6 = "/sys/devices/platform/fan1/hwmon/hwmon2/pwm1";
  fan_p7 = "/sys/devices/platform/fan2/hwmon/hwmon1/pwm1";
  #fan_p7 = "/sys/devices/platform/fan2/hwmon/hwmon2/pwm1";
  #fan_p7 = "/sys/devices/platform/fan2/hwmon/hwmon3/pwm1";
  #fan_p7 = "/sys/devices/platform/fan2/hwmon/hwmon4/pwm1";

  #temp_input = "/sys/class/hwmon/hwmon0/temp1_input"; # lm75
  temp_input = "/sys/class/hwmon/hwmon2/temp1_input"; # lm75
  #temp_input = "/sys/class/hwmon/hwmon4/temp1_input"; # lm75
  #temp_input = "/sys/class/hwmon/hwmon-1/temp1_input"; # lm75
in
{
  hardware.fancontrol.enable = true;
  hardware.fancontrol.config = ''
    # Helios64 PWM Fan Control Configuration
    # Temp source : /dev/thermal-cpu
    INTERVAL=10
    FCTEMPS=${fan_p6}=${temp_input} ${fan_p7}=${temp_input}
    MINTEMP=${fan_p6}=40 ${fan_p7}=40
    MAXTEMP=${fan_p6}=80 ${fan_p7}=80
    MINSTART=${fan_p6}=60 ${fan_p7}=60
    MINSTOP=${fan_p6}=29 ${fan_p7}=29
    MINPWM=20
  '';
}
