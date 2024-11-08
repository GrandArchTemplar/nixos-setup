{ config, pkgs, ... }:

{
  hardware.nvidia.open = true;
  hardware.nvidia.package = config.boot.kernelPackages.nvidiaPackages.stable;
  services.xserver.videoDrivers = [ "nvidia" ];
  services.xserver.dpi = 96;
  environment.variables = {
    GDK_SCALE = "0.5";
  };
}
