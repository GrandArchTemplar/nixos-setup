{ config, pkgs, ... }:

{
  imports =
    [
      ./nvidia.nix
      ./hardware-configuration.nix
    ];
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  networking.hostName = "nixos"; # Define your hostname.
  networking.networkmanager.enable = true;
  time.timeZone = "Europe/Moscow";
  i18n.defaultLocale = "en_US.UTF-8";

  fonts.packages = with pkgs; [
    fira-code
    fira-code-symbols
  ];
  i18n.extraLocaleSettings = {
    LC_ADDRESS = "ru_RU.UTF-8";
    LC_IDENTIFICATION = "ru_RU.UTF-8";
    LC_MEASUREMENT = "ru_RU.UTF-8";
    LC_MONETARY = "ru_RU.UTF-8";
    LC_NAME = "ru_RU.UTF-8";
    LC_NUMERIC = "ru_RU.UTF-8";
    LC_PAPER = "ru_RU.UTF-8";
    LC_TELEPHONE = "ru_RU.UTF-8";
    LC_TIME = "ru_RU.UTF-8";
  };
  services = {
    xserver = {
      xkb = {
        layout = "us,ru";
        variant = "";
        options = "grp:alt_shift_toggle";
      };
      enable = true;
      windowManager = {
        xmonad.enable = true;
        xmonad.enableContribAndExtras = true;
        xmonad.extraPackages = hpkgs: [
          hpkgs.xmonad-contrib
          hpkgs.xmonad-extras
          hpkgs.xmonad
        ];
      };
      displayManager = {
        startx.enable = true;
      };
      desktopManager = {
        xterm.enable = false;
        wallpaper = {
          mode = "fill";
          combineScreens = false;
        };
      };
    };
    displayManager = {
      defaultSession = "none+xmonad";
    };
  };

  services.logind.extraConfig = ''
    HandlePowerKey=ignore
    HandleSuspendKey=ignore
    HandleHibernateKey=ignore
  '';
  users = {
    defaultUserShell = pkgs.zsh;
    users.archgt = {
      isNormalUser = true;
      description = "archgt";
      extraGroups = [ "networkmanager" "wheel" ];
      packages = with pkgs; [ ];
    };
  };
  nixpkgs.config.allowUnfree = true;
  environment = {
    pathsToLink = [ "/share/agda" ];
    variables = {
      EDITOR = "vim";
    };
    extraInit = ''
    '';
    systemPackages = with pkgs; [
      (pkgs.texlive.combine {
        inherit (pkgs.texlive)
          scheme-full
          ;
      })
      ((vim_configurable.override { }).customize {
        name = "vim";
        vimrcConfig.packages.myplugins = with pkgs.vimPlugins; {
          start = [
            vim-nix
            vim-lastplace
          ];
          opt = [ ];
        };
        vimrcConfig.customRC = ''
          filetype plugin indent on
          set nocompatible
          set backspace=indent,eol,start
          syntax on
          set smartindent         
          set tabstop=2
          set shiftwidth=4
          set expandtab
          set autoindent
          set number
        '';
      })
      (agda.withPackages [ agdaPackages.standard-library ])
      vim
      feh
      wget
      git
      google-chrome
      vscode
      dmenu
      ghc
      haskellPackages.xmonad
      haskellPackages.xmobar
      haskellPackages.haskell-language-server
      haskellPackages.hlint
      alacritty
      telegram-desktop
      screenfetch
      zsh
      i3lock-color
      oh-my-zsh
      rustup
      gcc
      openjdk
      glibc
      lldb
      pavucontrol
      nixpkgs-fmt
      pciutils
      usbutils
      python3
      xclip
      maim
      discord
      trayer
    ];
  };



  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  # };
  programs = {
    zsh = {
      enable = true;
      ohMyZsh = {
        enable = true;
        theme = "norm";
      };
    };
    steam = {
      enable = true;
      remotePlay.openFirewall = true; # Open ports in the firewall for Steam Remote Play
      dedicatedServer.openFirewall = true; # Open ports in the firewall for Source Dedicated Server
    };
  };
  # List services that you want to enable:
  # Enable the OpenSSH daemon.
  # services.openssh.enable = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "23.05"; # Did you read the comment?
  security.rtkit.enable = true;
  services.blueman.enable = true;
  services.picom.enable = true;
}
