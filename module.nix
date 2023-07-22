{ config, lib, pkgs, ... }:

let
  cfg = config.services.re-server;
  re-server = import (pkgs.fetchFromGitHub {
    owner = "d86leader";
    repo = "re";
    rev = "a82b7618a84748c639100f08af176ecdc156d088";
    hash = "sha256-gpQ84GvDkv1sDe28NVfQO5WfQMO/DZxHR6CzYBQsnWc=";
  }) { inherit pkgs; };
in {
  options.services.re-server = {
    enable = lib.mkEnableOption "re-server";

    port = lib.mkOption {
      type = lib.types.port;
      default = 8040;
      description = lib.mdDoc ''
        Port to listen on. Note that service always binds to 127.0.0.1
      '';
    };

    user = lib.mkOption {
      type = lib.types.str;
      default = "root";
      example = "yourUser";
      description = lib.mdDoc ''
        The user to service as. Recommended option: administrator user.
      '';
    };

    group = lib.mkOption {
      type = lib.types.str;
      default = "root";
      example = "yourGroup";
      description = lib.mdDoc ''
        The group to service as. Recommended option: administrator group.
      '';
    };

    configPath = lib.mkOption {
      type = lib.types.path;
      example = "/home/administrator/.config/re.ron";
      description = lib.mdDoc ''
        Location of redirections file. See service documentation for format.
      '';
    };
  };

  config = lib.mkIf cfg.enable {
    systemd.packages = [re-server];

    systemd.services.re-server = {
      description = "Re-directs service";
      after = [ "network-online.target" ];

      serviceConfig = {
        User = cfg.user;
        Group = cfg.group;
        ExecStart = ''${re-server}/bin/re-server \
          --port=${toString cfg.port} \
          --config=${cfg.configPath}
        '';

        Restart = "on-failure";
        StandardOutput = "journal";
        StandardError = "journal";
      };
    };
  };
}
