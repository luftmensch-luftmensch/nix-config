inputs:
let
  addPatches =
    pkg: patches:
    pkg.overrideAttrs (oldAttrs: {
      patches = (oldAttrs.patches or [ ]) ++ patches;
    });
in
{
  # For every flake input, aliases 'pkgs.inputs.${flake}' to
  # 'inputs.${flake}.packages.${pkgs.system}' or
  # 'inputs.${flake}.legacyPackages.${pkgs.system}' or
  flake-inputs = final: _: {
    inputs = builtins.mapAttrs (
      _: flake:
      let
        legacyPackages = (flake.legacyPackages or { }).${final.stdenv.hostPlatform.system} or { };
        packages = (flake.packages or { }).${final.stdenv.hostPlatform.system} or { };
      in
      if legacyPackages != { } then legacyPackages else packages
    ) inputs;
  };

  # Overlays for personal pkgs (callPackage)
  additions = final: _: import ../packages { pkgs = final; };

  # Overlays for various pkgs (e.g. broken / too old)
  modifications = final: prev: {
    stable = import inputs.nixpkgs {
      inherit (final) system;
      config.allowUnfree = true; # Forgive me Stallman
    };

    rofi-emoji-wayland = prev.rofi-emoji.overrideAttrs (_oldAttrs: {
      buildInputs = with final; [
        rofi-wayland-unwrapped
        cairo
        glib
        libnotify
        wl-clipboard
        xclip
        xsel
      ];
    });

    tdlib = prev.tdlib.overrideAttrs (_oldAttrs: {
      version = "1.8.55";
      src = final.fetchFromGitHub {
        owner = "tdlib";
        repo = "td";

        # The tdlib authors do not set tags for minor versions, but
        # external programs depending on tdlib constrain the minor
        # version, hence we set a specific commit with a known version.
        rev = "369ee922b45bfa7e8da357e4d62e93925862d86d";
        hash = "sha256-wmvRWZR1XoJCnN3j7LYp/xdZspjCa3VJv2d9sVU87j4=";
      };
    });

    # mpv-visualizer = prev.mpvScripts.visualizer.overrideAttrs (_oldAttrs: {
    #   patches = [ ./patches/visualizer.patch ];
    # });

    mpv-visualizer = addPatches prev.mpvScripts.visualizer [ ./patches/visualizer.patch ];
  };
}
