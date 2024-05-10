# better repl with preloaded functions and libs already loaded
# Run with: nix repl --file repl.nix --arg host '"<value>"'
{host ? "atlas"}: let
  user = "valentino";
  home = "${user}@${host}";
  flake = builtins.getFlake (toString ./.);
  inherit (flake.inputs.nixpkgs) lib;
in rec {
  inherit (flake) inputs self;
  inherit (flake.inputs) nixpkgs;
  inherit flake lib host user;
  inherit (flake.nixosConfigurations.${host}) config pkgs;

  hm-cfg = flake.homeConfigurations."${home}".config;
  hm-opts = flake.homeConfigurations."${home}".options;

  packageNames = map (p: p.pname or p.name or null) hm-cfg.home.packages;
  hasPackage = name: lib.any (x: x == name) packageNames;
}
