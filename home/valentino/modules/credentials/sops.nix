{
  config,
  inputs,
  pkgs,
  ...
}: {
  imports = [inputs.sops-nix.homeManagerModules.sops];

  # Getting started:
  # 1. Generate new key at ~/.config/sops/age/keys.txt from private ssh key file
  #    nix run nixpkgs#ssh-to-age -- -private-key -i ~/.ssh/id_homelab > .config/sops/age/keys.txt
  # 2. Generate the public key of the upon generated key for the .sops.yaml file
  #    nix shell nixpkgs#age -c age-keygen -y ~/.config/sops/age/keys.txt
  sops.age = {
    keyFile = "${config.home.homeDirectory}/.config/sops/age/keys.txt";
    generateKey = false;
  };

  home.packages = with pkgs; [age sops];
}
