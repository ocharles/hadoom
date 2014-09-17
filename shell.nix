let
  pkgs = import <nixpkgs> {};
  haskellPackages = pkgs.haskellPackages.override {
    extension = self: super: {
      hadoom = self.callPackage ./. {};
       };
       };

       in pkgs.lib.overrideDerivation haskellPackages.hadoom (attrs: {
       buildInputs = [ haskellPackages.cabalInstall ] ++ attrs.buildInputs;
       })