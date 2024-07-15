{
  description = "KeyAct";

  inputs = {
    nixpkgs.url = "nixpkgs";
    typed-systems = {
      url = "github:YellowOnion/nix-typed-systems";
      flake = false;
    };
  };

  outputs = { self, typed-systems, nixpkgs }:
    let
      pkgName = "KeyAct";
      inherit (import typed-systems) id genAttrsMapBy systems';
      systems = [ systems'.x86_64-linux systems'.aarch64-linux ];

      eachSystem = genAttrsMapBy id (system:
        let pkgs = import nixpkgs {
              inherit system;
            };
        in {
          inherit system pkgs;
          haskellPkgs = pkgs.haskellPackages.override {
            overrides = self: super: {
              evdev = pkgs.haskell.lib.markUnbroken super.evdev;
            };
          };
        }) systems;
    in {
      packages = eachSystem ({ pkgs, haskellPkgs, ... }:
        let
          pkg = haskellPkgs.callCabal2nix pkgName self {};
        in {
          ${pkgName} = pkg;
          default = pkg;
        });

      devShells = eachSystem ({ pkgs, haskellPkgs, system }: {
        default = haskellPkgs.shellFor {
          withHoogle = true;
          packages = p: [ self.packages.${system}.default ];
          buildInputs = builtins.attrValues {
            inherit (pkgs) cabal-install cabal2nix;
            inherit (haskellPkgs) ghc haskell-language-server hlint;
          };
        };});

      formatter = eachSystem ({ pkgs, ... }: pkgs.nixfmt);
    };
}
