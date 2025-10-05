{
  description = "Delter - File diff monitoring tool";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/master";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
      in
      {
        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            pkgsStatic.xdelta

            (haskell-language-server.override { supportedGhcVersions = ["9122"]; })
            haskell.compiler.ghc9122

            haskell-language-server

            zlib
          ];

          shellHook = ''
            echo "Development environment for Delter"
            echo "Available tools: xdelta3, ghc, cabal"
          '';
        };
      });
}
