{
  description = "Development environment for Chester";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
      in
      {
        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            graalvm-ce
            nodejs_23
            git
            python313
            clang
            pnpm
            zsh
          ];

          shellHook = ''
            export SHELL=$(which zsh)
            echo "Chester development environment loaded!"
          '';
        };
      }
    );
}