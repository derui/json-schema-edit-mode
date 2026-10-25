{
  description = "json-schema-edit-mode development environment";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    {
      nixpkgs,
      flake-utils,
      ...
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        # Provides lib/libtree-sitter-json.so in the layout Emacs expects.
        grammars = pkgs.emacs.pkgs.treesit-grammars.with-grammars (p: [ p.tree-sitter-json ]);
      in
      {
        devShells.default = pkgs.mkShell {
          packages = [
            pkgs.emacs
            pkgs.go-task
          ];
          TREESIT_GRAMMAR_DIR = "${grammars}/lib";
        };
      }
    );
}
