{
  description = "pure-noise - coherent noise generation for haskell";

  nixConfig = {
    extra-substituters = [
      "https://cache.iog.io"
    ];
    extra-trusted-public-keys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
    ];
    allow-import-from-derivation = true;
  };

  inputs = {
    haskellNix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = {
    nixpkgs,
    flake-utils,
    haskellNix,
    ...
  }:
    flake-utils.lib.eachSystem ["x86_64-linux"] (system: let
      pkgs = import nixpkgs {
        inherit system;
        inherit (haskellNix) config;
        overlays = [
          haskellNix.overlay
        ];
      };

      llvmTools = with pkgs.llvmPackages_19; [llvm clang-unwrapped];

      project = pkgs.haskell-nix.cabalProject' {
        src = ./.;
        compiler-nix-name = "ghc9122";
        evalSystem = "x86_64-linux";

        modules = [
          {
            packages.pure-noise.components.benchmarks.pure-noise-bench.build-tools = [pkgs.llvmPackages_19.llvm];
            packages.pure-noise.components.benchmarks.pure-noise-fnl-bench.build-tools = [pkgs.llvmPackages_19.llvm];
          }
        ];

        shell = {
          withHoogle = true;
          tools = {
            cabal = "latest";
            haskell-language-server = "latest";
            fourmolu = "latest";
            hpack = "latest";
            tasty-discover = "latest";
            hlint = "latest";
          };
          buildInputs =
            llvmTools
            ++ (with pkgs; [
              qsv
              bc
              cmake
              gcc
              pkg-config
              SDL2
              glew
              libx11
              ast-grep
              git
              gawk
              fd
              tree-sitter
              shellcheck
              bat
              coreutils
              jq
              diffutils
              dyff
              xq
              dasel
              xmlstarlet
              ripgrep
              odiff
              (python3.withPackages (python-packages: [
                python-packages.pandas
                python-packages.scipy
                python-packages.numpy
              ]))
              typos
            ]);
        };
      };

      flake = project.flake {};
    in
      flake
      // {
        packages =
          flake.packages
          // {
            default = flake.packages."pure-noise:lib:pure-noise";
          };
      });
}
