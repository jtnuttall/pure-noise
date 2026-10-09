{
  description = "pure-noise - coherent noise generation for haskell";

  inputs = {
    haskellNix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  };

  outputs = {
    self,
    nixpkgs,
    haskellNix,
    ...
  }: let
    supportedSystems = ["x86_64-linux" "aarch64-linux" "aarch64-darwin"];

    forAllSystems = nixpkgs.lib.genAttrs supportedSystems;

    mkOutputs = system: let
      pkgs = import nixpkgs {
        inherit system;
        inherit (haskellNix) config;
        overlays = [haskellNix.overlay];
      };

      tools = with pkgs; {
        benchmarkAnalysis = [
          dasel
          xq
          (python3.withPackages (p: with p; [pandas scipy numpy]))
          qsv
        ];
        build = with llvmPackages_19; [clang-unwrapped gcc llvm pkg-config];
        check = [dprint lychee odiff shellcheck typos];
        examples = [SDL2 glew libx11];
      };

      project = pkgs.haskell-nix.cabalProject' {
        src = ./.;
        compiler-nix-name = "ghc9124";

        modules = [
          {
            packages.pure-noise.components.benchmarks.pure-noise-bench.build-tools = tools.build;
            packages.pure-noise.components.benchmarks.pure-noise-fnl-bench.build-tools = tools.build;
          }
        ];

        shell = {
          withHoogle = true;
          tools = {
            cabal = "latest";
            fourmolu = "latest";
            haskell-language-server = "latest";
            hlint = "latest";
            hpack = "latest";
            tasty-discover = "latest";
          };
          nativeBuildInputs = tools.benchmarkAnalysis ++ tools.build ++ tools.check;
          buildInputs = tools.examples;
        };
      };

      # Generate the base flake outputs from haskell.nix
      flake = project.flake {};
    in {
      inherit (flake) apps;
      inherit (flake) devShells;

      formatter = pkgs.alejandra;

      packages =
        flake.packages
        // {
          default = flake.packages."pure-noise:lib:pure-noise";
        };

      checks =
        flake.checks
        // {
          fourmolu =
            pkgs.runCommand "fourmolu-check" {
              nativeBuildInputs = [(project.tool "fourmolu" "latest")];
            } ''
              cd ${./.}
              fourmolu -m check .
              touch $out
            '';

          hlint =
            pkgs.runCommand "hlint-check" {
              nativeBuildInputs = [(project.tool "hlint" "latest")];
            } ''
              cd ${./.}
              hlint --ignore-suggestions .
              touch $out
            '';

          shellcheck =
            pkgs.runCommand "shellcheck-check" {
              nativeBuildInputs = [pkgs.shellcheck];
            } ''
              cd ${./.}
              shellcheck **/*.sh
              touch $out
            '';

          dprint =
            pkgs.runCommand "dprint-check" {
              nativeBuildInputs = [pkgs.dprint];
            } ''
              cd ${./.}
              touch $out
            '';
        };
    };
  in
    {
      packages = forAllSystems (sys: (mkOutputs sys).packages);
      apps = forAllSystems (sys: (mkOutputs sys).apps);
      devShells = forAllSystems (sys: (mkOutputs sys).devShells);
      checks = forAllSystems (sys: (mkOutputs sys).checks);
      formatter = forAllSystems (sys: (mkOutputs sys).formatter);
    }
    // {
      nixConfig = {
        extra-substituters = [
          "https://cache.iog.io"
        ];
        extra-trusted-public-keys = [
          "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
        ];
        allow-import-from-derivation = true;
      };
    };
}
