{
  description = "Plutus Contract API";

  inputs = {

    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    haskell-nix.url = "github:input-output-hk/haskell.nix";
    haskell-nix-extra-hackage = {
      url = "github:mlabs-haskell/haskell-nix-extra-hackage";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.haskell-nix.follows = "haskell-nix";
    };

    pre-commit-hooks = {
      url = "github:cachix/pre-commit-hooks.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    iohk-nix = {
      url = "github:input-output-hk/iohk-nix";
      flake = false;
    };

    optparse-applicative = {
      url =
        "github:input-output-hk/optparse-applicative/7497a29cb998721a9068d5725d49461f2bba0e7a";
      flake = false;
    };

    plutus = {
      url = "github:input-output-hk/plutus/a56c96598b4b25c9e28215214d25189331087244";
      flake = false;
    };

    ouroboros-network = {
      url =
        "github:input-output-hk/ouroboros-network/cb9eba406ceb2df338d8384b35c8addfe2067201";
      flake = false;
    };

    cardano-base = {
      url =
        "github:input-output-hk/cardano-base/0f3a867493059e650cda69e20a5cbf1ace289a57";
      flake = false;
    };

    cardano-prelude = {
      url =
        "github:input-output-hk/cardano-prelude/bb4ed71ba8e587f672d06edf9d2e376f4b055555";
      flake = false;
    };

    cardano-crypto = {
      url =
        "github:input-output-hk/cardano-crypto/f73079303f663e028288f9f4a9e08bcca39a923e";
      flake = false;
    };

    cardano-ledger = {
      url =
        "github:input-output-hk/cardano-ledger/c7c63dabdb215ebdaed8b63274965966f2bf408f";
      flake = false;
    };

    Win32-network = {
      url =
        "github:input-output-hk/Win32-network/3825d3abf75f83f406c1f7161883c438dac7277d";
      flake = false;
    };


    iohk-monitoring-framework = {
      url =
        "github:input-output-hk/iohk-monitoring-framework/066f7002aac5a0efc20e49643fea45454f226caa";
      flake = false;
    };


    cardano-node = {
      url =
        "github:input-output-hk/cardano-node?ref=1.35.3-rc1";
      flake = false;
    };

    goblins = {
      url =
        "github:input-output-hk/goblins/cde90a2b27f79187ca8310b6549331e59595e7ba";
      flake = false;
    };

    io-sim = {
      url =
        "github:input-output-hk/io-sim/57e888b1894829056cb00b7b5785fdf6a74c3271";
      flake = false;
    };

    typed-protocols = {
      url =
        "github:input-output-hk/typed-protocols/181601bc3d9e9d21a671ce01e0b481348b3ca104";
      flake = false;
    };

    ekg-forward = {
      url = "github:input-output-hk/ekg-forward/297cd9db5074339a2fb2e5ae7d0780debb670c63";
      flake = false;
    };
    ekg-json = {
      url = "github:vshabanov/ekg-json/00ebe7211c981686e65730b7144fbf5350462608";
      flake = false;
    };

    flat = {
      url =
        "github:Quid2/flat/ee59880f47ab835dbd73bea0847dab7869fc20d8";
      flake = false;
    };
  };


  outputs =
    inputs@{ self
    , nixpkgs
    , haskell-nix
    , haskell-nix-extra-hackage
    , iohk-nix
    , pre-commit-hooks
    , ...
    }:
    let
      plainNixpkgsFor = system: import nixpkgs { inherit system; };
      nixpkgsFor = system: import nixpkgs {
        inherit system;
        overlays = [ haskell-nix.overlay (import "${iohk-nix}/overlays/crypto") ];
      };

      supportedSystems = [ "x86_64-linux" "x86_64-darwin" ];
      perSystem = nixpkgs.lib.genAttrs supportedSystems;

      fourmoluFor = system: (plainNixpkgsFor system).haskellPackages.fourmolu;
      hlintFor = system: (plainNixpkgsFor system).haskellPackages.hlint_3_4_1;


      preCommitCheckFor = system:
        pre-commit-hooks.lib.${system}.run
          {
            src = ./.;

            settings = {
              ormolu.defaultExtensions = [
                "ImportQualifiedPost"
              ];
            };

            hooks = {
              cabal-fmt.enable = true;
              fourmolu.enable = true;
              nixpkgs-fmt.enable = true;
              shellcheck.enable = true;
              statix.enable = true;
              hlint.enable = false;
              markdownlint.enable = false;
            };

            tools = {
              fourmolu = fourmoluFor system;
              hlint = hlintFor system;
            };
          };

      ghcVersion = "8107";
      compiler-nix-name = "ghc" + ghcVersion;

      projectFor = system:
        let
          pkgs = nixpkgsFor system;
          plainPkgs = plainNixpkgsFor system;

          hls = pkgs.haskell-language-server.override { supportedGhcVersions = [ ghcVersion ]; };

          hackages = haskell-nix-extra-hackage.mkHackagesFor system compiler-nix-name
            [
              "${inputs.plutus}/plutus-core"
              "${inputs.plutus}/plutus-ledger-api"
              "${inputs.plutus}/plutus-tx"
              "${inputs.plutus}/plutus-tx-plugin"
              "${inputs.plutus}/prettyprinter-configurable"
              "${inputs.plutus}/stubs/plutus-ghc-stub"
              "${inputs.plutus}/word-array"

              "${inputs.ouroboros-network}/monoidal-synchronisation"
              "${inputs.ouroboros-network}/network-mux"
              "${inputs.ouroboros-network}/ntp-client"
              "${inputs.ouroboros-network}/ouroboros-consensus"
              "${inputs.ouroboros-network}/ouroboros-consensus-byron"
              "${inputs.ouroboros-network}/ouroboros-consensus-cardano"
              "${inputs.ouroboros-network}/ouroboros-consensus-protocol"
              "${inputs.ouroboros-network}/ouroboros-consensus-shelley"
              "${inputs.ouroboros-network}/ouroboros-network"
              "${inputs.ouroboros-network}/ouroboros-network-framework"
              "${inputs.ouroboros-network}/ouroboros-network-testing"

              "${inputs.cardano-base}/base-deriving-via"
              "${inputs.cardano-base}/binary"
              "${inputs.cardano-base}/binary/test"
              "${inputs.cardano-base}/cardano-crypto-class"
              "${inputs.cardano-base}/cardano-crypto-praos"
              "${inputs.cardano-base}/cardano-crypto-tests"
              "${inputs.cardano-base}/measures"
              "${inputs.cardano-base}/orphans-deriving-via"
              "${inputs.cardano-base}/slotting"
              "${inputs.cardano-base}/strict-containers"

              "${inputs.cardano-prelude}/cardano-prelude"
              "${inputs.cardano-prelude}/cardano-prelude-test"


              "${inputs.cardano-crypto}"


              "${inputs.cardano-ledger}/eras/alonzo/impl"
              "${inputs.cardano-ledger}/eras/alonzo/test-suite"
              "${inputs.cardano-ledger}/eras/babbage/impl"
              "${inputs.cardano-ledger}/eras/byron/chain/executable-spec"
              "${inputs.cardano-ledger}/eras/byron/crypto"
              "${inputs.cardano-ledger}/eras/byron/crypto/test"
              "${inputs.cardano-ledger}/eras/byron/ledger/executable-spec"
              "${inputs.cardano-ledger}/eras/byron/ledger/impl"
              "${inputs.cardano-ledger}/eras/byron/ledger/impl/test"
              "${inputs.cardano-ledger}/eras/shelley-ma/impl"
              "${inputs.cardano-ledger}/eras/shelley-ma/test-suite"
              "${inputs.cardano-ledger}/eras/shelley/impl"
              "${inputs.cardano-ledger}/eras/shelley/test-suite"
              "${inputs.cardano-ledger}/libs/cardano-data"
              "${inputs.cardano-ledger}/libs/cardano-ledger-core"
              "${inputs.cardano-ledger}/libs/cardano-ledger-pretty"
              "${inputs.cardano-ledger}/libs/cardano-protocol-tpraos"
              "${inputs.cardano-ledger}/libs/non-integral"
              "${inputs.cardano-ledger}/libs/set-algebra"
              "${inputs.cardano-ledger}/libs/small-steps"
              "${inputs.cardano-ledger}/libs/small-steps-test"
              "${inputs.cardano-ledger}/libs/vector-map"

              "${inputs.Win32-network}"

              "${inputs.iohk-monitoring-framework}/contra-tracer"
              "${inputs.iohk-monitoring-framework}/iohk-monitoring"
              "${inputs.iohk-monitoring-framework}/tracer-transformers"
              "${inputs.iohk-monitoring-framework}/plugins/backend-ekg"
              "${inputs.iohk-monitoring-framework}/plugins/backend-aggregation"
              "${inputs.iohk-monitoring-framework}/plugins/backend-monitoring"
              "${inputs.iohk-monitoring-framework}/plugins/backend-trace-forwarder"

              "${inputs.cardano-node}/cardano-api"
              "${inputs.cardano-node}/cardano-cli"
              "${inputs.cardano-node}/cardano-git-rev"
              "${inputs.cardano-node}/cardano-node"
              "${inputs.cardano-node}/trace-dispatcher"
              "${inputs.cardano-node}/trace-forward"
              "${inputs.cardano-node}/trace-resources"

              "${inputs.goblins}"

              "${inputs.io-sim}/io-classes"
              "${inputs.io-sim}/io-sim"
              "${inputs.io-sim}/strict-stm"

              "${inputs.typed-protocols}/typed-protocols"
              "${inputs.typed-protocols}/typed-protocols-cborg"
              "${inputs.typed-protocols}/typed-protocols-examples"

              "${inputs.ekg-forward}"

              "${inputs.ekg-json}"

              "${inputs.optparse-applicative}"

              "${inputs.flat}"
            ];

          moduleFixes = [ ];
          /*
          moduleFixes = [
            ( {pkgs, ...}:
            {
              packages = {
                cardano-crypto-praos.components.library.pkgconfig = pkgs.lib.mkForce [ [ pkgs.libsodium-vrf ] ];
                cardano-crypto-class.components.library.pkgconfig = pkgs.lib.mkForce [ [ pkgs.libsodium-vrf ] ];
              };
            }
            )
          ];
          */

        in
        pkgs.haskell-nix.cabalProject' {
          src = ./.;
          index-state = "2022-02-22T20:47:03Z";
          inherit compiler-nix-name;
          inherit (hackages) extra-hackages extra-hackage-tarballs;
          modules = moduleFixes ++ hackages.modules;

          cabalProjectLocal = ''
            allow-newer: ekg:aeson, *:hedgehog
            constraints: aeson >= 2, hedgehog >= 1.1
          '';

          shell = {
            inherit (preCommitCheckFor system) shellHook;
            withHoogle = true;
            exactDeps = true;

            nativeBuildInputs = [
              plainPkgs.cabal-install
              plainPkgs.fd
              plainPkgs.haskellPackages.apply-refact
              plainPkgs.haskellPackages.cabal-fmt
              plainPkgs.nixpkgs-fmt

              (fourmoluFor system)
              (hlintFor system)
              hls
            ];
          };
        };
    in
    {
      inherit plainNixpkgsFor;


      project = perSystem projectFor;
      flake = perSystem (system: self.project.${system}.flake { });

      packages = perSystem (system:
        self.flake.${system}.packages
      );

      devShells = perSystem (system: {
        default = self.flake.${system}.devShell;
        tooling =
          let
            pkgs = plainNixpkgsFor system;
          in
          pkgs.mkShell {
            inherit (preCommitCheckFor system) shellHook;
            nativeBuildInputs = [
              pkgs.cabal-install
              pkgs.fd
              pkgs.haskellPackages.apply-refact
              pkgs.haskellPackages.cabal-fmt
              pkgs.nixpkgs-fmt
              (hlintFor system)
              (fourmoluFor system)
            ];
          };
      });

      checks = perSystem (system:
        self.flake.${system}.checks
        // { formatCheck = preCommitCheckFor system; }
      );

      hydraJobs = {
        inherit (self) checks packages devShells;
      };
    };
}


