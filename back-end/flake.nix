{
  inputs = {
    nixpkgs_.url = "github:deemp/flakes?dir=source-flake/nixpkgs";
    nixpkgs.follows = "nixpkgs_/nixpkgs";
    my-codium.url = "github:deemp/flakes?dir=codium";
    drv-tools.url = "github:deemp/flakes?dir=drv-tools";
    flake-utils_.url = "github:deemp/flakes?dir=source-flake/flake-utils";
    flake-utils.follows = "flake-utils_/flake-utils";
    haskell-tools.url = "github:deemp/flakes?dir=language-tools/haskell";
    devshell.url = "github:deemp/flakes?dir=devshell";
    flakes-tools.url = "github:deemp/flakes?dir=flakes-tools";
    workflows.url = "github:deemp/flakes?dir=workflows";
    lima.url = "github:deemp/flakes?dir=lima";
  };
  outputs =
    { self
    , flake-utils
    , flakes-tools
    , nixpkgs
    , my-codium
    , drv-tools
    , haskell-tools
    , devshell
    , workflows
    , lima
    , ...
    }:
    flake-utils.lib.eachDefaultSystem (system:
    let
      # We're going to make some dev tools for our Haskell package
      # See NixOS wiki for more info - https://nixos.wiki/wiki/Haskell

      # First, we import stuff
      pkgs = nixpkgs.legacyPackages.${system};
      inherit (my-codium.functions.${system}) writeSettingsJSON mkCodium;
      inherit (my-codium.configs.${system}) extensions settingsNix;
      inherit (flakes-tools.functions.${system}) mkFlakesTools;
      inherit (devshell.functions.${system}) mkCommands mkShell;
      inherit (haskell-tools.functions.${system}) toolsGHC;
      inherit (workflows.functions.${system}) writeWorkflow;
      inherit (workflows.configs.${system}) nixCI;

      # Next, set the desired GHC version
      ghcVersion_ = "92";

      # and the name of the package
      myPackageName = "nix-managed";

      # Then, we list separately the libraries that our package needs
      myPackageDepsLib = [ ];

      # And the binaries. 
      # In our case, the Haskell app will call the `hello` command
      myPackageDepsBin = [ ];

      # --- shells ---

      # First of all, we need to prepare the haskellPackages attrset
      # So, we define the overrides - https://nixos.wiki/wiki/Haskell#Overrides
      # This is to supply the necessary libraries and executables to our packages
      # Sometimes, we need to fix the broken packages - https://gutier.io/post/development-fixing-broken-haskell-packages-nixpkgs/
      # That's why, inherit several helper functions
      # Note that overriding the packages from haskellPackages will require their rebuilds
      # So, override as few packages as possible and consider making a PR when haskellPackages.somePackage doesn't build

      inherit (pkgs.haskell.lib)
        # doJailbreak - remove package bounds from build-depends of a package
        doJailbreak
        # dontCheck - skip tests
        dontCheck
        # override deps of a package
        # see what can be overriden - https://github.com/NixOS/nixpkgs/blob/0ba44a03f620806a2558a699dba143e6cf9858db/pkgs/development/haskell-modules/generic-builder.nix#L13
        overrideCabal
        ;

      # Here's our override
      # We should use `cabal v1-*` commands with it - https://github.com/NixOS/nixpkgs/issues/130556#issuecomment-1114239002
      override = {
        overrides = self: super: {
          lzma = dontCheck (doJailbreak super.lzma);
          myPackage = overrideCabal
            (super.callCabal2nix myPackageName ./. { })
            (x: {
              # we can combine the existing deps and new deps
              # we should write the new deps before the existing deps to override them
              # these deps will be in haskellPackages.myPackage.getCabalDeps.librarySystemDepends
              librarySystemDepends = myPackageDepsLib ++ (x.librarySystemDepends or [ ]);
              # we may skip the old deps if we'd like to
              executableSystemDepends = myPackageDepsBin;
              # here's how we can add a package built from sources
              # then, we may use this package in .cabal in a test-suite
              testHaskellDepends = [
                (super.callCabal2nix "lima" "${lima.outPath}/lima" { })
              ] ++ x.testHaskellDepends;
            });
        };
      };


      # We supply it to a helper function that will give us haskell tools for given 
      # compiler version, override, packages we're going to develop, and their binary runtime dependencies

      # Our devShells should only be aware of the dev dependencies of the Haskell packages that we're going to develop
      # So, we need to supply all Haskell packages that we'd like to develop so that they're excluded from the dev dependencies
      # More specifically, if we're developing Haskell packages A and B and A depends on B, we need to supply both A and B
      # This will prevent nix from building B as a dev dependency of A

      inherit (toolsGHC ghcVersion_ override (ps: [ ps.myPackage ]) myPackageDepsBin)
        hls cabal implicit-hie justStaticExecutable
        ghcid callCabal2nix haskellPackages hpack;

      codiumTools = [
        ghcid
        hpack
        implicit-hie
        cabal
        hls
      ];

      # And compose VSCodium with dev tools and HLS
      codium = mkCodium {
        extensions = { inherit (extensions) nix haskell misc github markdown; };
        runtimeDependencies = codiumTools;
      };

      # a script to write .vscode/settings.json
      writeSettings = writeSettingsJSON {
        inherit (settingsNix) haskell todo-tree files editor gitlens
          git nix-ide workbench markdown-all-in-one markdown-language-features;
      };

      tools = codiumTools ++ [ codium ];

      # --- flakes tools ---
      # Also, we provide scripts that can be used in CI
      flakesTools = mkFlakesTools [ "." ];

      # write .github/ci.yaml to get a GitHub Actions workflow file
      writeWorkflows = writeWorkflow "ci" nixCI;
    in
    {
      packages = {
        inherit (flakesTools)
          updateLocks
          pushToCachix;
        inherit
          writeSettings
          writeWorkflows
          codium;
      };

      devShells = {
        default = mkShell {
          packages = tools;
          # sometimes necessary for programs that work with files
          bash.extra = "export LANG=C.utf8";
          commands = mkCommands "tools" tools ++ [
            {
              name = "nix run .#codium .";
              category = "ide";
              help = "Run " + codium.meta.description + " in the current directory";
            }
            {
              name = "nix run .#writeSettings";
              category = "ide";
              help = writeSettings.meta.description;
            }
            {
              name = "nix run .#writeWorkflows";
              category = "infra";
              help = writeWorkflows.meta.description;
            }
            {
              name = "nix run .#updateLocks";
              category = "infra";
              help = flakesTools.updateLocks.meta.description;
            }
            {
              name = "nix run .#pushToCachix";
              category = "infra";
              help = flakesTools.pushToCachix.meta.description;
            }
          ];
        };
      };
    });

  nixConfig = {
    extra-substituters = [
      "https://haskell-language-server.cachix.org"
      "https://nix-community.cachix.org"
      "https://cache.iog.io"
      "https://deemp.cachix.org"
    ];
    extra-trusted-public-keys = [
      "haskell-language-server.cachix.org-1:juFfHrwkOxqIOZShtC4YC1uT1bBcq2RSvC7OMKx0Nz8="
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      "deemp.cachix.org-1:9shDxyR2ANqEPQEEYDL/xIOnoPwxHot21L5fiZnFL18="
    ];
  };
}
