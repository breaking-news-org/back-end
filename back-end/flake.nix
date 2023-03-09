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
      inherit (drv-tools.functions.${system}) mkShellApps mkBin withDescription framed;
      inherit (my-codium.functions.${system}) writeSettingsJSON mkCodium;
      inherit (my-codium.configs.${system}) extensions settingsNix;
      inherit (flakes-tools.functions.${system}) mkFlakesTools;
      inherit (devshell.functions.${system}) mkCommands mkShell;
      inherit (haskell-tools.functions.${system}) toolsGHC;
      inherit (workflows.functions.${system}) writeWorkflow;
      inherit (workflows.configs.${system}) nixCI;

      # Next, set the desired GHC version
      ghcVersion_ = "925";

      # and the name of the package
      myPackageName = "back-end";

      # Then, we list separately the libraries that our package needs
      myPackageDepsLib = [ pkgs.zlib pkgs.libpqxx ];

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

      # --- build an executable ---
      # We can take one step further and make our app builds reproducible
      # We'll take the haskellPackages.myPackage and turn it into an executable
      # At this moment, we can set a name of our executable

      exeName = "back-end";

      # We'll also add a description
      exe =
        withDescription
          (justStaticExecutable exeName haskellPackages.myPackage)
          "Back end"
      ;
      # A disadvantage of this approach is that the package and all its local dependencies
      # will be rebuilt even if only that package changes
      # So, the builds aren't incremental

      # --- docker image ---
      # What if we'd like to share our Haskell app?
      # In this case, we can easily make a Docker image with it
      # We'll take an executable from the previous step and put it into an image
      # At this moment, we can set a name and a tag of our image

      imageName = "breaking-news-back";
      localImageName = imageName;
      imageTag = "latest";

      image = pkgs.dockerTools.buildLayeredImage {
        name = imageName;
        tag = imageTag;
        config.Entrypoint = [ "bash" "-c" exe.name ];
        contents = [ pkgs.bash exe ];
      };

      scripts =
        let
          dockerHubImageName = "breaking-news-back";
          herokuAppName = "breaking-news-back";
          host = "127.0.0.1";
          port = "8082";
          result = "result";
          tag = "latest";

          scripts1 = mkShellApps {
            dockerBuild = {
              text = ''docker load < ${image}'';
              runtimeInputs = [ pkgs.docker ];
              description = "Load an image into docker";
            };
          };

          scripts2 = mkShellApps {
            dockerRun = {
              text = ''
                ${mkBin scripts1.dockerBuild}
                docker run -p ${host}:${port}:${port} ${localImageName}:${tag}
              '';
              runtimeInputs = [ pkgs.docker ];
              description = "Run `${localImageName}` in a docker container";
            };
            herokuRelease = {
              text = ''
                ${mkBin scripts1.dockerBuild}
                docker login --username=_ --password=$(heroku auth:token) registry.heroku.com
                docker tag ${localImageName}:${tag} registry.heroku.com/${herokuAppName}/web
                docker push registry.heroku.com/${herokuAppName}/web
                heroku container:release web -a ${herokuAppName}
              '';
              runtimeInputs = [ pkgs.docker pkgs.heroku ];
              description = "Release `${herokuAppName}` on Heroku";
            };
          };
        in
        scripts1 // scripts2;

      # The image is ready. We can run it in a devshell
      dockerShell = mkShell {
        packages = [ pkgs.docker ];
        bash.extra = ''
          docker load < ${image}
          ${framed "docker run -it ${localImageName}:${imageTag}"}
        '';
        commands = mkCommands "tools" [ pkgs.docker ];
      };

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

      tools = codiumTools;
    in
    {
      packages = {
        inherit
          writeSettings
          codium;
      } // scripts;

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
              name = "nix run .#${scripts.dockerRun.pname}";
              category = "infra";
              help = scripts.dockerRun.meta.description;
            }
            {
              name = "nix run .#${scripts.herokuRelease.pname}";
              category = "infra";
              help = scripts.herokuRelease.meta.description;
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
