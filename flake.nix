{
  inputs = {
    nixpkgs_.url = "github:deemp/flakes?dir=source-flake/nixpkgs";
    nixpkgs.follows = "nixpkgs_/nixpkgs";
    codium.url = "github:deemp/flakes?dir=codium";
    drv-tools.url = "github:deemp/flakes?dir=drv-tools";
    flake-utils_.url = "github:deemp/flakes?dir=source-flake/flake-utils";
    flake-utils.follows = "flake-utils_/flake-utils";
    haskell-tools.url = "github:deemp/flakes?dir=language-tools/haskell";
    devshell.url = "github:deemp/flakes?dir=devshell";
    flakes-tools.url = "github:deemp/flakes?dir=flakes-tools";
    workflows.url = "github:deemp/flakes?dir=workflows";
    dream2nix.url = "github:nix-community/dream2nix";
    jose = {
      url = "github:frasertweedale/hs-jose";
      flake = false;
    };
    servant = {
      url = "github:deemp/servant";
      flake = false;
    };
  };
  outputs = inputs: inputs.flake-utils.lib.eachDefaultSystem
    (system:
      let
        # We're going to make some dev tools for our Haskell package
        # See NixOS wiki for more info - https://nixos.wiki/wiki/Haskell

        # First, we import stuff
        pkgs = import inputs.nixpkgs { config.allowUnfree = true; inherit system; };
        inherit (inputs.drv-tools.functions.${system}) mkShellApps mkShellApp mkBin withDescription framed mapGenAttrs;
        inherit (inputs.codium.functions.${system}) writeSettingsJSON mkCodium;
        inherit (inputs.codium.configs.${system}) extensions settingsNix;
        inherit (inputs.flakes-tools.functions.${system}) mkFlakesTools;
        inherit (inputs.devshell.functions.${system}) mkCommands mkShell mkRunCommands;
        inherit (inputs.haskell-tools.functions.${system}) toolsGHC;
        inherit (inputs.workflows.functions.${system}) writeWorkflow run;
        inherit (inputs.workflows.configs.${system}) nixCI;

        # Next, set the desired GHC version
        ghcVersion_ = "927";

        # and the name of the package
        appPackageName = "back";

        # and the name of the package
        testPackageName = "test";

        # Then, we list separately the libraries that our package needs
        appPackageDepsLib = [ pkgs.zlib pkgs.libpqxx ];
        testPackageDepsLib = appPackageDepsLib;

        # And the binaries. 
        # In our case, the Haskell app will call the `hello` command
        appPackageDepsBin = [ ];
        testPackageDepsBin = [ ];

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
          # dontCheck - skip test
          dontCheck
          dontHaddock
          dontBenchmark
          # override deps of a package
          # see what can be overriden - https://github.com/NixOS/nixpkgs/blob/0ba44a03f620806a2558a699dba143e6cf9858db/pkgs/development/haskell-modules/generic-builder.nix#L13
          overrideCabal
          unmarkBroken
          ;

        # Here's our override
        # We should use `cabal v1-*` commands with it - https://github.com/NixOS/nixpkgs/issues/130556#issuecomment-1114239002
        override = {
          overrides = self: super:
            let modify = drv: pkgs.lib.pipe drv [ dontBenchmark dontCheck ]; in
            {
              lzma = modify super.lzma;
              openapi3 = modify (unmarkBroken super.openapi3);

              servant-queryparam-core = (super.callCabal2nix "servant-queryparam-core" "${inputs.servant.outPath}/servant-queryparam/servant-queryparam-core" { });
              servant-queryparam-server = (super.callCabal2nix "servant-queryparam-server" "${inputs.servant.outPath}/servant-queryparam/servant-queryparam-server" {
                inherit (self) servant-queryparam-core;
              });
              servant-queryparam-client = (super.callCabal2nix "servant-queryparam-client" "${inputs.servant.outPath}/servant-queryparam/servant-queryparam-client" {
                inherit (self) servant-queryparam-core;
              });
            } //
            (
              let mkPackage = name: path: deps: depsLib: depsBin: overrideCabal
                (dontCheck (dontHaddock (dontBenchmark (super.callCabal2nix appPackageName path deps))))
                (x: {
                  # we can combine the existing deps and new deps
                  # we should write the new deps before the existing deps to override them
                  # these deps will be in haskellPackages.myPackage.getCabalDeps.librarySystemDepends
                  librarySystemDepends = depsLib ++ (x.librarySystemDepends or [ ]);
                  executableSystemDepends = depsBin ++ (x.executableSystemDepends or [ ]);

                  libraryHaskellDepends = [
                  ] ++ (x.libraryHaskellDepends or [ ]);

                  testHaskellDepends = [
                    super.tasty-discover
                  ] ++ (x.testHaskellDepends or [ ]);
                }); in
              {
                "${appPackageName}" = mkPackage appPackageName ./back { } appPackageDepsLib appPackageDepsBin;
                "${testPackageName}" = mkPackage testPackageName ./test { "${appPackageName}" = self."${appPackageName}"; } testPackageDepsLib testPackageDepsBin;
              }
            );
        };


        # We supply it to a helper function that will give us haskell tools for given 
        # compiler version, override, packages we're going to develop, and their binary runtime dependencies

        # Our devShells should only be aware of the dev dependencies of the Haskell packages that we're going to develop
        # So, we need to supply all Haskell packages that we'd like to develop so that they're excluded from the dev dependencies
        # More specifically, if we're developing Haskell packages A and B and A depends on B, we need to supply both A and B
        # This will prevent nix from building B as a dev dependency of A

        inherit (toolsGHC {
          version = ghcVersion_;
          inherit override;
          packages = (ps: [
            ps.${appPackageName}
            ps.${testPackageName}
          ]);
          runtimeDependencies = appPackageDepsBin ++ testPackageDepsBin;
        })
          hls cabal implicit-hie justStaticExecutable
          ghcid callCabal2nix haskellPackages hpack;

        # --- build an executable ---
        # We can take one step further and make our app builds reproducible
        # We'll take the haskellPackages.myPackage and turn it into an executable
        # At this moment, we can set a name of our executable

        mkExe = packageName: exeName:
          justStaticExecutable {
            package = dontCheck (dontHaddock (dontBenchmark haskellPackages.${packageName}));
            executableName = exeName;
          };

        appExeName = "back";
        appExe = mkExe appPackageName appExeName;

        testExeName = "test";
        testExe = mkExe testPackageName testExeName;

        # A disadvantage of this approach is that the package and all its local dependencies
        # will be rebuilt even if only that package changes
        # So, the builds aren't incremental

        # --- docker image ---
        # What if we'd like to share our Haskell app?
        # In this case, we can easily make a Docker image with it
        # We'll take an executable from the previous step and put it into an image
        # At this moment, we can set a name and a tag of our image


        mkImage = { imageName, imageTag, exeName, exe }:
          pkgs.dockerTools.buildLayeredImage {
            name = imageName;
            tag = imageTag;
            config.Cmd = [ exeName ];
            contents = [
              exe
            ];
          };

        mkScripts = { name, imageName, imageTag, exeName, exe }:
          let
            image = mkImage { inherit imageName imageTag exeName exe; };
            env =
              mapGenAttrs
                (name_: { "${name_}" = "\$${name_}"; })
                [ "DOCKER_HUB_USERNAME" "DOCKER_HUB_PASSWORD" ];

            scripts1 = mkShellApps {
              "${name}DockerBuild" = {
                text = ''docker load < ${image}'';
                runtimeInputs = [ pkgs.docker ];
                description = "Load an image into docker";
              };
            };

            scripts2 = mkShellApps {
              "${name}PushToDockerHub" = rec {
                text = ''
                  ${mkBin scripts1."${name}DockerBuild"}
                  docker login -u ${env.DOCKER_HUB_USERNAME} -p ${env.DOCKER_HUB_PASSWORD}
                  docker tag ${imageName}:${imageTag} ${env.DOCKER_HUB_USERNAME}/${imageName}:${imageTag}
                  docker push ${env.DOCKER_HUB_USERNAME}/${imageName}:${imageTag}
                '';
                description = ''Push image to Docker Hub'';
                longDescription = ''
                  ${description}

                  Expected env variables:
                  - ${env.DOCKER_HUB_USERNAME}
                  - ${env.DOCKER_HUB_PASSWORD}
                '';
              };
            };
          in
          scripts1 // scripts2;

        appDockerHubImageName = appImageName;
        testDockerHubImageName = testImageName;

        appImageName = "breaking-news-back-app";
        appLocalImageName = appImageName;
        appImageTag = "latest";

        testImageName = "breaking-news-back-test";
        testLocalImageName = testImageName;
        testImageTag = "latest";

        genOpenApi3 = "gen-openapi3";

        scripts =
          (mkScripts { name = "app"; imageName = appDockerHubImageName; imageTag = "latest"; exeName = "back"; exe = appExe; })
          // (mkScripts { name = "test"; imageName = testDockerHubImageName; imageTag = "latest"; exeName = "test"; exe = testExe; })
          // (mkShellApps {
            genOpenApi3 = {
              text = ''${mkExe appPackageName genOpenApi3}/bin/${genOpenApi3}'';
              description = ''Generate OpenAPI3 specification for the server'';
            };
          });

        tools = [
          ghcid
          hpack
          implicit-hie
          cabal
          hls

          # db stuff
          pkgs.postgresql_15

          # kubernetes
          pkgs.kubectl

          pkgs.minikube

          # tests
          pkgs.postman

          # node
          pkgs.nodejs-16_x
        ];

        extraTools = [
          pkgs.bashInteractive
        ];

        packages = {
          writeSettings = writeSettingsJSON {
            inherit (settingsNix)
              haskell todo-tree files editor gitlens
              git nix-ide workbench markdown-all-in-one markdown-language-features
              ;
            extra = settingsNix.python // {
              "python.defaultInterpreterPath" = ''''${workspaceFolder}/pulumi/venv/bin/python'';
            };
          };
          # And compose VSCodium with dev tools and HLS
          codium = mkCodium {
            extensions = { inherit (extensions) nix haskell misc github markdown kubernetes python typescript; };
            runtimeDependencies = tools;
          };
          inherit (mkFlakesTools [ "." ]) updateLocks pushToCachix;

          writeWorkflows = import ./nix-files/workflows.nix {
            inherit (inputs) workflows;
            backDir = "back";
            name = "CI";
            inherit system;
            inherit scripts;
          };
        } // scripts;

        devShells = {
          default = mkShell {
            packages = tools ++ extraTools;
            bash.extra = ''
              export LANG="C.utf8"

              export CONFIG_FILE="$PWD/local/config.yaml"
              export TEST_CONFIG_FILE="$PWD/local/test.yaml"

              export JWK_FILE="$PWD/local/jwk.json"
            '';
            commands =
              (mkCommands "tools" tools)
              # ++ [
              #   {
              #     name = "pulumi";
              #     command = ''LD_LIBRARY_PATH=${pkgs.lib.makeLibraryPath [ pkgs-22-11.glibc.out ]} ${pkgs-22-11.pulumi}/bin/pulumi "$@"'';
              #     help = "pulumi";
              #     category = "tools";
              #   }
              # ]
              ++ (mkRunCommands "scripts" scripts)
              ++ (mkRunCommands "ide" { inherit (packages) writeSettings; "codium ." = packages.codium; })
              ++ (mkRunCommands "infra" {
                inherit (packages)
                  appPushToDockerHub testPushToDockerHub
                  writeWorkflows updateLocks pushToCachix;
              })
            ;
          };
        };
      in
      {
        inherit packages devShells;
      }
    )
  ;

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
