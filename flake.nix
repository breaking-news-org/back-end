{
  inputs = {
    flakes.url = "github:deemp/flakes";
    jose = {
      url = "github:frasertweedale/hs-jose";
      flake = false;
    };
  };

  outputs = inputs:
    let
      inputs_ =
        let flakes = inputs.flakes.flakes; in
        {
          inherit (flakes.source-flake) nixpkgs flake-utils formatter;
          haskell-tools = flakes.language-tools.haskell;
          inherit (flakes) codium devshell flakes-tools workflows drv-tools;
          inherit flakes;
        };

      outputs = outputs_ { } // { inputs = inputs_; outputs = outputs_; };

      outputs_ =
        inputs__:
        let inputs = inputs_ // inputs__; in

        inputs.flake-utils.lib.eachDefaultSystem
          (system:
          let
            # We're going to make some dev tools for our Haskell package
            # See NixOS wiki for more info - https://nixos.wiki/wiki/Haskell

            # First, we import stuff
            pkgs = import inputs.nixpkgs { config.allowUnfree = true; inherit system; };
            inherit (inputs.drv-tools.lib.${system}) mkShellApps mkShellApp mkBin withDescription framed mapGenAttrs;
            inherit (inputs.codium.lib.${system}) writeSettingsJSON mkCodium;
            inherit (inputs.codium.lib.${system}) extensions extensionsCommon settingsNix settingsCommonNix;
            inherit (inputs.flakes-tools.lib.${system}) mkFlakesTools;
            inherit (inputs.devshell.lib.${system}) mkCommands mkShell mkRunCommands;
            inherit (inputs.haskell-tools.lib.${system}) toolsGHC;
            inherit (inputs.workflows.lib.${system}) writeWorkflow run nixCI;

            # Next, set the desired GHC version
            ghcVersion_ = "928";

            # And the names of packages
            back = "back";
            test = "test";

            # --- shells ---

            # First of all, we need to prepare the haskellPackages attrset
            # So, we define the overrides - https://nixos.wiki/wiki/Haskell#Overrides
            # This is to supply the necessary libraries and executables to our packages
            # Sometimes, we need to fix the broken packages - https://gutier.io/post/development-fixing-broken-haskell-packages-nixpkgs/
            # That's why, inherit several helper lib
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
            override = pkgs_: {
              overrides = self: super:
                let modify = drv: pkgs.lib.pipe drv [ dontBenchmark dontCheck ]; in
                {
                  openapi3 = modify (unmarkBroken super.openapi3);
                } //
                (
                  let
                    mkPackage = name: path: deps: depsLib: overrideCabal
                      (dontCheck (dontHaddock (dontBenchmark (super.callCabal2nix name path deps))))
                      (x: {
                        # we can combine the existing deps and new deps
                        # we should write the new deps before the existing deps to override them
                        # these deps will be in haskellPackages.myPackage.getCabalDeps.librarySystemDepends
                        librarySystemDepends = depsLib ++ (x.librarySystemDepends or [ ]);

                        testHaskellDepends = [
                          super.tasty-discover
                        ] ++ (x.testHaskellDepends or [ ]);
                      });
                    backDepsLib = (__attrValues { inherit (pkgs_) zlib libpqxx; });
                    testDepsLib = backDepsLib;
                  in
                  {
                    "${back}" = mkPackage back ./${back} { inherit (self) servant-queryparam-server; } backDepsLib;
                    "${test}" = mkPackage test ./${test} { "${back}" = self."${back}"; } testDepsLib;
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
              override = override pkgs;
              packages = (ps: [
                ps.${back}
                ps.${test}
              ]);
            })
              hls cabal ghc justStaticExecutable
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

            backExeName = back;
            backExe = mkExe back backExeName;

            testExeName = test;
            testExe = mkExe test testExeName;


            # A disadvantage of this approach is that the package and all its local dependencies
            # will be rebuilt even if only that package changes
            # So, the builds aren't incremental

            # --- docker image ---
            # What if we'd like to share our Haskell app?
            # In this case, we can easily make a Docker image with it
            # We'll take an executable from the previous step and put it into an image
            # At this moment, we can set a name and a tag of our image


            mkImage = { imageName, imageTag ? "latest", package, executableName }:
              pkgs.dockerTools.buildLayeredImage {
                name = imageName;
                tag = imageTag;
                config.Cmd = [ executableName ];
                contents = [
                  package
                  # pkgs.coreutils
                  # pkgs.bashInteractive
                  # pkgs.htop
                  # pkgs.curl
                ];
              };

            mkScripts = { name, imageName, imageTag ? "latest", package, executableName }:
              let
                image = mkImage { inherit imageName imageTag package executableName; };
                env =
                  mapGenAttrs
                    (name_: { "${name_}" = "\$${name_}"; })
                    [ "DOCKER_HUB_USERNAME" "DOCKER_HUB_PASSWORD" ];

                dockerBuild = "${name}DockerBuild";
                digests = "pulumi/digests.yaml";
                scripts1 = mkShellApps {
                  "${dockerBuild}" = {
                    text = ''docker load < ${image}'';
                    runtimeInputs = [ pkgs.docker ];
                    description = "Load an image into docker";
                  };
                  "${name}WriteDigest" = rec {
                    text = let imageRef = "${env.DOCKER_HUB_USERNAME}/${imageName}:${imageTag}"; in
                      ''
                        docker pull ${imageRef}

                        touch ${digests}
                        tmp=$(mktemp)

                        printf "a: %s" $(docker inspect --format='{{index .RepoDigests 0}}' ${imageRef}) \
                          | yq e '.'"\"${name}\""' = .a | del(.a)' > $tmp
                      
                        yq eval-all -i 'select(fileIndex == 0) * select(fileIndex == 1)' ${digests} $tmp
                      '';
                    description = ''Write digest of ${name} image from Docker Hub'';
                  };
                };

                scripts2 = mkShellApps {
                  "${name}PushToDockerHub" = rec {
                    text = ''
                      ${mkBin scripts1.${dockerBuild}}
                      docker login -u ${env.DOCKER_HUB_USERNAME} -p ${env.DOCKER_HUB_PASSWORD}
                      docker tag ${imageName}:${imageTag} ${env.DOCKER_HUB_USERNAME}/${imageName}:${imageTag}
                      docker push ${env.DOCKER_HUB_USERNAME}/${imageName}:${imageTag}
                    '';
                    runtimeInputs = [ pkgs.docker ];
                    description = ''Push ${name} image to Docker Hub'';
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

            backDockerHubImageName = backImageName;
            testDockerHubImageName = testImageName;

            backImageName = "breaking-news-${back}-app";
            backLocalImageName = backImageName;
            backImageTag = "latest";

            testImageName = "breaking-news-back-test";
            testLocalImageName = testImageName;
            testImageTag = "latest";

            genOpenAPI3 = "gen-openapi3";

            scripts =
              let scripts1 =
                (mkScripts { name = back; imageName = backDockerHubImageName; package = backExe; executableName = "back"; })
                // (mkScripts { name = test; imageName = testDockerHubImageName; package = testExe; executableName = "test"; });
              in
              scripts1 // (mkShellApps {
                genOpenAPI3 = {
                  text = ''${mkExe back genOpenAPI3}/bin/${genOpenAPI3}'';
                  description = ''Generate OpenAPI3 specification for the server'';
                };
                writeDigests = {
                  text = ''
                    ${mkBin scripts.backWriteDigest}
                    ${mkBin scripts.testWriteDigest}
                  '';
                };
              }) // {
                inherit (mkFlakesTools { dirs = [ "." ]; root = ./.; }) updateLocks pushToCachix logInToCachix;
              };

            tools = [
              ghcid
              hpack
              cabal
              # ghc
              hls

              # db stuff
              pkgs.postgresql_15

              # kubernetes

              # pkgs.kubectl
              # pkgs.minikube
              # pkgs.kubernetes-helm

              # tests
              pkgs.postman

              # node
              pkgs.nodejs_18

              pkgs.deno

              # hosting
              pkgs.nodePackages.localtunnel
            ];

            extraTools = [
              pkgs.bashInteractive
            ];

            packages = {
              writeSettings = writeSettingsJSON (settingsCommonNix // { inherit (settingsNix) haskell; });
              # And compose VSCodium with dev tools and HLS
              codium = mkCodium {
                extensions = extensionsCommon // { inherit (extensions) haskell kubernetes typescript; };
              };

              writeWorkflows = import ./nix-files/workflows.nix {
                inherit (inputs) workflows;
                backDir = "back";
                name = "CI";
                inherit system;
                inherit scripts;
                inherit back test;
              };
            } // scripts;

            devShells = {
              default = mkShell {
                packages = tools ++ extraTools;
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
                      backPushToDockerHub testPushToDockerHub
                      backWriteDigest
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
    in
    outputs;

  nixConfig = {
    extra-substituters = [
      "https://nix-community.cachix.org"
      "https://cache.iog.io"
      "https://deemp.cachix.org"
    ];
    extra-trusted-public-keys = [
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      "deemp.cachix.org-1:9shDxyR2ANqEPQEEYDL/xIOnoPwxHot21L5fiZnFL18="
    ];
  };
}
