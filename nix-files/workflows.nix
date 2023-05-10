{ workflows, backDir, name, system, back, test, scripts }:
let
  inherit (workflows.functions.${system}) writeWorkflow expr mkAccessors genAttrsId run;
  inherit (workflows.configs.${system}) steps os oss;
  job1 = "_1_push_to_cachix";
  job2 = "_2_push_to_docker_hub";
  job3 = "_3_extra_commit";
  job4 = "_4_build_deploy_gh_pages";
  names = mkAccessors {
    matrix.os = "";
    matrix.scriptName = genAttrsId [
      "scriptName"
    ];
    secrets = genAttrsId [
      "GITHUB_TOKEN"
      "DOCKER_HUB_USERNAME"
      "DOCKER_HUB_PASSWORD"
    ];
  };
  steps_ = {
    dockerHubLogin = {
      # not sure it's necessary
      name = "Log in to Docker";
      uses = "docker/login-action@v2.1.0";
      "with" = {
        username = expr names.secrets.DOCKER_HUB_USERNAME;
        password = expr names.secrets.DOCKER_HUB_PASSWORD;
      };
    };
  };

  on = {
    # https://crontab.guru/#30_5,17_*_*_*
    schedule = [
      { cron = "0 0 * * *"; }
    ];
    pull_request = { };
    push = { };
    workflow_dispatch = { };
  };

  workflow = {
    name = "CI";
    inherit on;
    # Sets permissions of the GITHUB_TOKEN to allow deployment to GitHub Pages
    permissions = {
      contents = "write";
      pages = "write";
      id-token = "write";
    };
    jobs = {
      "${job1}" = {
        name = "Push to cachix";
        strategy.matrix.os = oss;
        runs-on = expr names.matrix.os;
        steps = [
          steps.checkout
          steps.installNix
          steps.logInToCachix
          steps.pushFlakesToCachix
          steps.cacheNix
        ];
      };
      "${job2}" = {
        name = "Push ${expr names.matrix.scriptName} to Docker Hub";
        runs-on = os.ubuntu-20;
        strategy = {
          matrix = {
            scriptName = [ back test ];
          };
        };
        steps = [
          steps.checkout
          steps.installNix
          steps_.dockerHubLogin
          {
            name = "Push to Docker Hub";
            env = {
              DOCKER_HUB_USERNAME = expr names.secrets.DOCKER_HUB_USERNAME;
              DOCKER_HUB_PASSWORD = expr names.secrets.DOCKER_HUB_PASSWORD;
            };
            working-directory = backDir;
            run = ''
              nix run .#${expr names.matrix.scriptName}PushToDockerHub
            '';
          }
        ];
      };
      "${job3}" = {
        name = "Extra commit";
        needs = [ job2 ];
        runs-on = os.ubuntu-20;
        steps = [
          steps.checkout
          steps.installNix
          steps.configGitAsGHActions
          {
            name = "Update flake locks";
            run = ''nix run .#${scripts.updateLocks.pname}'';
          }
          {
            name = "Generate OpenAPI3 specification for the server";
            run = ''nix run .#${scripts.genOpenApi3.pname}'';
          }
          {
            name = "Write Docker image digests";
            env = {
              DOCKER_HUB_USERNAME = expr names.secrets.DOCKER_HUB_USERNAME;
            };
            run = ''nix run .#${scripts.writeDigests.pname}'';
          }
          {
            name = "Extra commit";
            run = ''
              git commit -a \
               -m "action: extra" \
               -m "- Update flake locks" \
               -m "- Generate OpenAPI3 specification for the server" \
               -m "- Write Docker image digests" \
               && git push || echo ""
            '';
          }
        ];
      };
      "${job4}" = {
        name = "Build and deploy GH Pages";
        needs = job3;
        environment = {
          name = "github-pages";
          url = expr "steps.deployment.outputs.page_url";
        };
        runs-on = os.ubuntu-20;
        steps = [
          steps.checkout
          {
            name = "Setup Pages";
            uses = "actions/configure-pages@v3";
          }
          {
            name = "Upload artifact";
            uses = "actions/upload-pages-artifact@v1";
            "with" = {
              # Upload entire repository
              path = ".";
            };
          }
          {
            name = "Deploy to GitHub Pages";
            id = "deployment";
            uses = "actions/deploy-pages@v2";
          }
        ];
      };
    };
  };
in
writeWorkflow name workflow
