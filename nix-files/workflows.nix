{ workflows, backDir, name, system, scripts }:
let
  inherit (workflows.functions.${system}) writeWorkflow expr mkAccessors genAttrsId run;
  inherit (workflows.configs.${system}) steps os oss;
  job1 = "_1_update_flake_locks";
  job2 = "_2_push_to_cachix";
  job3 = "_3_push_to_docker_hub";
  job4 = "_4_gen_openapi3_specification";
  job5 = "_5_build_deploy_gh_pages";
  names = mkAccessors {
    matrix.os = "";
    secrets = genAttrsId [
      "GITHUB_TOKEN"
      "DOCKER_HUB_USERNAME"
      "DOCKER_HUB_PASSWORD"
    ];
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
    concurrency = {
      group = "pages";
      cancel-in-progress = false;
    };
    # Sets permissions of the GITHUB_TOKEN to allow deployment to GitHub Pages
    permissions = {
      contents = "read";
      pages = "write";
      id-token = "write";
    };
    jobs = {
      "${job1}" = {
        name = "Update flake locks";
        runs-on = os.ubuntu-20;
        steps = [
          steps.checkout
          steps.installNix
          steps.configGitAsGHActions
          steps.updateLocksAndCommit
        ];
      };
      "${job2}" = {
        name = "Push to cachix";
        # needs = job1;
        strategy.matrix.os = oss;
        runs-on = expr names.matrix.os;
        steps = [
          steps.checkout
          steps.installNix
          steps.logInToCachix
          steps.pushFlakesToCachix
        ];
      };
      "${job3}" = {
        name = "Push to Docker Hub";
        # needs = job1;
        runs-on = os.ubuntu-20;
        steps = [
          steps.checkout
          steps.installNix
          {
            name = "Log in to Docker";
            uses = "docker/login-action@v2.1.0";
            "with" = {
              username = expr names.secrets.DOCKER_HUB_USERNAME;
              password = expr names.secrets.DOCKER_HUB_PASSWORD;
            };
          }
          {
            name = "Push to Docker Hub";
            env = {
              DOCKER_HUB_USERNAME = expr names.secrets.DOCKER_HUB_USERNAME;
              DOCKER_HUB_PASSWORD = expr names.secrets.DOCKER_HUB_PASSWORD;
            };
            run = ''
              cd ${backDir}
              nix run .#${scripts.appPushToDockerHub.pname}
              nix run .#${scripts.testPushToDockerHub.pname}
            '';
          }
        ];
      };
      "${job4}" = {
        name = "Generate OpenAPI3 specification for the server";
        # needs = job1;
        runs-on = os.ubuntu-20;
        steps = [
          steps.checkout
          steps.configGitAsGHActions
          steps.installNix
          {
            name = "Generate specification";
            run = run.nixRunAndCommit scripts.genOpenApi3.pname "update OpenAPI3 spec";
          }
        ];
      };
      "${job5}" = {
        needs = job4;
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
