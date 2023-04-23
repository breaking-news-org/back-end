{ workflows, backDir, name, system, scripts }:
let
  inherit (workflows.functions.${system}) writeWorkflow expr mkAccessors genAttrsId;
  inherit (workflows.configs.${system}) steps os oss;
  job1 = "_1_update_flake_locks";
  job2 = "_2_push_to_cachix";
  job3 = "_3_push_to_docker_hub";
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
        needs = job1;
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
        needs = job1;
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
    };
  };
in
writeWorkflow name workflow
