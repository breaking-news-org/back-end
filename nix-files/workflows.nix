{ workflows, backDir, name, herokuAppName, system }:
let
  inherit (workflows.functions.${system}) writeWorkflow expr mkAccessors genAttrsId;
  inherit (workflows.configs.${system}) steps os oss;
  job1 = "_1_update_flake_locks";
  job2 = "_2_push_to_cachix";
  job3 = "_3_push_to_heroku";
  job4 = "_4_push_to_docker_hub";
  herokuAppName = "breaking-news-back";
  names = mkAccessors {
    matrix.os = "";
    secrets = genAttrsId [
      "GITHUB_TOKEN"
      "HEROKU_API_KEY"
      "HEROKU_EMAIL"
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
        name = "Push to Heroku";
        needs = job1;
        runs-on = os.ubuntu-20;
        steps = [
          steps.checkout
          steps.installNix
          {
            name = "Log in to Heroku";
            uses = "AkhileshNS/heroku-deploy@master";
            "with" = {
              heroku_api_key = expr names.secrets.HEROKU_API_KEY;
              heroku_email = expr names.secrets.HEROKU_EMAIL;
              heroku_app_name = herokuAppName;
              justlogin = true;
            };
          }
          {
            name = "Release app on Heroku";
            run = ''
              cd ${backDir}
              nix run .#herokuRelease
            '';
          }
        ];
      };
      "${job4}" = {
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
            run = ''
              cd ${backDir}
              nix run .#pushToDockerHub
            '';
          }
        ];
      };
    };
  };
in
writeWorkflow name workflow
