{ workflows, backDir, name, system, back, test, scripts }:
let
  inherit (workflows.functions.${system}) writeWorkflow expr mkAccessors genAttrsId run;
  inherit (workflows.configs.${system}) steps os oss;
  job1 = "_1_update_flake_locks";
  job2 = "_2_push_to_cachix";
  job3 = "_3_push_to_docker_hub";
  job4 = "_4_gen_openapi3_specification";
  job5 = "_5_build_deploy_gh_pages";
  job6 = "_6_write_image_digests";
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
    concurrency = {
      group = "pages";
      cancel-in-progress = false;
    };
    # Sets permissions of the GITHUB_TOKEN to allow deployment to GitHub Pages
    permissions = {
      contents = "write";
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
        name = "Push ${expr names.matrix.scriptName} to Docker Hub";
        # needs = job1;
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
      "${job4}" = {
        name = "Generate OpenAPI3 specification for the server";
        # needs = job1;
        runs-on = os.ubuntu-20;
        steps = [
          steps.checkout
          steps.installNix
          steps.configGitAsGHActions
          {
            name = "Generate specification";
            run = run.nixRunAndCommit scripts.genOpenApi3.pname "update OpenAPI3 spec";
          }
        ];
      };
      "${job5}" = {
        name = "Build and deploy GH Pages";
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
      "${job6}" = {
        name = "Push ${expr names.matrix.scriptName} to Docker Hub";
        needs = [ job3 ];
        runs-on = os.ubuntu-20;
        steps = [
          steps.checkout
          steps.installNix
          steps.configGitAsGHActions
          {
            name = "Write digests";
            env = {
              DOCKER_HUB_USERNAME = expr names.secrets.DOCKER_HUB_USERNAME;
            };
            run = run.nixRunAndCommit scripts.writeDigests.pname "Write image digests";
          }
        ];
      };
    };
  };
in
writeWorkflow name workflow
