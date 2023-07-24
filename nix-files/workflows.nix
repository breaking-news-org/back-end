{ workflows, backDir, name, system, back, test, scripts }:
let
  inherit (workflows.lib.${system}) writeWorkflow expr mkAccessors genAttrsId run steps os oss strategies nixCI job;
  job1 = "_1_push_to_docker_hub_and_commit";
  job2 = "_2_build_deploy_gh_pages";
  job3 = "_3_purge_cache";
  names = mkAccessors {
    matrix.os = "";
    matrix.store = "";
    matrix.scriptName = genAttrsId [
      "scriptName"
    ];
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

  commonKeySuffix = "common";
  workflow = {
    name = "CI";
    inherit on;
    # Sets permissions of the GITHUB_TOKEN to allow deployment to GitHub Pages
    permissions = {
      contents = "write";
      pages = "write";
      actions = "write";
      id-token = "write";
    };
    jobs = {
      "${job1}" = job
        {
          name = "Push ${expr names.matrix.scriptName} to Docker Hub";
          runsOn = os.ubuntu-22;
          strategy = {
            matrix = {
              scriptName = [ back test ];
            };
          };
          cacheNixArgs = {
            keyJob = expr names.matrix.scriptName;
            linuxGCEnabled = true;
            linuxMaxStoreSize = 7000000000;
            macosGCEnabled = true;
            macosMaxStoreSize = 7000000000;
          };
          doUpdateLocks = true;
          doFormat = true;
          doPurgeCache = false;
          steps = { stepsAttrs, ... }: [
            {
              name = "Push to Docker Hub";
              env = {
                DOCKER_HUB_USERNAME = expr names.secrets.DOCKER_HUB_USERNAME;
                DOCKER_HUB_PASSWORD = expr names.secrets.DOCKER_HUB_PASSWORD;
              };
              run = run.nixScript { name = "${expr names.matrix.scriptName}PushToDockerHub"; };
            }
            {
              name = "Generate server specification";
              run = run.nixScript { name = scripts.genOpenAPI3.pname; };
            }
            {
              name = "Write Docker image digests";
              env = {
                DOCKER_HUB_USERNAME = expr names.secrets.DOCKER_HUB_USERNAME;
              };
              run = run.nixScript { name = scripts.writeDigests.pname; };
            }
            (steps.commit {
              messages = [
                (steps.updateLocks { }).name
                (steps.format { }).name
                stepsAttrs."Generate server specification".name
                stepsAttrs."Write Docker image digests".name
              ];
            })
          ];
        };
      "${job2}" = {
        name = "Build and deploy GH Pages";
        needs = job1;
        environment = {
          name = "github-pages";
          url = expr "steps.deployment.outputs.page_url";
        };
        runs-on = os.ubuntu-22;
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
