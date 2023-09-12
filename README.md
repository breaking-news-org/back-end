# Back end

This repository provides a news website server written in Haskell.

## Overview

### Functional requirements

- Most functional requirements are described [here](https://github.com/fullstack-development/haskell-internship/blob/master/server-task.md).

### OpenAPI 3

- [Swagger UI](https://breaking-news-org.github.io/back-end/).

### Security

#### Authorization

We provide user authorization via JWT.

- [JWT](https://stackoverflow.com/a/62095037).
- [What Are Refresh Tokens and How to Use Them Securely](https://auth0.com/blog/refresh-tokens-what-are-they-and-when-to-use-them/#Refresh-Token-Automatic-Reuse-Detection).

#### Encryption

In a database, we store user passwords encrypted via [password](https://hackage.haskell.org/package/password).

### Infrastructure

- [Pulumi](https://www.pulumi.com/)
- [Kubernetes](https://kubernetes.io/)
- [GitHub Actions](https://github.com/breaking-news-org/back-end/actions)

### Database

- [PostgreSQL](https://www.postgresql.org/).
- [esqueleto](https://hackage.haskell.org/package/esqueleto).
  - `esqueleto` [prevents](https://github.com/bitemyapp/esqueleto/tree/9a4f7d7c3e56357abb1dd2afe822139882577464#sql-injection) SQL injections.

### Architecture

Haskell modules from [back/src](./back/src/) are logically grouped into the following units.

- `Persist` - provides operations with a database.
- `Service` - provides services on top of `Persist`.
- `API` - describes API.
- `Controller` - provides operations for `API` endpoints on top of `Service`.
- `Server` - binds `API` endpoints and `Controller` operations to produce a server.

## Development

1. <details>

   <summary>Nix prerequisites</summary>

    See these for additional info:

    - [codium-generic](https://github.com/deemp/flakes/tree/main/templates/codium/generic#readme) - info just about `VSCodium` and extensions.
    - [codium-haskell](https://github.com/deemp/flakes/tree/main/templates/codium/haskell#readme) - an advanced version of this flake.
    - [flake.nix](./flake.nix) - extensively commented code.
    - [Haskell](https://github.com/deemp/flakes/blob/main/README/Haskell.md)
    - [Troubleshooting](https://github.com/deemp/flakes/blob/main/README/Troubleshooting.md)
    - [Prerequisites](https://github.com/deemp/flakes#prerequisites)

    </details>

1. Install Nix - see [how](https://github.com/deemp/flakes/blob/main/README/InstallNix.md).

1. Install the following programs.

   - `pulumi`
   - `microk8s`

1. Start a `devShell`.

    ```console
    nix develop
    ```

1. (Optionally) Write `settings.json` and start `VSCodium`.

    ```console
    nix run .#writeSettings
    nix run .#codium .
    ```

1. (Optionally) Open a `.hs` file and hover over a function. Shortly, Haskell Language Server should start giving the type info.

1. See the following sections. They assume `dev` environment.

   - There are configurations for the `prod` environment.

### Run the application

#### Cluster

Run a database, a server, and tests in a Kubernetes cluster.

1. Create a cluster.

    ```console
    # prepare the environment
    source microk8s.sh
    
    # start a microk8s vm
    microk8s start
    ```

1. The pods configuration is in [Pulumi.dev.yaml](./pulumi/Pulumi.dev.yaml).

1. Prepare `pulumi`.

    ```console
    # go to pulumi directory
    cd pulumi

    # install node packages
    npm i

    # select dev stack
    pulumi stack select dev
    ```

1. Create pods in the cluster.

    ```console
    # create all resources
    pulumi up

    # select `yes`

    # check that all pods are ready
    kubectl get po -n breaking-news-dev
    ```

#### Cabal

1. Follow the steps from [Cluster](#cluster) until pod creation.

1. Run a database in the cluster.

   ```console
   # create postgres-related resources
   pulumi up -t **postgres*

   # select `yes`

   kubectl get po -n breaking-news-dev
   ```

1. The app configuration is in the [local](./local) directory. See [back.dev.yaml](./local/back.dev.yaml).

1. Run the server using `cabal-install`. The server will connect to the database in the cluster.

    ```console
    cabal run back:exe:back
    ```

#### Images

Build Docker images, push to Docker Hub and pin image digests for app and test.

1. Create `secrets.env` file from `secrets.example.env`.

1. `source` environment files.

    1. Manually

        ```console
        source secrets.env
        source .env
        ```

    1. Via `direnv`

        ```console
        direnv allow
        ```

1. Push images to Docker Hub.

    ```console
    nix run .#backPushToDockerHub
    nix run .#testPushToDockerHub
    ```

1. Write image digests.

   ```console
   nix run .#writeDigests
   ```

### Expose the server

1. The server IP address (let's call it `<IP>`) is `localhost`.

1. Get the port (let's call it `<port>`) of the server.

   - For the server in the cluster, see `dev:back.service.nodePort` in [Pulumi.dev.yaml](./pulumi/Pulumi.dev.yaml).
   - For the server run via `cabal-install`, see `web.port` in [back.dev.yaml](./local/back.dev.yaml).

1. Expose the server to the world via `localtunnel`.

    ```console
    lt -l <IP> -p <port> -s <subdomain>
    
    # or, in case of a cluster

    lt -l localhost -p 30003 -s breaking-news-fun
    ```

1. Copy the address (let's call it `<address>`) obtained from `localtunnel`.

### Swagger

View the API, make requests to it.

1. Access the Swagger UI at `<address>/api1/index.html`.

### Postman

View the API, make requests to it.

1. Import the [API](./API/v1.yaml) into `Postman` as a `Postman Collection`.

1. In that collection, set the variable `baseUrl` to `<address>`.

   1. Click on the collection -> `Variables` -> `Current Value`.
   1. Save.

1. Send a request to `/api1/user/register`.

    1. Set the values in the request `Body`.
    1. You'll get a refresh token (`refreshToken`) and an access token (`accessToken`).

1. Send a request to `/api1/user/rotate-refresh-token`.

    1. Copy `refreshToken` to `Authorization` -> `BearerToken` -> `Token`.
    1. You'll get a renewed pair of a refresh token and an access token.

1. Send the request again with the old `refreshToken`.

    1. You'll get a message that the session has a newer token.
    1. This is due to refresh token reuse detection (see [Authorization](#authorization)).

1. Send a request to `/api1/user/login`.

    1. Copy the values from the request to `/api1/user/register` into `Body`.
    1. You'll get a renewed pair of a refresh token and an access token.

1. Send a request to `api1/news/create`.

   1. Copy the last `accessToken` to your Collection -> `Variables` -> `{{bearerToken}}`.
   1. Edit the value in the request `Body`.
   1. You'll get nothing in the response body.

1. Send a request to `api1/news/get`.
   1. Remove all query parameters.
   1. You'll get a list of news in the response body.

### Database

1. Check the database.

    ```console
    # connect to the database
    psql postgresql://admin:admin_password@localhost:30002/postgresdb

    # check records
    select * from users;
    ```

### Monitoring

1. The cluster provides pod monitoring via [loki-stack](https://github.com/grafana/helm-charts/tree/eb55960453f49814d3234fd4417ffb2d5b127b80/charts/loki-stack).

1. Monitoring is configured in [Pulumi.dev.yaml](./pulumi/Pulumi.dev.yaml) at `dev:monitoring`.

1. `Grafana` should be available at `http://localhost:30005`.

   - Username: `admin`
   - Password: `admin_password`

1. In `Explore` -> `Log browser`, type `{app="back-deployment"}`

### Pulumi

Manage resources in a running cluster via `pulumi` (see [Cluster](#cluster)).

1. Go to `pulumi`.

    ```console
    cd pulumi
    ```

1. Get urns.

    ```console
    pulumi stack --show-urns
    ```

1. Try deleting a resource.

    ```concole
    pulumi state delete <urn>
    ```

1. If you delete a resource via `kubectl`, refresh `pulumi`. Usually, `pulumi` will notice the changes.

    ```console
    # delete some pod
    kubectl delete po ...
    
    # after deleting resources, run
    pulumi refresh
    
    # update resources
    pulumi up
    ```

1. Destroy all resources.

    ```console
    pulumi destroy
    ```

## Configs

- [package.yaml](./package.yaml) - used by `hpack` to generate a `.cabal`
- [.markdownlint.jsonc](./.markdownlint.jsonc) - for `markdownlint` from the extension `davidanson.vscode-markdownlint`
- [.ghcid](./.ghcid) - for [ghcid](https://github.com/ndmitchell/ghcid)
- [.envrc](./.envrc) - for [direnv](https://github.com/direnv/direnv)
- [fourmolu.yaml](./fourmolu.yaml) - for [fourmolu](https://github.com/fourmolu/fourmolu#configuration)
- [ci.yaml](.github/workflows/ci.yaml) - a generated `GitHub Actions` workflow. See [workflows](https://github.com/deemp/flakes/tree/main/workflows). Generate a workflow via `nix run .#writeWorkflows`.
- `hie.yaml` - not present, but can be generated via [implicit-hie](https://github.com/Avi-D-coder/implicit-hie) (available on devshell) to verify the `Haskell Language Server` setup.

## References

- [charts/loki-stack](https://github.com/grafana/helm-charts/tree/eb55960453f49814d3234fd4417ffb2d5b127b80/charts/loki-stack)
  - It's possible to overwrite chart values
    - Find `grafana` in [requirements.yaml](https://github.com/grafana/helm-charts/blob/eb55960453f49814d3234fd4417ffb2d5b127b80/charts/loki-stack/requirements.yaml#L14)
    - Find its `grafana` chart [values](https://github.com/grafana/helm-charts/blob/eb55960453f49814d3234fd4417ffb2d5b127b80/charts/grafana/values.yaml)
    - Edit `loki-stack` [values](https://github.com/grafana/helm-charts/blob/eb55960453f49814d3234fd4417ffb2d5b127b80/charts/loki-stack/values.yaml#L36) for `grafana`
- Grafana, Loki, Promtail - [YT](https://www.youtube.com/watch?v=MBJ9puJyYH8)
  - [similar](https://brain2life.hashnode.dev/grafana-loki-setup-in-minikube)
- `DeriveAnyClass` sometimes leads to infinite loops - see [ToHttpApiData](https://hackage.haskell.org/package/servant-0.19.1/docs/Servant-API.html#t:ToHttpApiData)
- Running server in a `VM`
  - Host shouldn't be Windows
  - Forward port to internal server - [src](https://www.cyberciti.biz/faq/how-to-configure-ufw-to-forward-port-80443-to-internal-server-hosted-on-lan/)
  - How to Setup Port Forward with NAT Network in Virtualbox - [YT](https://www.youtube.com/watch?v=FBcFNdzJOes)
  - Set up prerouting on guest - [SO](https://serverfault.com/a/859440)
  - Disable firewall on guest - `sudo ufw disable`
- [kustomization.yaml](https://kubernetes.io/docs/tasks/manage-kubernetes-objects/kustomization/)
- [configmap](https://kubernetes.io/docs/tasks/configure-pod-container/configure-pod-configmap/#create-a-configmap-from-generator)
- [web app](https://www.endpointdev.com/blog/2022/01/kubernetes-101/)
- [Understanding Kubernetes Objects](https://kubernetes.io/docs/concepts/overview/working-with-objects/kubernetes-objects/)
- [Debugging Templates (helm)](https://helm.sh/docs/chart_template_guide/debugging/)
  - `helm lint --strict`
- [toYaml correct indentation](https://stackoverflow.com/questions/73660380/helm-toyaml-indenting-dictionary-map-correctly)
- [Persistent Volumes](https://kubernetes.io/docs/concepts/storage/persistent-volumes)
  - PVC is bound to a PV at runtime automatically
  - There's no connection between PVCs and PVs in a manifest
- [advanced helm templating](https://blog.palark.com/advanced-helm-templating/)
- [Using production data in staging](https://help.cloud66.com/docs/databases/using-master-data-staging)
- [API versioning](https://usecsv.com/community/api-versioning)
  - via url part (easy to version the whole API)
- [PostgreSQL Initialization scripts](https://github.com/docker-library/docs/blob/master/postgres/README.md#initialization-scripts)
