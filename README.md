# Back end

## Overview

- Functional requirements are described [here](https://github.com/fullstack-development/haskell-internship/blob/master/server-task.md).
- API versioning - via url part (easy to version the whole API) ([src](https://usecsv.com/community/api-versioning))

## OpenAPI3

- [V1](https://redocly.github.io/redoc/?url=https://raw.githubusercontent.com/breaking-news-org/back-end/main/API/v1.yaml)

## Authorization

- [JWT](https://stackoverflow.com/a/62095037)
- [What Are Refresh Tokens and How to Use Them Securely](https://auth0.com/blog/refresh-tokens-what-are-they-and-when-to-use-them/#Refresh-Token-Automatic-Reuse-Detection)

## Infra

- [Pulumi](https://www.pulumi.com/)

## Architecture

- Units:
  - `API` - describes API.
  - `Persist` - provides operations with a database.
  - `Service` - provides services on top of `Persist`.
  - `Controller` - provides operations for `API` endpoints on top of `Service`.
  - `Server` - binds `API` endpoints and `Controller` operations to produce a server.

## Development

Set up the cluster and make test requests.

1. Install `minikube`, `pulumi`, `kubernetes`, `Postman`, `psql`, `docker`, `microk8s`, `nodejs` (16).

1. Run a cluster

    ```console
    # start a minikube vm
    minikube start

    # go to pulumi config
    cd pulumi

    # install node package
    npm i

    # select dev stack
    pulumi stack select dev

    # select dev stack
    pulumi stack select dev

    # start cluster
    pulumi up

    # select `yes`

    # check that all pods are ready
    kubectl get po
    ```

1. Get the IP address of the cluster (let's call it `<IP>`).

    ```console  
    minikube ip
    ```

1. Get the port of the server `dev:back.service.nodePort` (let's call it `<port>`).

1. Import the [API](./API/v1.yaml) into `Postman` as a `Postman Collection`.

1. In that collection, set the variable `baseUrl` to `http://<IP>:<port>`.
   1. Click on the collection -> `Variables` -> `Current Value`
   1. Save

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

1. Send a request to `api/news/create`.

   1. Copy the last `accessToken` to your Collection -> `Variables` -> `{{bearerToken}}`.
   1. Edit the value in the request `Body`.
   1. You'll get nothing in the response body.

1. Send a request to `api/news/get`.
   1. Remove all query parameters
   1. You'll get a list of news in the response body.

1. Check database

    ```console
    # connect to the database
    psql postgresql://admin:psltest@localhost:30002/postgresdb

    # check records
    select * from users;
    ```

1. If you delete a resource, refresh `pulumi`.

    ```console
    # delete some pod
    kubectl delete po ...
    
    # after deleting resources, run
    pulumi refresh

    pulumi up
    ```

## Production

- On a server, we user `microk8s`.
  - Run the script to set up environment in a current shell.

    ```console
    source microk8s.sh
    ```

## Nix

### Prerequisites

See these for additional info:

- [codium-generic](https://github.com/deemp/flakes/tree/main/templates/codium/generic#readme) - info just about `VSCodium` and extensions.
- [codium-haskell](https://github.com/deemp/flakes/tree/main/templates/codium/haskell#readme) - an advanced version of this flake.
- [flake.nix](./flake.nix) - extensively commented code.
- [Haskell](https://github.com/deemp/flakes/blob/main/README/Haskell.md)
- [Troubleshooting](https://github.com/deemp/flakes/blob/main/README/Troubleshooting.md)
- [Prerequisites](https://github.com/deemp/flakes#prerequisites)

### Quick start

1. Install Nix - see [how](https://github.com/deemp/flakes/blob/main/README/InstallNix.md).

1. In a new terminal, start a devshell:

    ```console
    nix develop
    ```

1. Write `settings.json` and start `VSCodium`:

    ```console
    nix run .#writeSettings
    nix run .#codium .
    ```

### Configs

- [package.yaml](./package.yaml) - used by `hpack` to generate a `.cabal`
- [.markdownlint.jsonc](./.markdownlint.jsonc) - for `markdownlint` from the extension `davidanson.vscode-markdownlint`
- [.ghcid](./.ghcid) - for [ghcid](https://github.com/ndmitchell/ghcid)
- [.envrc](./.envrc) - for [direnv](https://github.com/direnv/direnv)
- [fourmolu.yaml](./fourmolu.yaml) - for [fourmolu](https://github.com/fourmolu/fourmolu#configuration)
- [ci.yaml](.github/workflows/ci.yaml) - a generated `GitHub Actions` workflow. See [workflows](https://github.com/deemp/flakes/tree/main/workflows). Generate a workflow via `nix run .#writeWorkflows`.
- `hie.yaml` - not present, but can be generated via [implicit-hie](https://github.com/Avi-D-coder/implicit-hie) (available on devshell) to verify the `Haskell Language Server` setup.

## References

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
