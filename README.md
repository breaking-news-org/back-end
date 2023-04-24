# Back end

## Overview

- Functional requirements are described [here](https://github.com/fullstack-development/haskell-internship/blob/master/server-task.md).
- API versioning - via url part (easy to version the whole API) ([src](https://usecsv.com/community/api-versioning))

## OpenAPI3

- [V1](https://redocly.github.io/redoc/?url=https://raw.githubusercontent.com/breaking-news-org/back-end/main/API/v1.yaml)

## Tests

- [Using production data in staging](https://help.cloud66.com/docs/databases/using-master-data-staging)

## Infra

- [Pulumi](https://www.pulumi.com/)

## Kubernetes

- For local development, we use `minikube`.

    ```console
    # start a minikube vm
    minikube start

    # go to pulumi config
    cd pulumi

    # select dev stack
    pulumi stack select dev

    # start cluster
    pulumi up

    # check that all pods are ready
    kubectl get po

    # make a request to the server
    curl --location --request POST "http://$(minikube ip):30003/api1/user/authorize" \
            --header 'Content-Type: application/json' \
            --data-raw '{
                "email": "contact@zelinf.net",
                "password": "abcdef"
            }'

    # connect to the database
    psql postgresql://admin:psltest@localhost:30002/postgresdb

    # check records
    select * from users;
    ```

- On a server, we user `microk8s`.

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
