import { Output } from "@pulumi/pulumi"
import { TestConfig } from "./pulumi"
import * as k8s from "@pulumi/kubernetes"
import { mkFullName } from "../common"
import * as pulumi from "@pulumi/pulumi"

export function main(
  config: TestConfig,
  environment: string,
  backHost: Output<string>,
  backPort: number,
  dockerHubImage: string,
  provider: k8s.Provider
) {
  const appName = config.name
  const fullName = mkFullName(environment, appName)
  const containerConfig = {
    ...config.deployment.container,
    ...{ dockerHubImage: dockerHubImage },
  }
  const labels = {
    environment: environment,
  }
  const opts = {
    provider: provider,
    deleteBeforeReplace: true,
  }

  var testConfig = containerConfig.app

  interface AppConfigFileContent {
    app: {
      host: Output<string>
      port: number
    }
    users: {
      email: string
      password: string
    }[]
  }

  const appConfigFileContent: AppConfigFileContent = {
    ...testConfig.fileContent,
    app: { host: backHost, port: backPort },
  }

  var data: { [k: string]: Output<string> } = {}
  data[testConfig.filePath] = pulumi.jsonStringify(appConfigFileContent)

  const configMap = ((name = `${fullName}-configmap`) =>
    new k8s.core.v1.ConfigMap(
      name,
      {
        metadata: {
          name: name,
          labels: labels,
        },
        data: data,
      },
      opts
    ))()
  const appConfig = containerConfig.app
  const containerPort = containerConfig.port
  const containerPortName = `${fullName}-port`

  const job = ((name = `${fullName}-job`) =>
    new k8s.batch.v1.Job(
      name,
      {
        metadata: {
          name: name,
          labels: labels,
        },
        spec: {
          template: {
            metadata: {
              labels: labels,
            },
            spec: ((
              appVolume = `${name}-app-config-volume`,
              configMountPath = appConfig.mountPath
            ) => {
              return {
                restartPolicy: "OnFailure",
                containers: [
                  {
                    name: containerConfig.name,
                    image: containerConfig.dockerHubImage,
                    imagePullPolicy: "Always",
                    ports: [
                      {
                        containerPort: containerPort,
                        name: containerPortName,
                      },
                    ],
                    volumeMounts: [
                      {
                        name: appVolume,
                        mountPath: configMountPath,
                      },
                    ],
                    env: [
                      {
                        name: appConfig.varName,
                        value: `${configMountPath}/${appConfig.filePath}`,
                      },
                    ],
                    resources: {
                      requests: {
                        memory: "64Mi",
                        cpu: "250m",
                      },
                      limits: {
                        memory: "128Mi",
                        cpu: "500m",
                      },
                    },
                  },
                ],
                volumes: [
                  {
                    name: appVolume,
                    configMap: {
                      name: configMap.metadata.name,
                    },
                  },
                ],
              }
            })(),
          },
        },
      },
      opts
    ))()
}