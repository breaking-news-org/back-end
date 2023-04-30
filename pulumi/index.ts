// """
// Creating a Kubernetes Deployment
// """

import * as pulumi from "@pulumi/pulumi"
import * as k8s from "@pulumi/kubernetes"
import { Output, Input } from "@pulumi/pulumi"

function mkFullName(environment: string, name: string): string {
  return `${environment}-${name}`
}

interface PostgresConfig {
  name: string
  configMap: {
    data: {
      POSTGRES_DB: string
      POSTGRES_USER: string
      POSTGRES_PASSWORD: string
    }
  }
  persistentVolume: {
    hostPath: string
  }
  container: {
    image: string
    replicaCount: number
    tag: string
    volumeMounts: {
      name: string
      mountPath: string
    }
  }
  db: {
    port: number
  }
  service: {
    type: Input<"NodePort">
    port: number
    nodePort: number
  }
}

function mkPostgres(
  config: PostgresConfig,
  environment: string,
  provider: k8s.Provider
) {
  const appName = config.name
  const fullName = mkFullName(environment, appName)
  const dbConfig = config.db
  const containerConfig = config.container
  const persistentVolumeConfig = config.persistentVolume
  const serviceConfig = config.service
  const labels = { environment: environment }

  const opts = {
    provider: provider,
    // TODO need it?
    deleteBeforeReplace: true,
  }

  const configMap = ((name = `${fullName}-configmap`) =>
    new k8s.core.v1.ConfigMap(
      name,
      {
        metadata: {
          name: name,
          labels: labels,
        },
        data: config.configMap.data,
      },
      opts
    ))()

  const persistentVolume = ((name = `${fullName}-persistent-volume`) =>
    new k8s.core.v1.PersistentVolume(
      name,
      {
        metadata: {
          name: name,
          labels: labels,
        },
        spec: {
          storageClassName: "manual",
          capacity: {
            storage: "5Gi",
          },
          accessModes: ["ReadWriteMany"],
          hostPath: {
            path: persistentVolumeConfig.hostPath,
          },
        },
      },
      opts
    ))()

  const persistentVolumeClaim = ((
    name = `${fullName}-persistent-volume-claim`
  ) =>
    new k8s.core.v1.PersistentVolumeClaim(
      name,
      {
        metadata: {
          name: name,
          labels: labels,
        },
        spec: {
          storageClassName: "manual",
          accessModes: ["ReadWriteMany"],
          resources: {
            requests: {
              storage: "5Gi",
            },
          },
        },
      },
      opts
    ))()

  const dbPort = "db-port"

  const deployment = ((name = `${fullName}-deployment`) =>
    new k8s.apps.v1.Deployment(
      name,
      {
        metadata: {
          name: name,
          labels: labels,
        },
        spec: {
          replicas: 1,
          selector: {
            matchLabels: labels,
          },
          template: {
            metadata: {
              labels: labels,
            },
            spec: ((volumeName = `${name}-container-volume`) => {
              return {
                containers: [
                  {
                    name: `${name}-container`,
                    image: containerConfig.image,
                    imagePullPolicy: "IfNotPresent",
                    ports: [
                      {
                        containerPort: dbConfig.port,
                        name: dbPort,
                      },
                    ],
                    envFrom: [
                      {
                        configMapRef: {
                          name: configMap.metadata.name,
                        },
                      },
                    ],
                    volumeMounts: [
                      {
                        name: volumeName,
                        mountPath: containerConfig.volumeMounts.mountPath,
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
                    name: volumeName,
                    persistentVolumeClaim: {
                      claimName: persistentVolumeClaim.metadata.name,
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

  const service = ((name = `${fullName}-service`) =>
    new k8s.core.v1.Service(
      name,
      {
        metadata: {
          name: name,
          labels: labels,
        },
        spec: {
          type: serviceConfig.type,
          ports: [
            {
              port: serviceConfig.port,
              targetPort: dbPort,
              nodePort: serviceConfig.nodePort,
            },
          ],
          selector: labels,
        },
      },
      opts
    ))()

  // TODO use just the service name
  const host = service.metadata.name
  const port = serviceConfig.port
  return { host: host, port: port }
}

interface AppConfig {
  env: string
  db: {
    db: string
    user: string
    password: string
    dbName: string
    numConns: number
  }
  web: {
    port: number
    pageSize: number
  }
}

interface JWKConfig {
  d: string
  dp: string
  dq: string
  e: string
  kty: string
  n: string
  p: string
  q: string
  qi: string
}

interface BackConfig {
  name: string
  deployment: {
    container: {
      replicaCount: number
      name: string
      dockerHubImage: string
      config: {
        mountPath: string
        app: {
          varName: string
          filePath: string
          fileContent: AppConfig
        }
        jwk: {
          varName: string
          filePath: string
          fileContent: JWKConfig
        }
      }
    }
  }
  service: {
    port: number
    type: Input<"NodePort">
    nodePort: number
  }
}

function mkBack(
  config: BackConfig,
  environment: string,
  postgresHost: Output<string>,
  postgresPort: number,
  provider: k8s.Provider
) {
  const appName = config.name
  const fullName = mkFullName(environment, appName)
  const deploymentConfig = config.deployment
  const containerConfig = deploymentConfig.container
  const serviceConfig = config.service
  const labels = {
    environment: environment,
  }
  const opts = {
    provider: provider,
    deleteBeforeReplace: true,
  }

  const appConfig = containerConfig.config.app
  const jwkConfig = containerConfig.config.jwk

  interface AppConfigFileContent {
    env: string
    db: {
      db: string
      user: string
      password: string
      dbName: string
      host: Output<string>
      port: number
      numConns: number
    }
    web: {
      port: number
      pageSize: number
    }
  }

  const appFile: AppConfigFileContent = {
    ...appConfig.fileContent,
    ...{
      db: {
        ...appConfig.fileContent.db,
        ...{ host: postgresHost, port: postgresPort },
      },
    },
  }

  var data: { [k: string]: Output<string> } = {}

  data[appConfig.filePath] = pulumi.jsonStringify(appFile)
  data[jwkConfig.filePath] = pulumi.jsonStringify(jwkConfig.fileContent)

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

  const containerPort = appConfig.fileContent.web.port
  const containerPortName = `${fullName}-port`

  const deployment = ((name = `${fullName}-deployment`) =>
    new k8s.apps.v1.Deployment(
      name,
      {
        metadata: {
          name: name,
          labels: labels,
        },
        spec: {
          replicas: 1,
          selector: {
            matchLabels: labels,
          },
          template: {
            metadata: {
              labels: labels,
            },
            spec: ((
              appVolume = `${name}-app-config-volume`,
              configMountPath = containerConfig.config.mountPath
            ) => {
              return {
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
                      {
                        name: jwkConfig.varName,
                        value: `${configMountPath}/${jwkConfig.filePath}`,
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
                      // TODO
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

  const backService = ((name = `${fullName}-service`) =>
    new k8s.core.v1.Service(
      name,
      {
        metadata: {
          name: name,
          labels: labels,
        },
        spec: {
          type: serviceConfig.type,
          ports: [
            {
              port: serviceConfig.port,
              name: `${name}-port`,
              targetPort: containerPortName,
              nodePort: serviceConfig.nodePort,
            },
          ],
          selector: labels,
        },
      },
      opts
    ))()

  const host = backService.metadata.name
  const port = serviceConfig.port
  return { host: host, port: port }
}

interface TestConfig {
  name: string
  deployment: {
    container: {
      replicaCount: 1
      name: string
      dockerHubImage: string
      port: number
      app: {
        mountPath: string
        varName: string
        filePath: string
        fileContent: {
          users: {
            email: string
            password: string
          }[]
        }
      }
    }
  }
  service: {
    port: number
    type: string
    nodePort: number
  }
}

function mkTest(
  config: TestConfig,
  environment: string,
  backHost: Output<string>,
  backPort: number,
  provider: k8s.Provider
) {
  const appName = config.name
  const fullName = mkFullName(environment, appName)
  const containerConfig = config.deployment.container
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

function mkSetup(environment: string, provider: k8s.Provider) {
  const config = new pulumi.Config(environment)

  const postgres = mkPostgres(
    config.requireObject("postgres"),
    environment,
    provider
  )

  const back = mkBack(
    config.requireObject("back"),
    environment,
    postgres.host,
    postgres.port,
    provider
  )

  if (environment == "dev") {
    mkTest(
      config.requireObject("test"),
      environment,
      back.host,
      back.port,
      (provider = provider)
    )
  }
}

// const provider = new k8s.Provider("k8s-yaml-rendered", {
//   renderYamlToDirectory: "yaml",
// })
const provider = new k8s.Provider("kubernetes-provider")

if (pulumi.getStack() == "dev") {
  mkSetup("dev", provider)
} else if (pulumi.getStack() == "prod") {
  mkSetup("prod", provider)
}