import { Output } from "@pulumi/pulumi"
import { Config } from "./pulumi"
import * as k8s from "@pulumi/kubernetes"
import { mkFullName } from "../common"
import { AppConfigFile } from "./config"
import * as pulumi from "@pulumi/pulumi"
import path = require("node:path")
import process = require('node:process')
import fs = require('node:fs')
import { parse } from "yaml"

export function main(
  config: Config,
  environment: string,
  postgresHost: Output<string>,
  postgresPort: number,
  dockerHubImage: string,
  provider: k8s.Provider
) {
  const appName = config.name
  const fullName = mkFullName(environment, appName)
  const deploymentConfig = config.deployment

  const containerConfig = {
    ...deploymentConfig.container,
    ...{ dockerHubImage: dockerHubImage },
  }
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
  const staticContentHostPath = `${path.dirname(process.cwd())}/static`
  console.log(staticContentHostPath)

  const appConfigFile_ = parse(
    fs.readFileSync(`${appName}.${environment}.yaml`, "utf8")
  ) as AppConfigFile

  const appConfigFile: AppConfigFile = {
    ...appConfigFile_,
    ...{
      dataBase: {
        ...appConfigFile_.dataBase,
        ...{ host: postgresHost, port: postgresPort },
      },
    },
  }

  var data: { [k: string]: Output<string> } = {}

  data[appConfig.filePath] = pulumi.jsonStringify(appConfigFile)
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

  const containerPort = appConfigFile.web.port
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
              staticVolume = `${name}-app-static-volume`,
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
                      {
                        name: staticVolume,
                        mountPath: appConfigFile.web.staticContent,
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
                  {
                    name: staticVolume,
                    hostPath: {
                      path: staticContentHostPath,
                      type: `DirectoryOrCreate`,
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