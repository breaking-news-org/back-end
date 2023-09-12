import * as k8s from "@pulumi/kubernetes"
import { PostgresConfig } from "./pulumi"
import { mkFullName } from "../common"

export function main(
  config: PostgresConfig,
  environment: string,
  provider: k8s.Provider
) {
  const appName = config.name
  const fullName = mkFullName(environment, appName)
  const dbConfig = config.dataBase
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
