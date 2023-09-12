import * as k8s from "@pulumi/kubernetes"
import { PostgresConfig } from "./pulumi"

export function main(
  config: PostgresConfig,
  environment: string,
  provider: k8s.Provider,
  namespace: string
) {
  const appName = config.name
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

  const configMap = ((name = `${appName}-configmap`) =>
    new k8s.core.v1.ConfigMap(
      name,
      {
        metadata: {
          name,
          labels,
          namespace
        },
        data: config.configMap.data,
      },
      opts
    ))()

  const persistentVolume = ((name = `${appName}-persistent-volume`) =>
    new k8s.core.v1.PersistentVolume(
      name,
      {
        metadata: {
          name,
          labels,
          namespace
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
    name = `${appName}-persistent-volume-claim`
  ) =>
    new k8s.core.v1.PersistentVolumeClaim(
      name,
      {
        metadata: {
          name,
          labels,
          namespace,
          finalizers: [
            
          ]
        },
        spec: {
          storageClassName: "manual",
          accessModes: ["ReadWriteMany"],
          resources: {
            requests: {
              storage: "512M",
            },
          },
        },
      },
      opts
    ))()

  const dbPort = "db-port"

  const deployment = ((name = `${appName}-deployment`) =>
    new k8s.apps.v1.Deployment(
      name,
      {
        metadata: {
          name,
          labels,
          namespace
        },
        spec: {
          replicas: 1,
          selector: {
            matchLabels: labels,
          },
          template: {
            metadata: {
              labels,
              namespace
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

  const service = ((name = `${appName}-service`) =>
    new k8s.core.v1.Service(
      name,
      {
        metadata: {
          name,
          labels,
          namespace
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
