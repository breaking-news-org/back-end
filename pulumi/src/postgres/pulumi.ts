import { Input } from "@pulumi/pulumi"

export interface PostgresConfig {
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
  dataBase: {
    port: number
  }
  service: {
    type: Input<"NodePort">
    port: number
    nodePort: number
  }
}