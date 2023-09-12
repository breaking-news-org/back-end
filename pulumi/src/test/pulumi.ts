import { Input } from "@pulumi/pulumi"


export interface TestConfig {
  name: string
  deployment: {
    container: {
      replicaCount: 1
      name: string
      dockerHubImage?: string
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

