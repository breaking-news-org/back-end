import { Input } from "@pulumi/pulumi"

export interface JWK {
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

export interface Config {
  name: string
  deployment: {
    container: {
      replicaCount: number
      name: string
      dockerHubImage?: string
      config: {
        mountPath: string
        app: {
          varName: string
          filePath: string
        }
        jwk: {
          varName: string
          filePath: string
          fileContent: JWK
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