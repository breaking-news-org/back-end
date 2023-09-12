import { Output } from "@pulumi/pulumi"

export interface AppConfigFileContent {
  app: {
    host: Output<string>
    port: number
  }
  users: {
    email: string
    password: string
  }[]
}