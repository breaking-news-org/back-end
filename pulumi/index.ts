// """
// Creating a Kubernetes Deployment
// """

import * as pulumi from "@pulumi/pulumi"
import * as k8s from "@pulumi/kubernetes"
import { parse } from "yaml"
import { readFileSync } from "node:fs"
import * as Back from "./src/back/index"
import * as Test from "./src/test/index"
import * as Postgres from "./src/postgres/index"
import * as Monitoring from "./src/monitoring/index"

interface Digests {
  back: string
  test: string
}

function mkSetup(
  environment: string,
  digests: Digests,
  provider: k8s.Provider
) {
  const config = new pulumi.Config(environment)

  const postgres = Postgres.main(
    config.requireObject("postgres"),
    environment,
    provider
  )

  const back = Back.main(
    config.requireObject("back"),
    environment,
    postgres.host,
    postgres.port,
    digests.back,
    provider
  )

  if (environment == "dev") {
    Test.main(
      config.requireObject("test"),
      environment,
      back.host,
      back.port,
      digests.test,
      provider
    )
  }

  const monitoring = Monitoring.main(
    config.requireObject("monitoring"),
    environment
  )
}

// didn't work with microk8s
// worked with minikube
// const provider = new k8s.Provider("k8s-yaml-rendered", {
//   renderYamlToDirectory: "yaml",
// })
const provider = new k8s.Provider("kubernetes-provider")

const digests: Digests = parse(readFileSync("digests.yaml", "utf8"))

mkSetup(pulumi.getStack(), digests, provider)
