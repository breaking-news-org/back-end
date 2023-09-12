import { Config } from "./pulumi"
import * as k8s from "@pulumi/kubernetes"

export function main(
  config: Config,
  namespaceName: string
) {
  const namespace = `${namespaceName}-monitoring`
  new k8s.core.v1.Namespace(namespace, {
    metadata: {
      name: namespace
    }
  })

  new k8s.helm.v3.Release(config.releaseName, {
    chart: config.chart,
    namespace: namespace,
    repositoryOpts: {
      repo: config.grafanaRepo,
    },
    skipCrds: true,
    values: config.values,
  })
}