import { Config } from "./pulumi"
import * as k8s from "@pulumi/kubernetes"

export function main(config: Config, environment: string) {
  const labels = {
    app: config.appLabel,
    environment: environment
  }

  const monitoringNs = new k8s.core.v1.Namespace(config.namespace, {
    metadata: {
      labels: labels,
      name: config.namespace,
    },
  })

  const monitoring = new k8s.helm.v3.Release(config.releaseName, {
    chart: config.chart,
    namespace: monitoringNs.metadata.name,
    repositoryOpts: {
      repo: config.grafanaRepo,
    },
    skipCrds: true,
    values: config.values,
  })
}