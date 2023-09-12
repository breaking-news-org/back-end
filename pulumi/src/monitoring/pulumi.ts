export interface Config {
  namespace: string
  appLabel: string
  releaseName: string
  grafanaRepo: string
  chart: string
  grafanaService: {
    port: number
    targetPort: number
    nodePort: number
  }
  values: {
    loki: {
      enabled: boolean
      isDefault: boolean
      url: string
    }
    promtail: {
      enabled: true
      config: {
        serverPort: number
        clients: [
          {
            url: string
          }
        ]
      }
    }
    grafana: {
      enabled: true
      sidecar: {
        datasources: {
          enabled: true
        }
      }
      image: {
        tag: string
      }
    }
  }
}