export interface AppConfigFile {
  env: string
  dataBase: {
    db: string
    user: string
    password: string
    dbName: string
    numConns: number
  }
  web: {
    port: number
    pageSize: number
    staticContent: string
  }
  jwtParameters: {
    expirationTime: number
  }
  admins: [{ userName: string; password: string }]
}

