import { App } from "uWebSockets.js"
import { mw, checkListen, notFound } from "./util.js"
import { aData } from "./route.js"

const port = 7654

const ws = App()

ws.get("/data", mw(aData))

ws.any("/*", mw(notFound))

ws.listen(port, checkListen(port))
