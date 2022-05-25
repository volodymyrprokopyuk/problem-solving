import { App } from "uWebSockets.js"
import { mw, qs, checkListen, notFound } from "./util.js"
import { asset, nodeCpu, report } from "./route.js"

const port = 7654

const ws = App()

ws.get("/asset/:file", mw(asset))
ws.get("/metrics/node-cpu", mw(qs, nodeCpu))
ws.get("/report", mw(report))
ws.any("/*", mw(notFound))

ws.listen(port, checkListen(port))
