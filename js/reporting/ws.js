import { App } from "uWebSockets.js"
import { mw, checkListen, notFound } from "./util.js"
import { asset, nodeCpu, report } from "./route.js"

const port = 7654

const ws = App()

ws.get("/static/:dir/:file", mw(asset))
ws.get("/metric/node-cpu", mw(nodeCpu))
ws.get("/report", mw(report))
ws.any("/*", mw(notFound))

ws.listen(port, checkListen(port))
