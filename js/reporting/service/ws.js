import { App } from "uWebSockets.js"
import { mw, qs, checkListen, notFound } from "./util.js"
import {
  asset, tsNodeCpu, tsNodeMemory, plNodeCpu, plNodeMemory, report
} from "./route.js"

const port = 7654

const ws = App()

ws.get("/asset/:file", mw(asset))
ws.get("/metrics/ts-node-cpu", mw(qs, tsNodeCpu))
ws.get("/metrics/ts-node-memory", mw(qs, tsNodeMemory))
ws.get("/metrics/pl-node-cpu", mw(qs, plNodeCpu))
ws.get("/metrics/pl-node-memory", mw(qs, plNodeMemory))
ws.get("/report", mw(report))
ws.any("/*", mw(notFound))

ws.listen(port, checkListen(port))
