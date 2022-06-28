import { file, render, json } from "./util.js"
import { nodeCpuMetrics, nodeMemoryMetrics } from "../database/db.js"
import { nodeCpuAnalytics, nodeMemoryAnalytics } from "../database/df.js"

export async function asset(res, req) {
  file(res, `app/${req.getParameter(0)}`)
}

export async function tsNodeCpu(res, req) {
  json(res, 200, { metrics: await nodeCpuMetrics(res.qs.instance) })
}

export async function tsNodeMemory(res, req) {
  json(res, 200, { metrics: await nodeMemoryMetrics(res.qs.instance) })
}

export async function plNodeCpu(res, req) {
  json(res, 200, { metrics: await nodeCpuAnalytics(res.qs.instance) })
}

export async function plNodeMemory(res, req) {
  json(res, 200, { metrics: await nodeMemoryAnalytics(res.qs.instance) })
}

export async function report(res, req) {
  render(res, 200, "app/index.html", { title: "Performance analysis" })
}
