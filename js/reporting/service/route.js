import { file, render, json } from "./util.js"
import { nodeCpuMetrics, nodeMemoryMetrics } from "../database/db.js"

export async function asset(res, req) {
  file(res, `app/${req.getParameter(0)}`)
}

export async function nodeCpu(res, req) {
  json(res, 200, { metrics: await nodeCpuMetrics(res.qs.instance) })
}

export async function nodeMemory(res, req) {
  json(res, 200, { metrics: await nodeMemoryMetrics(res.qs.instance) })
}

export async function report(res, req) {
  render(res, 200, "app/index.html", { title: "Performance analysis" })
}
