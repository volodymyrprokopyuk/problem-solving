import { file, render, json } from "./util.js"
import { nodeCpuMetrics } from "./db.js"

export async function asset(res, req) {
  file(res, `${req.getParameter(0)}/${req.getParameter(1)}`)
}

export async function nodeCpu(res, req) {
  json(res, 200, { metrics: await nodeCpuMetrics() })
}

export async function report(res, req) {
  render(res, 200, "asset/index.html", { title: "Report" })
}
