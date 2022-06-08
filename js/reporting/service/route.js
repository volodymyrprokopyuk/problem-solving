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



import { URLSearchParams } from "url"
import axios from "axios"
const { post: POST } = axios
import pl from "nodejs-polars"
const { DataFrame } = pl

const backInterval = "6m"
const query = `node_cpu_seconds_total{instance=~"mongodb.+",cpu=~"0|1"}
[${backInterval}]`
const endTime = "2022-04-27T03:25:00Z"
const url = "/api/v1/query"
const baseURL = "http://localhost:9090"

async function fetchMetric(query, time, url, baseURL) {
  const req = new URLSearchParams({ query, time })
  const headers = { "Content-Type": "application/x-www-form-urlencoded" }
  const { data } = await POST(url, req, { baseURL, headers })
  return data
}

function nodeCpuFlat(data) {
  return data.map(({ metric: { job, instance, cpu, mode }, values }) =>
    { return values.map(([ts, value]) =>
      { return { job, instance, cpu, mode, ts, value } })
    }
  ).flat()
}

function nodeCpuLong(data) {
  const job = data.map(({ job }) => { return job })
  const instance = data.map(({ instance }) => { return instance })
  const cpu = data.map(({ cpu }) => { return cpu })
  const mode = data.map(({ mode }) => { return mode })
  const ts = data.map(({ ts }) => { return new Date(ts * 1000) })
  const value = data.map(({ value }) => { return value })
  return DataFrame({ job, instance, cpu, mode, ts, value })
}

function nodeCpuWide(data) {
  return data
}

// try {
//   const { data: { result } } = await fetchMetric(query, endTime, url, baseURL)
//   const flat = nodeCpuFlat(result)
//   const long = nodeCpuLong(flat)
//   const wide = nodeCpuWide(long)
//   console.log(wide)
// } catch (e){ console.error(e) }
