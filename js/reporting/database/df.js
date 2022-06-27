import { URLSearchParams } from "url"
import { curry, pipe } from "rambda"
import axios from "axios"
const { post: POST } = axios
import pl from "nodejs-polars"
const { DataFrame, col, when, lit } = pl

const url = "/api/v1/query"
const baseURL = "http://localhost:9090"
const backInterval = "60m"
const endTime = "2022-04-27T03:25:00Z"

const fetchMetric = curry(async (baseURL, url, query, time) => {
  const req = new URLSearchParams({ query, time })
  const headers = { "Content-Type": "application/x-www-form-urlencoded" }
  const { data } = await POST(url, req, { baseURL, headers })
  return data
})(baseURL, url)

// Node memory

async function fetchNodeMemory(backInterval, endTime) {
  const query = `node_memory_MemFree_bytes[${backInterval}]`
  return fetchMetric(query, endTime)
}

function flatNodeMemory(json) {
  return json.map(({ metric: { job, instance }, values }) =>
    values.map(([ts, value]) => {
      return {
        job, instance, ts: new Date(ts * 1000), value: parseFloat(value)
      }
    })
  ).flat()
}

const analyzeNodeMemory = curry((instance, json) =>
  pl.readRecords(json)
    .filter(col("instance").str.contains(`^${instance}.+`))
    .groupByDynamic({ indexColumn: "ts", every: "15s" })
    .agg(pl.avg("value").alias("free"))
    .sort("ts")
    .withColumn(
      lit(16 * 2 ** 30).sub(col("free"))
        .div(16 * 2 ** 30).mul(100).alias("used"))
)

function formatNodeMemory(df) {
  return [{
    tsb: df["ts"].toArray(), used: df["used"].toArray()
  }]
}

export async function nodeMemoryAnalytics(instance) {
  const { data: { result } } = await fetchNodeMemory(backInterval, endTime)
  return pipe(flatNodeMemory, analyzeNodeMemory(instance), formatNodeMemory)
  (result)
}

// Node CPU

async function fetchNodeCpu(backInterval, endTime) {
  const query =
    `node_cpu_seconds_total{instance=~"mongodb.+",cpu=~"0|1"}[${backInterval}]`
  return fetchMetric(query, endTime)
}

function flatNodeCpu(json) {
  return json.map(({ metric: { job, instance, cpu, mode }, values }) =>
    values.map(([ts, value]) => {
      return {
        job, instance, cpu, mode,
        ts: new Date(ts * 1000),
        value: parseFloat(value)
      }
    })
  ).flat()
}

function wideNodeCpu(json) {
  return pl.readRecords(json)
    .groupBy("job", "instance", "cpu", "ts").pivot("mode", "value").first()
}

function analyzeNodeCpu(df) {
  return df.groupBy("job", "instance", "cpu").agg(
    col("idle").sortBy("ts").cumSum().alias("idleCounter"),
    // when(col("idle").diff(1).gtEq(0))
    //   .then(col("idle").diff(1))
    //   .otherwise(col("idle"))
    //   .cumSum().alias("idleCounter"),
  )
}

// try {
//   // const { data: { result } } = await fetchNodeCpu(backInterval, endTime)
//   // pipe(flatNodeCpu, wideNodeCpu, analyzeNodeCpu, console.log)(result)
//   const { data: { result } } = await fetchNodeMemory(backInterval, endTime)
//   pipe(flatNodeMemory, analyzeNodeMemory("mongodb"), formatNodeMemory,
//        console.log)(result)
// } catch (e) { console.error(e) }
