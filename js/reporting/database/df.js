import { mkdir } from "fs/promises"
import { URLSearchParams } from "url"
import { curry, pipe } from "rambda"
import axios from "axios"
const { post: POST } = axios
import pl from "nodejs-polars"
const { DataFrame, col, when, lit, readParquet } = pl

const url = "/api/v1/query"
const baseURL = "http://localhost:9090"

const fetchMetric = curry(async (baseURL, url, query, time) => {
  const req = new URLSearchParams({ query, time })
  const headers = { "Content-Type": "application/x-www-form-urlencoded" }
  const { data } = await POST(url, req, { baseURL, headers })
  return data
})(baseURL, url)

const writeParquet = curry((file, df) => df.writeParquet(file))

const counterAgg = curry((columns, df) =>
  columns.reduce((df, [column, alias]) =>
    df.withColumn(
      when(col(column).diff(1).gtEq(0))
        .then(col(column).diff(1))
        .otherwise(col(column))
        .cumSum().alias(alias)), df)
)

const counterRate = curry((time, every, columns, df) => {
  const aggs = columns.map(([column, alias]) =>
    col(column).max().sub(col(column).min())
      .div(col(time).max().sub(col(time).min()))
      .mul(1000).alias(alias))
  return df.groupByDynamic({ indexColumn: time, every }).agg(...aggs)
})

// Node CPU

async function fetchNodeCpu(backInterval, endTime) {
  const query = `node_cpu_seconds_total[${backInterval}]`
  return fetchMetric(query, endTime)
}

function transformNodeCpu(json) {
  const flat = json.map(({ metric: { job, instance, cpu, mode }, values }) =>
    values.map(([ts, value]) => {
      return {
        job, instance, cpu, mode,
        ts: new Date(ts * 1000), value: parseFloat(value)
      }
    })
  ).flat()
  return pl.readRecords(flat)
    .groupBy("job", "instance", "cpu", "ts").pivot("mode", "value").first()
}

export async function archiveNodeCpu(backInterval, endTime, path) {
  const file = `${path}/${endTime}-node-cpu.parquet`
  const { data: { result } } = await fetchNodeCpu(backInterval, endTime)
  await mkdir(path, { recursive: true })
  pipe(transformNodeCpu, writeParquet(file))(result)
}

const analyzeNodeCpu = curry((instance, df) => {
  df = df.filter(
    col("instance").str.contains(`^${instance}.+`)
      .and(col("cpu").eq(lit("0")))
  ).sort("ts")
  df = pipe(
    counterAgg([
      ["idle", "idleCounter"],
      ["system", "systemCounter"],
      ["user", "userCounter"],
      ["iowait", "iowaitCounter"]
    ]),
    counterRate("ts", "30s", [
      ["idleCounter", "idleRate"],
      ["systemCounter", "systemRate"],
      ["userCounter", "userRate"],
      ["iowaitCounter", "iowaitRate"]
    ])
  )(df)
  return df.withColumn(
    col("systemRate").add(col("userRate")).alias("sysUserRate"))
})

function formatNodeCpu(df) {
  return {
    ts: df["ts"].toArray(),
    idle: df["idleRate"].toArray(),
    sysuser: df["sysUserRate"].toArray(),
    iowait: df["iowaitRate"].toArray()
  }
}

export async function nodeCpuAnalytics(endTime, instance) {
  const file = `data/${endTime}-node-cpu.parquet`
  return pipe(readParquet, analyzeNodeCpu(instance), formatNodeCpu)(file)
}

// Node memory

async function fetchNodeMemory(backInterval, endTime) {
  const query = `node_memory_MemFree_bytes[${backInterval}]`
  return fetchMetric(query, endTime)
}

function transformNodeMemory(json) {
  const flat = json.map(({ metric: { job, instance }, values }) =>
    values.map(([ts, value]) => {
      return {
        job, instance, ts: new Date(ts * 1000), value: parseFloat(value)
      }
    })
  ).flat()
  return pl.readRecords(flat)
}

export async function archiveNodeMemory(backInterval, endTime, path) {
  const file = `${path}/${endTime}-node-memory.parquet`
  const { data: { result } } = await fetchNodeMemory(backInterval, endTime)
  await mkdir(path, { recursive: true })
  pipe(transformNodeMemory, writeParquet(file))(result)
}

const analyzeNodeMemory = curry((instance, df) => {
  const maxMemory = 48 * 2 ** 30
  return df.filter(col("instance").str.contains(`^${instance}.+`))
    .groupByDynamic({ indexColumn: "ts", every: "15s" })
    .agg(col("value").mean().alias("free"))
    .sort("ts")
    .withColumn(
      lit(maxMemory).sub(col("free")).div(maxMemory).mul(100).alias("used"))
})

function formatNodeMemory(df) {
  return { ts: df["ts"].toArray(), used: df["used"].toArray() }
}

export async function nodeMemoryAnalytics(endTime, instance) {
  const file = `data/${endTime}-node-memory.parquet`
  return pipe(readParquet, analyzeNodeMemory(instance), formatNodeMemory)(file)
}

// try {
//   let backInterval = "60m"
//   let endTime = "2022-04-27T03:25:00Z"
//   await archiveNodeCpu(backInterval, endTime, "data")
//   await archiveNodeMemory(backInterval, endTime, "data")
//   // let backInterval = "35m"
//   // let endTime = "2022-06-29T21:25:00Z"
//   // await archiveNodeCpu(backInterval, endTime, "data")
//   // await archiveNodeMemory(backInterval, endTime, "data")
//   console.log(pl.readParquet(`data/${endTime}-node-cpu.parquet`))
//   console.log(pl.readParquet(`data/${endTime}-node-memory.parquet`))
// } catch (e) { console.error(e) }

// try {
//   let endTime = "2022-04-27T03:25:00Z"
//   // console.log(await nodeMemoryAnalytics(endTime, "mongodb"))
//   console.log(await nodeCpuAnalytics(endTime, "mongodb"))
// } catch (e) { console.error(e) }
