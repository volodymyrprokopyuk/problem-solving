import "/asset/plotly-basic-2.12.1.min.js"

async function fetchData(url) {
  const res = await fetch(url)
  if (res.status != 200) { throw new Error(`cannot fetch data from ${url}`) }
  return res.json()
}

async function seNodeCpu(gd, endTime, instance) {
  const url = `/metrics/node-cpu?endTime=${endTime}&instance=${instance}`
  const { metrics: { ts, idle, sysuser, iowait } } = await fetchData(url)
  const data = [
    { name: "Idle", x: ts, y: idle },
    { name: "System + user", x: ts, y: sysuser },
    { name: "IO wait", x: ts, y: iowait }
  ]
  const layout = { title: `CPU utilization (${endTime})` }
  const config = { toImageButtonOptions: { format: "svg" } }
  Plotly.newPlot(gd, data, layout, config)
}

async function seNodeMemory(gd, endTime, instance) {
  const url = `/metrics/node-memory?endTime=${endTime}&instance=${instance}`
  const { metrics: { ts, used } } = await fetchData(url)
  const data = [{ name: "Used memory", x: ts, y: used }]
  const layout = {
    title: `Memory consumption (${endTime})`,
    yaxis: { range: [0, 100] }
  }
  const config = { toImageButtonOptions: { format: "svg" } }
  Plotly.newPlot(gd, data, layout, config)
}

async function ceNodeMemory(gd, executions) {
  const req = executions.map(({ endTime, instance }) => {
    const url = `/metrics/node-memory?endTime=${endTime}&instance=${instance}`
    return fetchData(url)
  })
  const [
    { metrics: { ts: ts1, used: used1 } },
    { metrics: { ts: ts2, used: used2 } }
  ] = await Promise.all(req)
  const data = [
    { name: "Execution 1", x: ts1, y: used1 },
    { name: "Execution 2", x: ts2, y: used2, xaxis: "x2" }
  ]
  const [{ endTime: endTime1 }, { endTime: endTime2 }] = executions;
  const layout = {
    title: `Execution 1 (${endTime1}) vs Execution 2 (${endTime2})`,
    xaxis2: { overlaying: "x", side: "top" },
    yaxis: { range: [0, 100] }
  }
  const config = { toImageButtonOptions: { format: "svg" } }
  Plotly.newPlot(gd, data, layout, config)
}

const memExecutions = [
  { endTime: "2022-04-27T03:25:00Z", db: "mongodb", pay: "bi" },
  { endTime: "2022-06-29T21:25:00Z", db: "10.224.0.249", pay: "10.224.0.220"}
]
const { endTime: memEndTime, db: memDb, pay: memPay } = memExecutions[0]

// seNodeCpu("se-db-cpu", endTime, "mongodb")
seNodeMemory("se-db-memory", memEndTime, memDb)
// seNodeCpu("se-pay-cpu", endTime, instance)
seNodeMemory("se-pay-memory", memEndTime, memPay)

ceNodeMemory("ce-db-memory", memExecutions.map(
  ({ endTime, db }) => { return { endTime, instance: db } }
))
ceNodeMemory("ce-pay-memory", memExecutions.map(
  ({ endTime, pay }) => { return { endTime, instance: pay } }
))

// let backend = "ts"
// nodeCpu("ts-db-cpu", backend, "mongodb")
// nodeMemory("ts-db-memory", backend, "mongodb")
// nodeCpu("ts-pay-cpu", backend, "bi")
// nodeMemory("ts-pay-memory", backend, "bi")
