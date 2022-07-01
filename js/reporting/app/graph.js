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
  const layout = { title: `${instance} CPU utilization (${endTime})` }
  const config = { toImageButtonOptions: { format: "svg" } }
  Plotly.newPlot(gd, data, layout, config)
}

async function seNodeMemory(gd, endTime, instance) {
  const url = `/metrics/node-memory?endTime=${endTime}&instance=${instance}`
  const { metrics: { ts, used } } = await fetchData(url)
  const data = [{ name: "Used memory", x: ts, y: used }]
  const layout = {
    title: `${instance} memory consumtion (${endTime})`,
    yaxis: { range: [0, 100] }
  }
  const config = { toImageButtonOptions: { format: "svg" } }
  Plotly.newPlot(gd, data, layout, config)
}

const endTime = "2022-04-27T03:25:00Z"
seNodeCpu("se-mongodb-cpu", endTime, "mongodb")
seNodeMemory("se-mongodb-memory", endTime, "mongodb")
seNodeCpu("se-bi-cpu", endTime, "bi")
seNodeMemory("se-bi-memory", endTime, "bi")

// let backend = "ts"
// nodeCpu("ts-mongodb-cpu", backend, "mongodb")
// nodeMemory("ts-mongodb-memory", backend, "mongodb")
// nodeCpu("ts-bi-cpu", backend, "bi")
// nodeMemory("ts-bi-memory", backend, "bi")
