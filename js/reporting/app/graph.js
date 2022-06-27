import "/asset/plotly-basic-2.12.1.min.js"

async function fetchData(url) {
  const res = await fetch(url)
  if (res.status != 200) { throw new Error(`cannot fetch data from ${url}`) }
  return res.json()
}

async function nodeCpu(gd, backend, instance) {
  const url = `/metrics/${backend}-node-cpu?instance=${instance}`
  const { metrics: [{ tsb, idle, sysuser, iowait, other }] } =
        await fetchData(url)
  const data = [
    { name: "Idle", x: tsb, y: idle },
    { name: "System + user", x: tsb, y: sysuser },
    { name: "IO wait", x: tsb, y: iowait },
    { name: "Other", x: tsb, y: other },
  ]
  const layout = { title: `${instance} CPU utilization` }
  const config = { toImageButtonOptions: { format: "svg" } }
  Plotly.newPlot(gd, data, layout, config)
}

async function nodeMemory(gd, backend, instance) {
  const url = `/metrics/${backend}-node-memory?instance=${instance}`
  const { metrics: [{ tsb, used }] } = await fetchData(url)
  const data = [{ name: "Used memory", x: tsb, y: used }]
  const layout = {
    title: `${instance} memory consumtion`,
    yaxis: { range: [0, 100] }
  }
  const config = { toImageButtonOptions: { format: "svg" } }
  Plotly.newPlot(gd, data, layout, config)
}

let backend = "ts"
// nodeCpu("ts-mongodb-cpu", backend, "mongodb")
nodeMemory("ts-mongodb-memory", backend, "mongodb")
// nodeCpu("ts-bi-cpu", backend, "bi")
nodeMemory("ts-bi-memory", backend, "bi")

backend = "pl"
// nodeCpu("pl-mongodb-cpu", backend, "mongodb")
nodeMemory("pl-mongodb-memory", backend, "mongodb")
// nodeCpu("pl-bi-cpu", backend, "bi")
nodeMemory("pl-bi-memory", backend, "bi")
