import "/asset/plotly-basic-2.12.1.min.js"

async function fetchData(url) {
  const res = await fetch(url)
  if (res.status != 200) { throw new Error(`cannot fetch data from ${url}`) }
  return res.json()
}

async function nodeCpu(gd, instance) {
  const url = `/metrics/node-cpu?instance=${instance}`
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

async function nodeMemory(gd, instance) {
  const url = `/metrics/node-memory?instance=${instance}`
  const { metrics: [{ tsb, free }] } = await fetchData(url)
  const data = [{ name: "Free memory", x: tsb, y: free }]
  const layout = {
    title: `${instance} memory consumtion`,
    yaxis: { range: [0, 100] }
  }
  const config = { toImageButtonOptions: { format: "svg" } }
  Plotly.newPlot(gd, data, layout, config)
}

nodeCpu("mongodb-cpu", "mongodb")
nodeMemory("mongodb-memory", "mongodb")

nodeCpu("bi-cpu", "bi")
nodeMemory("bi-memory", "bi")
