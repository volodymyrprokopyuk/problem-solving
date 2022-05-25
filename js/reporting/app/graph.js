import "/asset/plotly-basic-2.12.1.min.js"

async function nodeCpu(gd, instance) {
  const url = `/metrics/node-cpu?instance=${instance}`
  const res = await fetch(url)
  if (res.status != 200) { throw new Error(`cannot fetch data from ${url}`) }
  const { metrics: [{ tsb, idle, sysuser, iowait, other }] } = await res.json()
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

await nodeCpu("mongodb-cpu", "mongodb")
