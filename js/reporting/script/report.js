import "/static/asset/plotly-2.12.1.min.js"

// Fetch
const url = "/metric/node-cpu"
const res = await fetch(url)
if (res.status != 200) { throw new Error(`cannot fetch data from ${url}`) }
const { metrics } = await res.json()

// Transform
const tbucket = metrics.map(({ tbucket }) => tbucket)
const idle = {
  name: "Idle",
  x: tbucket, y: metrics.map(({ idle }) => idle)
}
const iowait = {
  name: "IO wait",
  x: tbucket, y: metrics.map(({ iowait }) => iowait)
}
const systemUser = {
  name: "Sytem + user",
  x: tbucket, y: metrics.map(({ system_user }) => system_user)
}

// Plot
const data = [idle, iowait, systemUser]
const layout = { title: "Time series (metrics)" }
const config = { toImageButtonOptions: { format: "svg" } }
Plotly.newPlot("plot", data, layout, config)
