import { promisify } from "util"
import { readFile as readFileCb } from "fs"
import { App } from "uWebSockets.js"

const app = App()
const port = 7654

app.get("/", async (res, req) => { // static file server
  res.onAborted(_ => res.aborted = true) // must on async
  try {
    const html = await readFile("index.html")
    res.writeStatus("200 OK").writeHeader("Content-Type", "text/html").end(html)
  } catch (e) { // error handling
    res.writeStatus("500 Internal Server Error").end(e.toString())
  }
})

app.get("/:param", (res, req) => { // params, headers, query string
  res.writeStatus("200 OK").writeHeader("Header", "Value")
  res.write(req.getParameter(0) + req.getQuery())
  res.end(req.getHeader("user-agent"))
})

app.post("/json", async (res, req) => { // receive / return JSON
  res.onAborted(_ => res.aborted = true)
  try {
    let json = await parseJson(res)
    res.writeStatus("200 OK").end(JSON.stringify(json))
  } catch (e) {
    res.writeStatus("500 Internal Server Error").end(e.toString())
  }
})

app.get("/middleware/:user", middleware(logRequest, authenticate))

app.any("/*", (res, req) => { // catch all route
  res.writeStatus("404 Not Found").end()
})

app.listen(port, (socket) => {
  if (socket) { console.log("Listening on port " + port) }
  else { console.error("ERROR: Failed to listen on port " + port) }
})

const readFile = promisify(readFileCb)

async function parseJson(res) {
  return new Promise((resolve, reject) => {
    let buffer
    res.onData((data, isLast) => {
      const chunk = Buffer.from(data)
      buffer = buffer ? Buffer.concat([buffer, chunk]) : chunk
      if (isLast) {
        try { resolve(JSON.parse(buffer)) }
        catch (e) { reject(e) }
      }
    })
  })
}

function middleware(...m) {
  return async (res, req) => {
    res.onAborted(_ => res.aborted = true)
    try {
      for (const f of m) { await f(res, req) }
    } catch (e) {
      res.writeStatus("500 Internal Server Error").end(e.toString())
    }
  }
}

async function logRequest(res, req) {
  console.log(`${req.getMethod()} ${req.getUrl()}`)
  res.user = req.getParameter(0)
}

async function authenticate(res, req) {
  if (res.user !== "anonymous") { throw new Error("unauthenticated") }
  res.end("authenticated")
}
