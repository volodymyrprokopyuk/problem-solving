import { promisify } from "util"
import { extname } from "path"
import { readFile as readFileCb } from "fs"
const readFile = promisify(readFileCb)
import nj from "nunjucks"
const { render: renderFile } = nj

export const statusCode = {
  200: "200 OK",
  404: "404 Not Found",
  500: "500 Internal Server Error"
}

export const contentType = {
  html: "text/html",
  css: "text/css",
  js: "text/javascript",
  json: "application/json"
}

export async function file(res, file) {
  try {
    const content = await readFile(file)
    const ext = extname(file).slice(1)
    res.writeStatus(statusCode[200])
       .writeHeader("Content-Type", contentType[ext]).end(content)
  } catch (e) {
    console.error("ERROR:", e)
    json(res, 404, { error: e.toString() })
  }
}

export async function render(res, status, file, context) {
  const content = await renderFile(file, context)
  res.writeStatus(statusCode[status])
     .writeHeader("Content-Type", contentType.html).end(content)
}

export function json(res, status, json) {
  res.writeStatus(statusCode[status])
     .writeHeader("Content-Type", contentType.json).end(JSON.stringify(json))
}

export function mw(...handlers) {
  return async (res, req) => {
    res.onAborted(_ => res.aborted = true)
    try {
      for (const h of [logRequest, ...handlers]) { await h(res, req) }
    } catch (e) {
      console.error("ERROR:", e)
      json(res, 500, { error: e.toString() })
    }
  }
}

export function checkListen(port) {
  return (socket) => {
    const ts = new Date(Date.now())
    if (socket) { console.log(ts, "Listening on port", port) }
    else { console.error(ts, "ERROR: failed to listen on port", port) }
  }
}

export function logRequest(res, req) {
  console.log(new Date(Date.now()), req.getMethod(), req.getUrl())
}

export function notFound(res, req) {
  json(res, 404, { error: `${req.getMethod()} ${req.getUrl()} not found` })
}
