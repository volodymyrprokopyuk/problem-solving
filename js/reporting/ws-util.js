import { promisify } from "util"

export function mw(...handlers) {
  return async (res, req) => {
    res.onAborted(_ => res.aborted = true)
    try {
      for (const h of [logRequest, ...handlers]) { await h(res, req) }
    } catch (e) {
      console.error("ERROR:", e)
      const error = { error: e.toString() }
      res.writeStatus("500 Internal Server Error").end(JSON.stringify(error))
    }
  }
}

export function logRequest(res, req) {
  throw new Error("oh")
  console.log(new Date(Date.now()), req.getMethod(), req.getUrl())
}

export function notFound(res, req) {
  const error = { error: `${req.getMethod()} ${req.getUrl()} not found` }
  res.writeStatus("404 Not Found").end(JSON.stringify(error))
}
