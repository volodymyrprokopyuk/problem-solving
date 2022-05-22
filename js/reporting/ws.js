import { App } from "uWebSockets.js"
import { mw, notFound } from "./ws-util.js"

const port = 7654

const ws = App()

ws.any("/*", mw(notFound))

ws.listen(port, socket => {
  if (socket) { console.log("Listening on port", port) }
  else { console.error("ERROR: failed to listen on port", port) }
})

