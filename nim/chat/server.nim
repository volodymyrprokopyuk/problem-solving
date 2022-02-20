import std/[strformat, asyncdispatch, asyncnet]

type
  Client = ref object
    socket: AsyncSocket
    netAddr: string
    id: int
    connected: bool
  Server = ref object
    socket: AsyncSocket
    clients: seq[Client]

proc newClient(
  socket: AsyncSocket, netAddr: string, id: int, connected: bool): Client =
  Client(socket: socket, netAddr: netAddr, id: id, connected: connected)

func `$`(client: Client): string = fmt "{client.id} ({client.netAddr})"

proc newServer(): Server =
  Server(socket: newAsyncSocket(), clients: @[])

proc processMessages(server: Server, client: Client) {.async.} =
  while true:
    let line = await client.socket.recvLine # pause and poll
    if line.len == 0:
      echo "Client ", client, " disconnected"
      client.connected = false
      client.socket.close
      return
    echo "Client ", client, " sent ", line
    for c in server.clients:
      if c.id != client.id and c.connected:
        await c.socket.send(line & "\c\l") # pause and poll

proc run(server: Server, port = 7687) {.async.} =
  let socket = server.socket
  socket.bindAddr(port.Port)
  socket.listen
  while true:
    let (clAddr, clSocket) = await socket.acceptAddr # pause and poll
    let client = newClient(clSocket, clAddr, server.clients.len, true)
    echo "Connection from client ", client
    server.clients.add(client)
    asyncCheck processMessages(server, client) # run code in the background

let server = newServer()
waitFor run(server) # pause and poll + run event loop
