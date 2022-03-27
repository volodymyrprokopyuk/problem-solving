import std/[strutils, strformat, sequtils, times, random, db_sqlite]

type
  User* = object
    uname*: string
    following*: seq[string]
  Message* = object
    uname*: string
    mtext*: string
    mtime*: Time

func newUser*(uname: string, following: seq[string] = @[]): User =
  User(uname: uname, following: following)

proc newMessage*(
  uname, mtext: string,
  mtime = getTime() + initDuration(days = rand(-5..5))): Message =
  Message(uname: uname, mtext: mtext, mtime: mtime)

randomize()

let db = "tweet.db".open("", "", "")

proc create*(user: User) =
  db.exec(sql"INSERT INTO user(uname) VALUES(?);", user.uname)

proc post*(message: Message) =
  if message.mtext.len > 140:
    raise newException(ValueError, "Message must be less than 140 chars")
  db.exec(
    sql"INSERT INTO message(uname, mtime, mtext) VALUES (?, ?, ?);",
    message.uname, message.mtime.toUnix.int, message.mtext)

proc follow*(uname, following: string) =
  db.exec(
    sql"INSERT INTO follower(uname, following) VALUES (?, ?);",
    uname, following)

proc unfollow*(uname, unfollow: string) =
  db.exec(
    sql"DELETE FROM follower WHERE uname = ? AND following = ?;",
    uname, unfollow)

proc findUser*(uname: string, user: var User): bool =
  let duser = db.getRow(
    sql"SELECT uname FROM user WHERE uname = ?;", uname)
  if duser[0].len == 0: return false
  user.uname = duser[0]
  user.following = @[]
  let following = db.getAllRows(
    sql"SELECT following FROM follower WHERE uname = ?;", uname)
  for fuser in following: user.following.add(fuser[0])
  return true

proc findMessages*(unames: openArray[string], limit = 5): seq[Message] =
  let dmsgs = db.getAllRows(
    sql("""
    SELECT uname, mtext, mtime FROM message WHERE uname IN ($1)
    ORDER BY mtime DESC LIMIT $2;
    """ % [unames.mapIt(fmt "'{it}'").join(", "), $limit]))
  for dmsg in dmsgs:
    result.add(newMessage(dmsg[0], dmsg[1], dmsg[2].parseInt.fromUnix))

when isMainModule:
  let
    vlad = newUser("Vlad")
    lana = newUser("Lana")
    lada = newUser("Lada")
  vlad.create
  lana.create
  lada.create
  lada.uname.follow(vlad.uname)
  lada.uname.follow(lana.uname)
  var user: User
  echo findUser("Lada", user)
  echo user

  lana.uname.follow(vlad.uname)
  echo findUser("Lana", user)
  echo user
  lana.uname.unfollow(vlad.uname)
  echo findUser("Lana", user)
  echo user

  post(newMessage("Vlad", "Nim is powerful"))
  post(newMessage("Vlad", "stdlib is estensive"))
  post(newMessage("Lana", "Vladyslava is great"))
  post(newMessage("Lana", "I'm healthy"))
  post(newMessage("Lada", "I'm growing"))
  post(newMessage("Lada", "The world is interesting"))
  echo findMessages(["Vlad", "Lana"])
