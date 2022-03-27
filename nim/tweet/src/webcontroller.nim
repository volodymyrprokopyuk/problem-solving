import std/[asyncdispatch, strformat, times]
import pkg/jester
import dbmodel, uiview

settings:
  port = Port(5432)

routes:

  get "/":
    if request.cookies.hasKey("principal"):
      let principal = request.cookies["principal"]
      redirect fmt "/timeline/{principal}"
    else: redirect "/login"

  get "/login":
    resp renderLogin()

  post "/login":
    let principal = @"principal"
    setCookie("principal", principal, expires = now() + 2.minutes)
    redirect fmt "/timeline/{principal}"

  post "/logout":
    setCookie("principal", @"principal", now() - 1.minutes)
    redirect "/login"

  get "/timeline/@uname":
    if request.cookies.hasKey("principal"):
      let
        principal = request.cookies["principal"]
        uname = @"uname"
      var user: User
      if findUser(uname, user):
        let messages = findMessages(user.following & principal)
        resp renderTimeline(user, messages, principal)
      else: redirect "/login"
    else: redirect "/login"

  post "/follow":
    let
      principal = @"principal"
      following = @"following"
    following.follow(principal)
    redirect fmt "/timeline/{principal}"

  post "/unfollow":
    let
      principal = @"principal"
      unfollow = @"unfollow"
    unfollow.unfollow(principal)
    redirect fmt "/timeline/{principal}"

  post "/message":
    if request.cookies.hasKey("principal"):
      let principal = request.cookies["principal"]
      post(newMessage(principal, @"mtext", getTime()))
      redirect fmt "/timeline/{principal}"
    else: redirect "/login"

runForever() # event loop
