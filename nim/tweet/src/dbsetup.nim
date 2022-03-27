# Run: rm tweet.db && nim r src/dbsetup.nim && nim r src/dbmodel.nim

import std/db_sqlite

let db = "tweet.db".open("", "", "")

echo "Creating DB schema..."

db.exec(sql"""
CREATE TABLE IF NOT EXISTS user(
  uname text NOT NULL,
  PRIMARY KEY (uname)
);""")

db.exec(sql"""
CREATE TABLE IF NOT EXISTS message(
  uname text NOT NULL,
  mtime integer NOT NULL,
  mtext text NOT NULL,
  FOREIGN KEY (uname) REFERENCES user(uname)
);""")

db.exec(sql"""
CREATE TABLE IF NOT EXISTS follower(
  uname text NOT NULL,
  following text NOT NULL,
  PRIMARY KEY (uname, following),
  FOREIGN KEY (uname) REFERENCES user(uname),
  FOREIGN KEY (following) REFERENCES user(uname)
);""")

echo "DB schema: SUCCESS"

db.close
