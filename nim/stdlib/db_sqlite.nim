import std/[strutils, db_sqlite]

let db = "stdlib/db_sqlite.db".open("", "", "")

db.exec(sql"""
  CREATE TABLE IF NOT EXISTS person(
    person_name text NOT NULL,
    person_height int NOT NULL,
    PRIMARY KEY (person_name)
  );
""")

var query = sql"""
  INSERT INTO person(person_name, person_height)
  VALUES (?, ?), (?, ?), (?, ?);
"""
db.exec(query, "Vlad", 172, "Lana", 165, "Lada", 52)

# Get single Row = seq[string]
let rawCount = db.getRow(sql"SELECT count(*) FROM person;")
if rawCount[0] != "":
  let count = rawCount[0].parseInt
  echo count

query = sql"SELECT person_name, person_height FROM person;"
# Get all rows seq[Row = seq[string]]
let people = db.getAllRows(query)
for person in people:
  echo person[0], " ", person[1].parseInt

query = sql"""
  SELECT  person_name, person_height FROM person
  WHERE person_name = ?;
"""
# Slower, but safe iterator (allows break)
for person in db.rows(query, "Vlad"):
  echo person[0], " ", person[1].parseInt

query = sql"""
  SELECT  person_name, person_height FROM person
  WHERE person_name IN (?, ?);
"""
# Fast, but unsafe iterator (must consume all rows, no break)
for person in db.fastRows(query, "Lana", "Lada"):
  echo person[0], " ", person[1].parseInt

let squery = """
  SELECT  person_name, person_height FROM person
  WHERE person_name = ? AND person_height = ?;
"""
# Prepared statement (for getAllRows, fastRows, rows)
let ps = db.prepare(squery)
ps.bindParam(1, "Vlad") # 1-based index
ps.bindParam(2, 172)
for person in db.fastRows(ps):
  echo person[0], " ", person[1].parseInt
ps.finalize

db.close
