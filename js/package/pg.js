import pg from "pg"
const { Client, Pool } = pg

const config = {
  host: "localhost", port: 5432, database: "timeseries",
  user: "postgres", password: null
}
const query = "SELECT $1::integer a, $2::text b;"
const qargs = [1, "a"]

const db = new Client(config) // individual client
await db.connect()
try {
  let res = await db.query(query, qargs)
  console.log(res.rowCount, res.rows)
} catch (e) {
  console.error(`ERROR: ${e}`)
} finally {
  await db.end() // client must be disconnected
}

const pool = new Pool(config) // multiple reusable connections
const db2 = await pool.connect() // reusable client
try {
  let res = await db2.query(query, qargs)
  console.log(res.rowCount, res.rows)
} catch (e) {
  console.error(`ERROR: ${e}`)
} finally {
  await db2.release() // reusable client must be released
}

try { // any client one time query + realse (not for transactions)
  const { rows } = await pool.query("SELECT $1::jsonb j", [{a: "b"}])
  console.log(rows)
} catch (e) {
  console.error(`ERROR: ${e}`)
}
await pool.end() // shutdown pool
