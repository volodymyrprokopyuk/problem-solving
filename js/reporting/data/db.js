import pg from "pg"
const { Pool } = pg

const config = {
  host: "localhost", port: 5432, database: "timeseries",
  user: "postgres", password: null
}

const pool = new Pool(config)

export async function getData() {
  const { rows } = await pool.query("SELECT 1 a, 'one' b;")
  return rows
}
