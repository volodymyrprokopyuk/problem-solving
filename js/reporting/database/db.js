import pg from "pg"
const { Pool } = pg

const config = {
  host: "localhost", port: 5432, database: "metrics",
  user: "postgres", password: null
}

const pool = new Pool(config)

export async function nodeCpuMetrics(instance) {
  const query = "SELECT c.* FROM node_cpu($1) c;"
  const { rows } = await pool.query(query, [`^${instance}`])
  return rows.length ? rows[0] : rows
}

export async function nodeMemoryMetrics(instance) {
  const query = "SELECT m.* FROM node_memory($1) m;"
  const { rows } = await pool.query(query, [`^${instance}`])
  return rows.length ? rows[0] : rows
}
