import pg from "pg"
const { Pool } = pg

const config = {
  host: "localhost", port: 5432, database: "timeseries",
  user: "postgres", password: null
}

const pool = new Pool(config)

export async function nodeCpuMetrics() {
  const query = `
    SELECT time_bucket('1 minute', u.time) tbucket, u.job,
      substring(u.instance FROM '^[^-]+') igroup,
      avg(u.idle) idle, avg(u.system + u.user) system_user, avg(u.iowait) iowait
    FROM node_cpu_utilization_wide u
    WHERE u.instance ~* '^(?:mongodb).+'
    GROUP BY u.job, igroup, tbucket
    ORDER BY u.job, igroup, tbucket;
  `
  const { rows } = await pool.query(query)
  return rows
}
