import { json } from "./util.js"
import { getData } from "../data/db.js"

export async function aData(res, req) {
  json(res, 200, await getData())
}
