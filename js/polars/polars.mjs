import { curry, pipe } from "rambda"
import pl from "nodejs-polars"
const { DataFrame, col, when } = pl

// const df = new DataFrame({a: [1, 2, 3], b: [4, 5, 6]})
// console.log(df, df.columns, df.dtypes, df.height, df.width)
// const dfBuf = df.serialize("json")
// console.log(String(dfBuf))
// const df2 = DataFrame.deserialize(dfBuf, "json")
// console.log(df2)

// const data = [{
//   "id": "a",
//   "name": "fred",
//   "country": "france",
//   "age": 30,
//   "city": "paris"
// }, {
//   "id": "b",
//   "name": "alexandra",
//   "country": "usa",
//   "age": 40
// }, {
//   "id": "c",
//   "name": "george",
//   "country": "argentina",
//   "age": 50
// }]
// const dataBuf = data.map(o => JSON.stringify(o)).join("\n")
// console.log(dataBuf)
// const df = pl.readJSON(dataBuf)
// console.log(df)
// const df2 = pl.readRecords(data)
// console.log(df2)

// const df = new DataFrame({
//   a: [13520, 13472, 13456, 13472, 13472],
//   b: [-16, -16, -16, -16, -16],
//   c: [384, 384, 368, 368, 352],
//   d: [208, 176, 160, 160, 176],
// })
// console.log(df)
// const cols = ["a", "b"]
// const df2 = df.select(
//   col("^c|d$"),
//   col("*").exclude("c", "d").suffix("Ex"),
//   col(cols), col(cols).mean().suffix("M"),
//   col(cols).sub(col(cols).mean()).suffix("C")
// )
// console.log(df2)

// const df = new DataFrame({
//   group: ["a", "a", "b", "b"],
//   name: ["foo", "bar", "baz", "boo"],
//   value: [1, 2, 1.5, -1]
// })
// console.log(df)
// const df2 = df.groupBy("group").agg(col("*").sortBy("value").last())
//       .sort("group")
// console.log(df2)
// const df3 = df.select(col("*").sortBy("value").last().over("group"))
//       .unique().sort("group", true)
// console.log(df3)

// const df = new DataFrame({
//   arrival: [
//     new Date("1970-01-01 08:00:00"),
//     new Date("1970-01-01 19:00:00"),
//     new Date("1970-01-01 21:00:00")
//   ],
//   train: [112, 134, 156]
// })
// console.log(df)
// const df2 = new DataFrame({
//   arrival2: [new Date("1970-01-01 08:00:00"), new Date("1970-01-01 19:00:00")],
//   period: ["early", "afternoon"]
// })
// console.log(df2)
// const df3 = df.join(
//   df2.rename({"arrival2": "arrival"}).withColumns(col("arrival").alias("arr")),
//   { on: "arrival", how: "left" }
// )
// console.log(df3)

// const df = new DataFrame({ a: ["foo", "bar", "ham"] })
// const df2 = df.withColumn(col("a").cast(pl.Categorical).alias("category"))
//       .filter(col("a").isIn(["bar", "ham"]))
// console.log(df2)

// const df = new DataFrame({
//   foo: ["one", "one", "one", "two", "two", "two"],
//   bar: ["A", "B", "C", "A", "B", "C"],
//   baz: [1, 2, 3, 4, 5, 6],
// })
// console.log(df)
// const df2 = df.groupBy("foo").pivot("bar", "baz").first()
// console.log(df2)
// // const df3 = df.pivot("baz", "foo", "bar")
// // console.log(df3)
// const df4 = df2.melt("foo", ["A", "B", "C"]).sort("value")
// console.log(df4)

// const df = new DataFrame([
//   [new Date("2022-06-08T00:00:01"), 0],
//   [new Date("2022-06-08T00:00:06"), 2],
//   [new Date("2022-06-08T00:00:11"), 3],
//   [new Date("2022-06-08T00:00:16"), 6],
//   [new Date("2022-06-08T00:00:21"), 2],
//   [new Date("2022-06-08T00:00:26"), 4],
//   [new Date("2022-06-08T00:00:31"), 7],
//   [new Date("2022-06-08T00:00:36"), 9],
//   [new Date("2022-06-08T00:00:41"), 3],
//   [new Date("2022-06-08T00:00:46"), 4],
//   [new Date("2022-06-08T00:00:51"), 6],
//   [new Date("2022-06-08T00:00:56"), 7],
// ], { orient: "row", columns: ["ts", "val"] })
// console.log(df)
// function counterAgg(columns, df) {
//   return columns.reduce((df, [column, alias]) =>
//     df.withColumn(
//       when(col(column).diff(1).gtEq(0))
//         .then(col(column).diff(1))
//         .otherwise(col(column))
//         .cumSum().alias(alias)), df)
// }
// function counterRate(time, every, columns, df) {
//   const aggs = columns.map(([column, alias]) =>
//     col(column).max().sub(col(column).min())
//       .div(col(time).max().sub(col(time).min()))
//       .mul(1000).alias(alias))
//   return df.groupByDynamic({ indexColumn: time, every }).agg(...aggs)
// }
// const makeCounterAgg = curry(counterAgg)
// const makeCounterRate = curry(counterRate)
// const df2 = pipe(
//   makeCounterAgg([["val", "valCounter"], ["val", "valCounter2"]]),
//   makeCounterRate("ts", "10s",
//                   [["valCounter", "valRate"], ["valCounter2", "valRate2"]])
// )(df)
// console.log(df2)


const df = new DataFrame([
  ["a", new Date("2022-06-08T00:00:01"), 0],
  ["a", new Date("2022-06-08T00:00:06"), 2],
  ["a", new Date("2022-06-08T00:00:11"), 3],
  ["a", new Date("2022-06-08T00:00:16"), 6],
  ["a", new Date("2022-06-08T00:00:21"), 2],
  ["a", new Date("2022-06-08T00:00:26"), 4],
  ["b", new Date("2022-06-08T00:00:31"), 7],
  ["b", new Date("2022-06-08T00:00:36"), 9],
  ["b", new Date("2022-06-08T00:00:41"), 3],
  ["b", new Date("2022-06-08T00:00:46"), 4],
  ["b", new Date("2022-06-08T00:00:51"), 6],
  ["b", new Date("2022-06-08T00:00:56"), 7],
], { orient: "row", columns: ["group", "ts", "value"] })
console.log(df)
// const df2 = df.groupBy("group").agg(
//   col("value").sortBy("ts").cumSum().alias("x")
// )
// console.log(df2)
