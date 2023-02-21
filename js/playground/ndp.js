import { promisify } from "util"
import { EventEmitter } from "events"
import { Readable, Transform, PassThrough } from "stream"
import { readFile, createReadStream } from "fs"
import { randomBytes as randomBytesCb } from "crypto"
const randomBytes = promisify(randomBytesCb)
import { createServer } from "http"

// module pattern
const aModule = (function() {
  let v = 0
  function inc() { return ++v }
  return { inc }
})()

// console.log(aModule.inc(), aModule.inc()) // 1, 2
// console.log(aModule.v) // undefined

const modules = {
  a:  `
    let v = 0
    function inc() { return ++v }
    module.exports = { inc }
  `,
  b: `
    const a = requireModule("a")
    function double() { return a.inc() * 2 }
    module.exports = { double }
  `
}

// Node.js require()
function requireModule(moduleId) {
  // Module is loaded and evaluated only once
  if (moduleId in requireModule.cache) {
    return requireModule.cache[moduleId].exports
  }
  // New module consists of empty exports
  const module = { exports: { } }
  requireModule.cache[moduleId] = module
  // Protect module implementation with the module pattern
  const wrappedModule = `
    (function(module) { ${modules[moduleId]} })(module)
  `
  // Evaluate protected module and build module public interface
  eval(wrappedModule)
  // Return module public interface
  return module.exports
}

requireModule.cache = { }

// let bModule = requireModule("b")
// console.log(bModule.double()) // 2
// bModule = requireModule("b")
// console.log(bModule.double()) // 4

// sync vs async callbacks
function syncAdd(a, b, cb) { cb(a + b) }
function asyncAdd(a, b, cb) { setImmediate(() => cb(a + b)) }

// console.log("Before")
// syncAdd(1, 2, console.log) // blocking
// asyncAdd(3, 4, console.log) // non-blocking
// console.log("After") // Before, 3, After, 7

// Function returning EventEmitter
function reMatch(file, re) {
  const ee = new EventEmitter()
  readFile(file, "utf8", (error, data) => {
    if (error) { return ee.emit("error", error) }
    ee.emit("read", file)
    const match = data.match(re)
    if (match) {
      for (const m of match) { ee.emit("match", m, file) }
    }
  })
  return ee
}

// const rem = reMatch("ndp.js", /^.*\bEvent/mg) // async events
// rem.on("read", file => console.log(`read: ${file}`))
// rem.on("match", (match, file) => console.log(`match: '${match}' in ${file}`))
// rem.on("error", error => console.error(`error: ${error}`))

// EventEmitter observable class
class ReMatch extends EventEmitter {
  constructor() { super() } // init EventEmitter
  match(file, re) {
    readFile(file, "utf8", (error, data) => {
      if (error) { return this.emit("error", error) }
      this.emit("read", file)
      const match = data.match(re)
      if (match) {
        for (const m of match) { this.emit("match", m, file) }
      }
    })
  }
}

// const rem = new ReMatch()
// rem.match("ndp.js", /^.*\bEvent/mg) // async events
// rem.on("read", file => console.log(`read: ${file}`))
// rem.on("match", (match, file) => console.log(`match: '${match}' in ${file}`))
// rem.on("error", error => console.error(`error: ${error}`))

function cbTask(x, cb) {
  setTimeout(() => {
    if (x === -1) { return cb("oh") }
    console.log(x); cb(null)
  }, 900)
}

// Sequential iteration pattern
function cbIterate(task, arr, cb) {
  // Sequential interation over an array applying an async operation
  let index = 0
  function iterate() {
    if (index === arr.length) {
      return process.nextTick(() => cb(null)) // always async
    }
    // Recurse for the next interation after completion of the previous one
    task(arr[index++], iterate)
  }
  iterate()
}

// cbIterate(cbTask, [], console.log) // null
// cbIterate(cbTask, [1, 2, 3], console.log) // 1, 2, 3, null

// Parallel execution pattern
function cbParallel(tasks, cb) {
  // Parallel execution of tasks until all complete or first error
  let completed = 0
  let failed = false
  function done(error) {
    if (error) { failed = true; return cb(error) }
    if (++completed === tasks.length && !failed) { return cb(null) }
  }
  for (const task of tasks) { task(done) }
}

// // 1, 3, 2, null
// cbParallel([1, 2, 3].map(i => (done) => cbTask(i, done)), console.log)
// // 1, oh, 3
// cbParallel([1, -1, 3].map(i => (done) => cbTask(i, done)), console.log)

// Limited parallel execution pattern
function cbParallelLimit(tasks, limit, cb) {
  // Parallel execution of at most N tasks until all complete or first error
  let completed = 0
  let failed = false
  let index = 0
  let running = 0 // Queue can be used instead
  function done (error) {
    --running
    if (error) { failed = true; return cb(error) }
    if (++completed === tasks.length && !failed) { return cb(null) }
    if (running < limit && !failed) { parallel() }
  }
  function parallel() {
    while (index < tasks.length && running < limit) {
      const task = tasks[index++]
      task(done)
      ++running
    }
  }
  parallel()
}

// cbParallelLimit(
//   [1, 2, 3, 4, 5].map(i => (done) => cbTask(i, done)), 2, console.log
// ) // 1, 2 | 3, 4 | 5, null
// cbParallelLimit(
//   [1, 2, 3, -1, 5].map(i => (done) => cbTask(i, done)), 2, console.log
// ) // 1, 2 | 3, oh | 5, null

// Convert callback-based function into a Promise-returning function
function promisifyCb(f) {
  return (...args) => {
    return new Promise((resolve, reject) => {
      const argsCb = [...args, (error, result) => {
        if (error) { return reject(error) }
        resolve(result)
      }]
      f(...argsCb)
    })
  }
}

const taskP = promisifyCb(cbTask)
// taskP(1).then(console.log) // 1, undefined
// taskP(-1).catch(console.error) // oh

// Sequential iteration pattern (dynamic promise chaining)
function promiseIterate(task, arr) {
  let p = Promise.resolve()
  for (const e of arr) { p = p.then(() => task(e)) }
  return p
}

// promiseIterate(taskP, []).then(console.log) // undefined
// promiseIterate(taskP, [1, 2, 3]).then(console.log) // 1, 2, 3, undefined

// Parallel execution pattern
function promiseParallel(tasks) {
  let completed = 0
  return new Promise((resolve, reject) => {
    function done() {
      if (++completed === tasks.length) { resolve() }
    }
    for (const task of tasks) { task().then(done, reject) }
  })
}

// promiseParallel([1, 2, 3].map(i => () => taskP(i)))
//   .then(console.log) // 1, 2, 3, undefined
// promiseParallel([1, -1, 3].map(i => () => taskP(i)))
//   .then(console.log, console.error) // 1, oh, 3

// Limited parallel execution pattern
function promiseParallelLimit(tasks, limit) {
  let completed = 0
  let index = 0
  let running = 0
  return new Promise((resolve, reject) => {
    function done() {
      --running
      if (++completed === tasks.length) { resolve() }
      if (running < limit) { parallel() }
    }
    function parallel() {
      while (index < tasks.length && running < limit) {
        const task = tasks[index++]
        task().then(done, reject)
        ++running
      }
    }
    parallel()
  })
}

// promiseParallelLimit(
//   [1, 2, 3, 4, 5].map(i => () => taskP(i)), 2
// ).then(console.log) // 1, 2 | 3, 4 | undefined
// promiseParallelLimit(
//   [1, 2, 3, -1, 5].map(i => () => taskP(i)), 2
// ).then(console.log, console.error) // 1, 2 | 3, oh | 5

// return await for local errors
async function localError() {
  try { return await taskP(-1) }
  catch (error) { console.error(`Local: ${error}`) }
}

// localError().catch(error => console.error(`Caller: ${error}`)) // Local: oh

// Sequential iteration pattern
async function asyncIterate(task, arr) {
  for (const e of arr) { await task(e) }
}

// await asyncIterate(taskP, [1, 2, 3]) // 1, 2, 3

// Parallel execution pattern
async function asyncParallel(tasks) {
  const promises = tasks.map(task => task())
  // Problem: unnecesary wait for all promosises in the array
  // preceding the rejected promise. Solution: use Promise.all()
  for (const promise of promises) { await promise }
}

// await asyncParallel([1, 2, 3].map(i => () => taskP(i))) // 1, 2, 3
// try {
//   await asyncParallel([1, -1, 3].map(i => () => taskP(i))) // 1, oh, 3
// } catch (error) { console.error(error) }

// Limited parallel execution pattern
async function asyncParallelLimit(tasks, limit) {
  let completed = 0
  let index = 0
  let running = 0
  let promises = []
  function parallel() {
    while (index < tasks.length && running < limit) {
      const task = tasks[index++]
      promises.push(task())
      ++running
    }
  }
  parallel()
  while (completed !== tasks.length) {
    if (promises.length !== 0) {
      await promises.shift()
      ++completed, --running
      parallel()
    }
  }
}

// asyncParallelLimit(
//   [1, 2, 3, 4, 5].map(i => () => taskP(i)), 2
// ) // 1, 2 | 3, 4 | 5
// try {
//   await asyncParallelLimit(
//     [1, 2, 3, -1, 5].map(i => () => taskP(i)), 2
//   ) // 1, 2 | 3, oh | 5
// } catch (error) { console.error(error) }

// Infinite recursive promise chain creates memory leaks
async function leakingRecursion(i = 0) {
  await taskP(i)
  // Memory leak: hain of dependent Promises that never resolve
  return leakingRecursion(i + 1)
  // No memory leak (GC collected Promises), but lost rejections
  leakingRecursion(i + 1)
}

// leakingRecursion() // 0, 1, 2, ...

async function nonLeakingLoop() {
  let i = 0
  while (true) {
    await taskP(i++) // No memory leak + correct error handling
  }
}

// nonLeakingLoop() // 0, 1, 2, ...

// Non-flowing (paused) mode of Readable = readable + end (default)
function nonFlowingReadable() {
  process.stdin
    .on("readable", () => { // new data is available
      let chunk // read() pulls data in a loop from an internal buffer
      // Flexible control over data consumption
      while ((chunk = process.stdin.read()) !== null) { // read() is sync
        console.log(chunk.toString())
      }
    })
    .on("end", () => { console.log("done") }) // end of stream
}

// nonFlowingReadable()

// Flowing mode of Readable = data + end
function flowingReadable() {
  process.stdin
    // on(data) or resume() switches to the flowing mode that pushes data
    // .pause() switches back to the non-flowing mode (default)
    .on("data", chunk => console.log(chunk.toString()))
    .on("end", () => { console.log("done") })
}

// flowingReadable()

// Readable = async iterator
async function asyncIterReadable() {
  for await (const chunk of process.stdin) {
    console.log(chunk.toString())
  }
}

// await asyncIterReadable()

// Implementing Readable
class RandomReadable extends Readable {
  #reads = 0

  async _read(size) {
    const chunk = await randomBytes(10)
    // push() => false for backpressure when the internal buffer is full
    // on(drain) resume pushing
    this.push(chunk) // push data to the internal buffer
    if (++this.#reads === 3) { this.push(null) } // end of stream
  }
}

async function randomReadable() {
  const rr = new RandomReadable()
  for await (const chunk of rr) {
    console.log(chunk.toString("hex"))
  }
}

// randomReadable()

// Simplified Readable sysntax
function randomReadable2() {
  let reads = 0
  return new Readable({
    async read(size) {
      const chunk = await randomBytes(10)
      this.push(chunk)
      if (++reads === 3) { this.push(null) }
    }
  })
}

// for await (const chunk of randomReadable2()) {
//   console.log(chunk.toString("hex"))
// }

function serverWritable() {
  const server = createServer(async (req, res) => {
    res.writeHead(200, { "Content-Type": "text/plain" })
    const body = await randomBytes(10)
    // write() => false for backpressure when the internal buffer is full
    // on(drain) resume writing
    res.write(body)
    res.end("\n\n")
    res.on("finish", () => console.log("done"))
  })
  const port = 9876
  server.listen(port, () => console.log("Listening on port ${port}"))
}

// serverWritable()

class ReplaceTransform extends Transform {
  constructor(re, rep, options) {
    super(options)
    this.re = re, this.rep = rep
    this.tail = "" // match may be spread across chunks
  }

  _transform(chunk, encoding, cb) { // applied to all chunks
    const tailChunk = this.tail + chunk
    const sp = tailChunk.split(this.re)
    if (sp.length === 1) { // no match
      this.push(tailChunk)
      this.tail = ""
    } else {
      // push transformed data into an internal read buffer
      this.push(sp.slice(0, -1).join(this.rep) + this.rep)
      this.tail = sp[sp.length - 1]
    }
    cb()
  }

  _flush(cb) {
    this.push(this.tail) // flush tail just before stream end
    cb()
  }
}

function replaceTransform() {
  const rt = new ReplaceTransform(/ab/, "AB")
  rt.write("ab c a")
  rt.write("b c ab c")
  rt.end()
  rt.on("data", chunk => console.log(chunk.toString()))
}

// replaceTransform() // AB c AB c AB c

// Stream map => filter => aggregate
const salesCsv = `Household,Namibia,597290.92
Baby Food,Iceland,808579.10
Meat,Russia,277305.60
Meat,Italy,413270.00
Cereal,Malta,174965.25
Meat,Indonesia,145402.40
Household,Italy,728880.54`

function salesReadable(salesCsv) {
  const lines = salesCsv.split("\n")
  let index = 0
  return new Readable({
    objectMode: true,
    read() {
      if (index < lines.length) {
        const [type, country, profit] = lines[index++].split(",")
        return this.push({ type, country, profit: Number(profit) })
      }
      this.push(null)
    }
  })
}

function filterBy(property, value) {
  return new Transform({
    objectMode: true,
    transform(record, encoding, cb) {
      if (record[property] === value) {
        this.push(record) // conditional push()
      }
      cb()
    }
  })
}

function totalBy(property) {
  let total = 0
  return new Transform({
    objectMode: true,
    transform(record, encoding, cb) {
      total += record[property] // aggregate without push()
      cb()
    },
    flush(cb) {
      this.push(total.toFixed(2) + "\n") // push() final aggregate
      cb()
    }
  })
}

// PassThrough for observability
function countRecords(title) {
  let count = 0
  const pt = new PassThrough({ objectMode: true })
  pt.on("data", chunk => ++count )
  pt.on("finish", () => console.log(`${title}${count}`))
  return pt
}

// salesReadable(salesCsv)
//   .pipe(countRecords("Total records: "))
//   .pipe(filterBy("country", "Italy"))
//   .pipe(countRecords("Filtered records: "))
//   .pipe(totalBy("profit"))
//   .pipe(process.stdout)

// PassThrough for late binding
function latePiping() {
  const pt = new PassThrough()
  pt.pipe(process.stdout)

  salesReadable(salesCsv)
    .pipe(filterBy("country", "Italy"))
    .pipe(totalBy("profit"))
    .pipe(pt)
}

// latePiping()

// Sequential iteration
function streamIterate(task, arr) {
  const taskTr = new Transform({
    objectMode: true,
    async transform(chunk, encoding, cb) {
      try { await task(chunk); cb() }
      catch (error) { cb(error) }
    }
  })
  return new Promise((resolve, reject) => {
    // Sequential interation
    Readable.from(arr).pipe(taskTr)
      .on("error", reject)
      .on("finish", resolve)
  })
}

// try {
//   await streamIterate(taskP, [1, 2, 3]) // 1, 2, 3
//   await streamIterate(taskP, [1, -1, 3]) // 1, oh
// } catch (error) { console.error(error) }

function streamIterateHead(files) {
  function head(limit) {
    return new Transform({
      objectMode: true,
      transform(file, encoding, cb) {
        const source = createReadStream(file)
        let index = 0
        source.on("data", chunk => {
          const lines = chunk.toString().split("\n")
          while (index < lines.length && index < limit) {
            process.stdout.write(`${file}: ${lines[index++]}\n`)
          }
          if (index === limit) { source.destroy(new Error("limit")) }
        })
        source.on("error", error => {
          if (error.message === "limit") { return cb() } // the limit reached
          cb(error)
        })
        source.on("end", cb) // file smaller than the limit
      }
    })
  }
  return new Promise((resolve, reject) => {
    Readable.from(files)
      .pipe(head(4)) // sequential iteration
      .on("error", reject)
      .on("finish", resolve)
  })
}

// try {
//   await streamIterateHead(["ndp.js", "ydnjs.js"])
// } catch(error) { console.error(error) }

// Parallel execution pattern
function streamParallel(tasks) {
  let completed = 0
  let done = null
  let fail = null
  const taskTr = new Transform({
    objectMode: true,
    transform(task, encoding, cb) {
      // Start all tasks in parallel
      task().catch(fail) // Global reject on first failure
        .finally(() => {
          // flush() stram only when all tasks are actually done
          if (++completed === tasks.length) { done() }
        })
      cb() // all tasks are done immediately
    },
    flush(cb) { done = cb }
  })
  return new Promise((resolve, reject) => {
    fail = reject
    Readable.from(tasks).pipe(taskTr)
      .on("error", reject)
      .on("finish", resolve)
  })
}

// try {
//   await streamParallel([1, 2, 3].map(i => () => taskP(i))) // 1, 2, 3
//   await streamParallel([1, -1, 3].map(i => () => taskP(i))) // 1, oh, 3
// } catch(error) { console.error(error) }

function streamParallelLimit(tasks, limit) {
  let completed = 0
  let running = 0
  let done = null
  let fail = null
  let resume = null
  const taskTr = new Transform({
    objectMode: true,
    transform(task, encoding, cb) {
      task().catch(fail)
        .finally(() => {
          if (++completed === tasks.length) { return done() }
          --running
          // Resume the stream when a task is completed
          if (resume) { const r = resume; resume = null; r() }
        })
      if (++running < limit) { cb() }
      else { resume = cb } // Suspend the stream until a task is completed
    },
    flush(cb) { done = cb }
  })
  return new Promise((resolve, reject) => {
    fail = reject
    Readable.from(tasks).pipe(taskTr)
      .on("error", reject)
      .on("finish", resolve)
  })
}

// try {
//   // 1, 2 | 3, 4 | 5
//   await streamParallelLimit([1, 2, 3, 4, 5].map(i => () => taskP(i)), 2)
//   // 1, 2 | 3, oh | 5
//   await streamParallelLimit([1, 2, 3, -1, 5].map(i => () => taskP(i)), 2)
// } catch(error) { console.error(error) }

function italicStream() {
  return new Transform({
    objectMode: true,
    transform(chunk, encoding, cb) {
      this.push(`_${chunk}_`)
    }
  })
}

function boldStream() {
  return new Transform({
    objectMode: true,
    transform(chunk, encoding, cb) {
      this.push(`**${chunk}**`)
    }
  })
}

// Composition of streams
function composeStreams() {
}

// composeStreams()
