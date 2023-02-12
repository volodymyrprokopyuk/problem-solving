import { EventEmitter } from "events"
import { readFile } from "fs"

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
function promisify(f) {
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

const taskP = promisify(cbTask)
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
