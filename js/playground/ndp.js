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

function asyncTask(x, cb) {
  setTimeout(() => {
    if (x === -1) { return cb("oh") }
    console.log(x); cb(null)
  }, 500)
}

// Sequential iterator pattern
function asyncIterate(task, arr, cb) {
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

// asyncIterate(asyncTask, [], console.log) // null
// asyncIterate(asyncTask, [1, 2, 3], console.log) // 1, 2, 3, null

// Parallel execution pattern
function parallel(tasks, cb) {
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
// parallel([1, 2, 3].map(i => (done) => asyncTask(i, done)), console.log)
// // 1, oh, 3
// parallel([1, -1, 3].map(i => (done) => asyncTask(i, done)), console.log)

// Limited parallel execution pattern
function parallelLimit(tasks, limit, cb) {
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

// parallelLimit(
//   [1, 2, 3, 4, 5].map(i => (done) => asyncTask(i, done)), 2, console.log
// ) // 1, 2 | 3, 4 | 5, null
// parallelLimit(
//   [1, 2, 3, -1, 5].map(i => (done) => asyncTask(i, done)), 2, console.log
// ) // 1, 2 | 3, oh | 5, null
