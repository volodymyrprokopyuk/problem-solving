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

function syncAdd(a, b, cb) { cb(a + b) }
function asyncAdd(a, b, cb) { setImmediate(() => cb(a + b)) }

// console.log("Before")
// syncAdd(1, 2, console.log) // blocking
// asyncAdd(3, 4, console.log) // non-blocking
// console.log("After") // Before, 3, After, 7
