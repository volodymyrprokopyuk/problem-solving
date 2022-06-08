function add(x) {
  return function(y) { return x + y }
}

const add1 = add(1)
// console.log(add1(2))

const sum = x => y => x + y
const sum1 = sum(1)
// console.log(sum1(2))

function f() { console.log(this.p) }

// const p = 0
// const o1 = { p: 1, f: f }
// const o2 = { p: 2 }

// f() // undefined
// o1.f() // 1
// f.call(o2) // 2
// new f() // undefined

// const o3 = { a: 1 }
// const o4 = Object.create(o3) // prototype
// console.log(o4.a)

// for (var i = 0; i < 3; ++i) {
//   // setTimeout(function() { console.log(i) }, i * 500) 3, 3, 3
//   setTimeout((function(j) {
//     return function() { console.log(j) }
//   })(i), i * 500)
// }

// for (let i = 0; i < 3; ++i) {
//   setTimeout(function() { console.log(i) }, i * 500)
// }

function module(a) {
  let b = a // private state
  function f() { return ++b } // closure over state
  return { f } // public interface
}
// const m = module(10) // module instance
// console.log(m.f())

const o5 = {
  count: 0,
  f: function() { return function() { console.log(++this.count) }.bind(this) }
}
// o5.f()()
// setTimeout(o5.f(), 100)

const o6 = {
  count: 0,
  f: function() { return () => console.log(++this.count) }
}
// o6.f()()
// setTimeout(o6.f(), 100)

// let c = { count: 0 }
// function f() { ++this.count }
// f.call(c)
// f.call(c)
// const ff = f.bind(c)
// ff()
// ff()
// console.log(c.count)

// function f(a) { this.a = a }
// const o = new f(1)
// console.log(o.a)

// function f(a, b) { this.c = a + b }
// const o = { }
// const ff = f.bind(o, 1) // partial application
// ff(2)
// console.log(o)

// const s = "a" // string type
// console.log(typeof s, s instanceof String) // string, false
// const s2 = new String("a") // String object
// console.log(typeof s2, s2 instanceof String) // object, true

// const o = { a: 1, b: function() { console.log("b") } }
// console.log(JSON.parse(JSON.stringify(o)))

// const o = { a: 1 }
// console.log("a" in o) // true
// for (const p in o) { console.log(p, o[p]) } // a 1
// delete o.a
// console.log(o) // { }

// const o = { }
// Object.defineProperty( // property descriptor
//   o, "a", { value: 1, writable: true, enumerable: true, configurable: true }
// )
// o.a = 2
// console.log(o) // 2

// const o = { }
// Object.defineProperty(o, "a", { // accessor descriptor
//   set: function(val) { this._a = val },
//   get: function() { return this._a * 2 }
// })
// o.a = 1
// console.log(o.a) // 2

// const o = { // object literal setter and getter
//   set a(val) { this._a = val },
//   get a() { return this._a * 2 }
// }
// o.a = 1
// console.log(o.a) // 2

// const a = [1, 2]
// for (let i = 0; i < a.length; ++i) { console.log(a[i]) } // 1, 2
// for (const e of a) { console.log(e) } // 1, 2
// const o = { a: 1, b: 2 }
// for (const p in o) { console.log(p, o[p]) } // a, 1, b, 2
// Object.defineProperty(o, Symbol.iterator, {
//   writable: false, enumerable: false, configurable: true,
//   value: function() {
//     const o = this
//     const keys = Object.keys(o)
//     let i = 0 // iterator state
//     function next() {
//       return { value: o[keys[i++]], done: (i > keys.length) }
//     }
//     return { next }
//   }
// })
// for (const e of o) { console.log(e) } // 1, 2

const random = {
  [Symbol.iterator]: function () {
    function next() { return { value: Math.random(), done: false } }
    return { next }
  }
}
// let i = 0;
// for (const r of random) {
//   console.log(r)
//   if (++i > 10) break;
// }

function mixin(source, target) {
  for (key in source) {
    if (!(key in target)) { target[key] = source[key] }
  }
  return target
}

var Vehicle = {
  engines: 1,
  ignition: function() {
    console.log(`Vehicle: turning on ${this.engines} engines`)
  },
  drive: function() {
    this.ignition()
    console.log("Vehicle: moving forward")
  }
}

var Car = mixin(Vehicle, {
  wheels: 4,
  drive: function() {
    // Vehicle.drive.call(this)
    Vehicle.drive()
    console.log(`Car: rolling on ${this.wheels} wheels`)
  }
})

// Car.engines = 2
// Vehicle.drive()
// Car.drive()

// const o = { a: 1 }
// new object o2.[[Prototype]] = o (prototype chain)
// const o2 = Object.create(o)
// console.log("a" in o2) // true
// console.log(o2.a) // 1
// for (const p in o2) { console.log(p, o2[p]) } // a, 1

function F() { this.a = 1 }
F.prototype.b = 2
const o = new F() // constructor call returns an object
console.log(o.a, o.b) // 1, 2
console.log(F.prototype.constructor === F, o instanceof F) // true, true
console.log(Object.getPrototypeOf(o) === F.prototype) // true
function G() {
  F.call(this) // call parent constructor
  this.c = 3
}

// prototypal inheritance
// Option 1. Throws away G.prototype = new object o.[[Prototype]] = F.prototype
G.prototype = Object.create(F.prototype) prototype chain
// true, true, not G
console.log(G.prototype.constructor === F, o instanceof F)

// Option 2. Updates G.prototype (the right ES6 way)
Object.setPrototypeOf(G.prototype, F.prototype) // prototype chain
G.prototype.d = 4
const o2 = new G()
console.log(o2.a, o2.b, o2.c, o2.d) // 1, 2, 3, 4
// true, true, true
console.log(G.prototype.constructor === G, o2 instanceof G, o2 instanceof F)
console.log(F.prototype.isPrototypeOf(o2)) // true
