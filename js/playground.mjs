// function add(x) {
//   return function(y) { return x + y }
// }

// const add1 = add(1)
// console.log(add1(2))

// const sum = x => y => x + y
// const sum1 = sum(1)
// console.log(sum1(2))

// function f() { console.log(this.p) }

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

// function module(a) {
//   let b = a // private state
//   function f() { return ++b } // closure over state
//   return { f } // public interface
// }
// const m = module(10) // module instance
// console.log(m.f())

// const o5 = {
//   count: 0,
//   f: function() { return function() { console.log(++this.count) }.bind(this) }
// }
// o5.f()()
// setTimeout(o5.f(), 100)

// const o6 = {
//   count: 0,
//   f: function() { return () => console.log(++this.count) }
// }
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

// const random = {
//   [Symbol.iterator]: function () {
//     function next() { return { value: Math.random(), done: false } }
//     return { next }
//   }
// }
// let i = 0;
// for (const r of random) {
//   console.log(r)
//   if (++i > 10) break;
// }

// function mixin(source, target) {
//   for (key in source) {
//     if (!(key in target)) { target[key] = source[key] }
//   }
//   return target
// }

// var Vehicle = {
//   engines: 1,
//   ignition: function() {
//     console.log(`Vehicle: turning on ${this.engines} engines`)
//   },
//   drive: function() {
//     this.ignition()
//     console.log("Vehicle: moving forward")
//   }
// }

// var Car = mixin(Vehicle, {
//   wheels: 4,
//   drive: function() {
//     // Vehicle.drive.call(this)
//     Vehicle.drive()
//     console.log(`Car: rolling on ${this.wheels} wheels`)
//   }
// })

// Car.engines = 2
// Vehicle.drive()
// Car.drive()

// const o = { a: 1 }
// new object o2.[[Prototype]] = o (prototype chain)
// const o2 = Object.create(o)
// console.log("a" in o2) // true
// console.log(o2.a) // 1
// for (const p in o2) { console.log(p, o2[p]) } // a, 1

// function F() { this.a = 1 } // constructor
// F.prototype.b = function() { return 2 } // method
// const o = new F()
// console.log(o.a, o.b()) // 1, 2
// function G() { F.call(this); this.c = 3 } // call parent constructor
// // Prototypal inheritance Option 1. Overwrite G.prototype
// G.prototype = Object.create(F.prototype)
// // Prototypal inheritance Option 2. Update G.prototype
// Object.setPrototypeOf(G.prototype, F.prototype)
// G.prototype.d =
//   function() { return F.prototype.b.call(this) + 2 } // call parent method
// const o2 = new G()
// console.log(o2.a, o2.b(), o2.c, o2.d()) // 1, 2, 3, 4

// class F {
//   constructor() { this.a = 1 } // constructor + property
//   b() { return 2 } // method
// }
// const o = new F()
// console.log(o.a, o.b()) // 1, 2
// class G extends F { // prototypal ihheritance
//   constructor() { super(); this.c = 3 }// call parent constructor
//   d() { return super.b() + 2 } // call parent method
// }
// const o2 = new G()
// console.log(o2.a, o2.b(), o2.c, o2.d()) // 1, 2, 3, 4

// const sym = Symbol("a")
// const o = { [sym]: 1 }
// console.log(o[sym]) // 1

// const o = { a: 1, toJSON() { return { a: this.a } } }
// o.oo = o // circular reference
// console.log(JSON.stringify(o))

// function timeoutify(fun, timeout) {
//   let id = setTimeout(() => {
//     id = null
//     fun(new Error("timeout"))
//   }, timeout)
//   return (...a) => {
//     if (id) {
//       clearTimeout(id)
//       fun(null, ...a)
//     }
//   }
// }
// function f(e, d) {
//   if (e) { console.error(e) } else { console.log(d) }
// }
// const tf = timeoutify(f, 500)
// setTimeout(() => tf(1), 400) // 1

// function timeoutPromise(timeout) {
//   return new Promise((_, reject) =>
//     setTimeout(() => reject("timeout"), timeout)
//   )
// }
// function f(x, timeout) {
//   return new Promise((resolve) =>
//     setTimeout(() => resolve(x), timeout)
//   )
// }
// Promise.race([f(1, 400), timeoutPromise(500)])
//   .then(console.log).catch(console.error)

// Promise.resolve(1)
//   .then(x => x + 1)
//   .then(x => new Promise(resolve => setTimeout(_ => resolve(x * 2), 100)))
//   .then(console.log) // 4

// Promise.resolve(1)
//   // default rejection handler: e => { throw e } for incoming errors
//   .then(_ => { throw new Error("oh") })
//   // default resolution handler: x => { return x } for incoming values
//   .catch(e => { console.error(e.message); return 2 }) // for outgoing errors
//   .then(console.log) // oh, 2 (back to normal)

// function f(x, cb) {
//   setTimeout(_ => { if (x >= 0) { cb(null, "ok") } else { cb("oh") } }, 100)
// }
// f(1, console.log)
// f(-1, console.error)
// function promisify(f) {
//   return function(...args) {
//     return new Promise((resolve, reject) => {
//       f.apply(null, args.concat(
//         function(e, x) { if (e) { reject(e) } else { resolve(x) } }))
//     })
//   }
// }
// const ff = promisify(f)
// ff(1).then(console.log)
// ff(-1).catch(console.error)

// let m
// if (m = "a 1 b 2".match(/(?<letter>\w) (?<digit>\d)/)) {
//   console.log(m[0], m[1], m[2])
//   console.log(m.groups.letter, m.groups.digit)
// }
// if (m = "a 1 b 2".matchAll(/(\w) (\d)/g)) {
//   for (const mm of m) { console.log(mm[0], mm[1], mm[2]) }
// }

// const f = x => new Promise((resolve) => setTimeout(_ => resolve(x + 1), 100))
// const p = [f, f].reduce((p, f) => p.then(f), Promise.resolve(1))
// p.then(console.log) // 3
// let r = 1
// for (const ff of [f, f]) { r = await ff(r) }
// console.log(r) // 3

// function* f(x) {
//   console.log(x++)
//   const y = yield "a" // yield requires 2 iterations: start + resume
//   console.log(y, x) // implicit return undefined;
// }
// const it = f(1) // creates iterator + initiates generator
// const { value } = it.next() // starts generator (must always be empty)
// console.log(value)
// const r = it.next("b") // message to generator + resumes generator 1, a, b, 2
// console.log(r) // { value: undefined, done: true }

// const closure = (function() {
//   let v = 0
//   return function() { return ++v }
// })()
// console.log(closure(), closure(), closure()) // 1, 2, 3

// const iterator = function(n) {
//   let v = 0
//   return {
//     [Symbol.iterator]() { return this },
//     next() {
//       return v < n ? { value: ++v, done: false } :
//       { value: undefined, done: true } }
//   }
// }
// for (const i of iterator(3)) { console.log(i) } // 1, 2, 3

// const generator = function*(n) {
//   let v = 0
//   while (v < n) { yield ++v }
// }
// for (const i of generator(3)) { console.log(i) } // 1, 2, 3

// const generator = function*() {
//   let v = 0
//   try {
//     while (true) { yield ++v }
//   } finally { console.log("finally") }
// }
// let gen = generator()
// for (const i of gen) {
//   if (i > 2) { const { value } = gen.return("return"); console.log(value) }
//   console.log(i) // 1, 2, finally, return, 3
// }

// function f(x, cb) {
//   setTimeout(_ => x === "oh" ? cb(new Error(x)) : cb(null, x), 100)
// }
// function cb(err, data) { if (err) { it.throw(err) } else { it.next(data) } }
// function* gen() {
//   try {
//     const a = yield f(1, cb)
//     console.log(a)
//     const b = yield f("oh", cb)
//   } catch (e) { console.error(e.message) }
// }
// const it = gen()
// it.next() // 1, oh

// function f(x) {
//   return new Promise((resolve, reject) =>
//     setTimeout(_ => x === "oh" ? reject(new Error(x)) : resolve(x), 100)
//   )
// }
// function* gen() {
//   try {
//     const a = yield f(1)
//     console.log(a)
//     const b = yield f("oh")
//     // const b = yield f(2)
//     // console.log(b)
//   } catch(e) { console.error(e.message) }
// }
// const it = gen()
// it.next().value
//   .then(a => it.next(a).value.then(b => it.next(b)))
//   .catch(e => console.log(e.message)) // 1, oh

// function* a() { yield 1; yield* b(); yield 4 }
// function* b() { yield 2; yield 3 }
// for (const i of a()) { console.log(i) } // 1, 2, 3, 4

// function f(a, b, c) { console.log(a, b, c) }
// f.apply(null, [1, 2, 3])
// f(...[1, 2, 3])
// console.log([1, ...[2, 3], 4])
// console.log([1, [2, 3], 4].flat())

// const o = { a: 1, b: 2, c: 3 }
// const a = [10, 20, 30]
// let o2 = { }
// let a2 = [];
// ({ a: o2.A, b: o2.B, c: o2.C } = o)  // object => object
// console.log(o2); // { A: 1, B: 2, C: 3 }
// [a2[2], a2[1], a2[0]] = a  // array => array
// console.log(a2); // [ 30, 20, 10 ]
// ({ a: a2[0], b: a2[1], c: a2[2] } = o) // object => array
// console.log(a2); // [ 1, 2, 3 ]
// [o2.A, o2.B, o2.C] = a // array => object
// console.log(o2) // { A: 10, B: 20, C: 30 }

// const [x, ...y] = a
// console.log(x, y, [x, ...y]) // 10, [ 20, 30 ], [ 10, 20, 30 ]
// const { a, ...x } = o
// console.log(a, x, { a, ...x }) // 1 { b: 2, c: 3 } { a: 1, b: 2, c: 3 }

// const [p, q, r, s = 0] = a
// console.log(p, q, r, s) // 10, 20, 30, 0
// const { a: p, d: s = 0 } = o
// console.log(p, s) // 1, 0

// const o = {
//   _a: 1,
//   get a() { return this._a },
//   set a(v) { this._a = v }
// }
// o.a++
// console.log(o.a) // 2

// const p = "a"
// const o = { [p]: 1 }
// console.log(o.a, o[p]) // 1, 1

// function tag(strings, ...values) {
//   return `${strings[1].trim()} ${values[0] + 1} ${strings[0]}`
// }
// const a = 1
// console.log(tag`A ${a + 1} B`) // B 3 A

// function Singleton() {
//   // const instance = Symbol("instance")
//   const instance = Symbol.for("singleton.instance") // Symbol registry
//   if (!Singleton[instance]) {
//     this.a = 1
//     Singleton[instance] = this
//   }
//   return Singleton[instance]
// }
// const s1 = new Singleton()
// const s2 = new Singleton()
// console.log(s1, s2, s1 === s2, new Number(1) === new Number(1))
// // Singleton { a: 1 }, Singleton { a: 1 }, true, false

// function iterator(n) { // iterator configuration
//   let i = 0; // iterator state
//   const next = _ => ({ value: i < n ? i : undefined, done: ++i > n })
//   // iterable + iterator
//   return { [Symbol.iterator]() { return this }, next }
// }
// for (const i of iterator(5)) { console.log(i) }

// function* gen() {
//   const a = yield 1
//   const b = yield 2
//   console.log(a, b)
// }
// const it = gen() // create controlling iterator
// const { value: a } = it.next() // start generator
// const { value: b } = it.next(10)
// it.next(20) // 10, 20 (finish generator)
// console.log(a, b) // 1, 2

// function* gen() {
//   try {
//     yield 1
//   } catch (e) { console.error(e.message) } // uh
//   throw new Error("oh")
// }
// const it = gen()
// try {
//   const { value: a } = it.next()
//   console.log(a) // 1
//   it.throw(new Error("uh"))
// } catch (e) { console.error(e.message) } // oh

// class A {
//   constructor(a) { this._a = a }
//   get a() { return this._a }
//   set a(v) { this._a = v }
// }
// class B extends A { // prototype delegation
//   constructor(a, b) {
//     super(a) // parent constructor
//     this.b = b
//   }
//   static c = 10 // statics are on the constructor function, not the prototype
//   sum() { return super.a + this.b } // parent object
// }
// const b = new B(1, 2)
// b.a += 3
// console.log(b.a, b.sum(), B.c) // 4, 6, 10

// const m = new Map([["a", 1], ["b", 2]])
// m.set("c", 3)
// console.log(m.size, m.get("b"))
// m.delete("c")
// console.log(m, m.entries(), m.keys(), m.values())
// console.log(m.has("a"))
// m.clear()

// const s = new Set([1, 2])
// s.add(3)
// console.log(s.size)
// s.delete(3)
// console.log(s, s.entries(), s.keys(), s.values())
// console.log(s.has(2))
// s.clear()

// for (const [k, v] of ["a", "b", "c"].entries()) { console.log(k, v) }

// function f() { }
// const g = function() { }
// console.log(f.name, g.name) // f, g

// function F() { console.log(new.target) }
// const f = new F() // function F
// class C {
//   constructor() { console.log(new.target) }
// }
// const c = new C() // class C

// const o = { a: 1 }
// const handlers = {
//   get(target, key, context) {
//     if (Reflect.has(target, key)) {
//       console.log("get key", key)
//       // forward operation from context (proxy) to target (object)
//       return Reflect.get(target, key, context)
//     } else {
//       throw new Error(`${key} does not exist`)
//     }
//   }
// }
// const p = new Proxy(o, handlers)
// console.log(p.a) // get key a, 1

// const o = { a: 1 }
// const handlers = {
//   get(target, key, context) { throw new Error(`${key} does not exits`) }
// }
// const p = new Proxy(o, handlers)
// Object.setPrototypeOf(o, p)
// console.log(o.a, o.b) // 1, Error

// function rmap(a, f = e => e, r = []) {
//   if (a.length > 1) {
//     const [h, ...t] = a
//     return rmap(t, f, r.concat(f(h)))
//   } else {
//     return r.concat(f(a[0]))
//   }
// }
// const a = new Array(9999)
// console.log(rmap(a.fill(0), e => e + 1)) // Maximum call stack size exceeded

// function trampoline(f) { // factors out recursion into loop
//   // stack depth remains constant (stack frames are reused)
//   while (typeof f === "function") { f = f() }
//   return f
// }
// function tmap(a, f = e => e, r = []) {
//   if (a.length > 1) {
//     // no recursive call to tmap(), just return the partial() function
//     return function partial() { // executed by trampoline
//       const [h, ...t] = a
//       return tmap(t, f, r.concat(f(h)))
//     }
//   } else {
//     return r.concat(f(a[0]))
//   }
// }
// const a = new Array(9999)
// console.log(trampoline(tmap(a.fill(0), e => e + 1))) // no RangeError
