import scala.math._
import scala.io._

// *** CHAPTER 2 - Control structures and functions

// var a = 1; var b = 2
// if (a > 0) { a *= 10; b *= 10 }
// else { a = 0; b = 0 }
// println(a, b)

// val x = 1
// val x0 = 2
// val y = 3
// val y0 = 4
// val d = { val dx = x - x0; val dy = y - y0; sqrt(dx * dx + dy * dy) }
// println(d)

// print("Vlad", "Lana")
// println("Vlad", "Lana")
// printf("%s %s\n", "Vlad", "Lana")
// val name = "Vlad"
// val age = 36
// print(s"$name next year ${age + 1}\n")
// print(f"$name next year ${age + 1}%10.2f\n")

// val name = StdIn.readLine("name > ")
// print("age > ")
// val age = StdIn.readInt()
// println(f"$name is $age\n")

// var i = 0
// while (i < 5) { print(i); i += 1 }
// for (i <- 0 to 4) { print(i) }
// val s = "Vlad"
// for (i <- 0 to s.length - 1) { print(s(i)) }
// for (c <- s) { print(c) }
// for (i <- 1 to 3; j <- 4 to 6) { print(i, j) }
// for (i <- 1 to 3; j <- 1 to 3 if i != j) { print(i, j) }
// for (i <- 1 to 3; k = 4 - i; j <- k to 3) { print(i, j) }
// println(for (i <- 1 to 10) yield i)

// def aabs(x: Double) = { if (x >= 0) x else -x }
// println(aabs(4), aabs(-5))

// def afact(n: Int) = {
//   var r = 1
//   for (i <- 2 to n) { r = r * i }
//   r
// }
// println(Vector(0, 1, 2, 3, 4, 5).map(afact))

// def afact(n: Int): Int = { if (n < 2) 1 else n * afact(n - 1) }
// println(Vector(0, 1, 2, 3, 4, 5).map(afact))

// def decorate(s: String, l: String = "[", r: String = "]") = { s"$l$s$r" }
// println(decorate("Vlad"))
// println(decorate("Vlad", "{", "}"))
// println(decorate(r = ">", l = "<", s = "Vlad"))

// def asum(x: Int*) = { var r = 0; for (i <- x) { r += i }; r }
// println(asum(1, 2, 3, 4, 5))
// println(asum(1 to 5: _*))

// def rsum(x: Int*): Int = { if (x.length == 0) 0 else x.head + rsum(x.tail: _*) }
// println(rsum(1, 2, 3, 4, 5))

// def box(s: String) = {
//   val l = "-" * (s.length + 4)
//   s"$l\n| $s |\n$l"
// }
// println(box("Vlad"))

// lazy val c = Source.fromFile("./bin/run.sh").mkString
// print(c)

// try {
//   try {
//     throw new IllegalArgumentException("oh")
//     println("ok")
//   } finally {
//     println("Cleaning up")
//   }
// } catch {
//   case e: IllegalArgumentException => println("ERROR: illegal argument")
//   case e: Exception => println("ERROR: unknown error")
// }

// def asignum(n: Double) = { if (n > 0) 1 else if (n < 0) -1 else 0 }
// println(Vector(4.1, 0.0, -3.2).map(asignum))

// for (i <- 10 to 0 by -1) { print(s"$i ") }

// def stringToNumber(s: String) = {
//   var n: BigInt = 1
//   for (c <- s) { n *= c.toInt }
//   n
// }
// println(stringToNumber("Hello"))

// def aproduct(x: Int, n: Int): Int = { if (n < 1) 0 else x + aproduct(x, n - 1) }
// println(Vector(0, 1, 2, 3).map((n) => aproduct(4, n)))

// def anexponent(x: Int, n: Int): Int = { if (n < 1) 1 else x * anexponent(x, n - 1) }
// println(Vector(0, 1, 2, 3).map((n) => anexponent(4, n)))

// *** CHAPTER 3 - Arrays

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

// val a = new Array[Int](5)
// println(a.mkString(" "))
// a(0) = 10
// println(a.mkString(" "))

// val a = new ArrayBuffer[Int]()
// a += 1
// println(a.mkString(" "))
// a ++= Array(2, 3, 4)
// println(a.mkString(" "))

// val a = Array(1, 2, 3, 4)
// for (i <- 0 until a.length) { print(s"${a(i)} ") }
// for (i <- 0 to 7 by 2) { print(s"$i ") }
// for (i <- 7 to 0 by -1) { print(s"$i ") }
// println((for (e <- a) yield e * 10).mkString(" "))
// println((for (e <- a if e % 2 == 0) yield e * 10).mkString(" "))
// println(a.filter(_ % 2 == 0).map(_ * 10).mkString(" "))
// println((a filter { _ % 2 == 0 } map { _ * 10 }).mkString(" "))

// val a = Array(0, 1, -2, 3, -4, 5)
// println((for (e <- a if e >= 0) yield e).mkString(" "))
// println(a.sum)
// println(a.min)
// println(a.sorted.mkString(" "))
// println(a.sortWith(_ > _).mkString(" "))
// println(a.count(_ => true))

// val t = new Array[Array[Int]](5)
// for (i <- t.indices) { t(i) = new Array[Int](i + 1) }
// for (l <- t) { println(l.mkString(" ")) }

// val a = new Array[Int](5)
// val r = new Random()
// for (i <- a.indices) { a(i) = r.nextInt(5) }
// for (i <- a.indices) { a(i) = Random.between(0, 5) }
// println(a.mkString(" "))

// val a = Array(1, 2, 3, 4, 5)
// for (i <- a.indices) { if (i > 0 && i % 2 == 1) {
//   val t = a(i); a(i) = a(i - 1); a(i - 1) = t
// }}
// println(a.mkString(" "))

// val a = Array(0, -1, 2, -3, 4, -5)
// val p = for (e <- a if (e > 0)) yield e
// val n = for (e <- a if (e <= 0)) yield e
// val r = p ++ n
// println(r.mkString(" "))

// def anaverage(a: Array[Double]) = { a.sum / a.length }
// println(anaverage(Array(1, 2, 3, 4, 5)))

// *** CHAPTER 4 - Maps and tuples

// Pair: a -> b => (a, b)
// val m = Map("Alice" -> 10, "Bob" -> 3, "Cindy" -> 8)
// println(m("Bob"))
// println(m.get("Bob"))
// println(m.getOrElse("Bobx", -1))
// val md = m.withDefaultValue(0)
// println(md("Bobx"))
// val mm = scala.collection.mutable.Map(("Alice", 10), ("Bob", 3), ("Cindy", 8))
// mm("Fred") = 7
// println(mm)
// mm ++= List("Vlad" -> 9, "Lana" -> 10)
// println(mm)
// mm -= "Fred"
// println(mm)
// val m2 = m + ("Vlad" -> 9)
// println(m2)
// for ((k, v) <- m2) { print(s"$k -> $v, ") }
// println(m2.keySet)
// println(m2.values)
// println(for ((k, v) <- m2) yield (v, k))
// val sm = new scala.collection.mutable.TreeMap[String, Int]()
// sm ++= List("Vlad" -> 9, "Lana" -> 10, "Alice" -> 10, "Bob" -> 3, "Cindy" -> 8)
// println(sm)

// val t = (1, "Vlad", true)
// println(t._2)
// val (_, b, _) = (1, "Vlad", true)
// println(b)

// val k = Array(1, 2, 3)
// val v = Array('a', 'b', 'c')
// println(k.zip(v).mkString(" "))

// val m = Map("a" -> 1, "b" -> 3, "c" -> 7)
// println(for ((k, v) <- m) yield (k, v * (1 - 0.1)))

// val t = "a b c b a b d b a c"
// val h = new scala.collection.mutable.HashMap[String, Int]()
// for (w <- t.split(" ")) { h(w) = h.getOrElse(w, 0) + 1 }
// println(h)

// *** CHAPTER 5 - Classes

// class Counter {
//   private var _counter = 0
//   def inc(delta: Int = 1) = _counter += delta
//   def value() = _counter
// }
// val c = new Counter();
// c.inc(); c.inc(2);
// println(c.value())

// class Person {
//   private var _age = 0
//   def age = _age
//   def age_= (age: Int) = if (age > _age) { _age = age }
// }
// val p = new Person()
// p.age = 10
// p.age = 5
// println(p.age)

// class Person {
//   private var name = "nobody"
//   private var age = 0
//   override def toString() = s"$name $age"
//   def this(n: String) = { this(); name = n }
//   def this(n: String, a: Int) = { this(n); age = a }
// }
// val p1 = new Person()
// println(p1.toString())
// val p2 = new Person("Vlad")
// println(p2.toString())
// val p3 = new Person("Vlad", 36)
// println(p3.toString())

// class Person(var name: String = "nobody", var age: Int = 0) {
//   override def toString() = s"$name $age"
// }
// val p1 = new Person()
// println(p1.toString())
// val p2 = new Person("Vlad")
// println(p2.toString())
// val p3 = new Person("Vlad", 36)
// println(p3.toString())

// class Account() {
//   private var _balance = 0.0
//   def balance = _balance
//   def deposit(amount: Double) = _balance += amount
//   def withdraw(amount: Double) = if (_balance > amount) _balance -= amount
// }
// val a = new Account()
// a.deposit(50)
// a.withdraw(10)
// println(a.balance)

// class Person(fullName: String) {
//   val firstName = (fullName.split(" "))(0)
//   val lastName = (fullName.split(" "))(1)
//   override def toString() = s"$firstName $lastName"
// }
// val p = new Person("Vlad Veles")
// println(p)

// *** CHAPTER 6 - Objects

// object Account {
//   private var _id = 0
//   def uniqueId() = { _id += 1; _id }
//   def apply(b: Double) = { val a = new Account(); a.balance = b; a }
// }
// println(Account.uniqueId(), Account.uniqueId())
// class Account {
//   val id = Account.uniqueId()
//   var balance = 0.0
// }
// val a = new Account();
// println(a.id)
// val a2 = Account(10.0)
// println(a2.balance)

// object TrafficLight extends Enumeration {
//   val Red, Yellow, Green = Value
// }
// import TrafficLight._
// def actOnTrafficLight(l: TrafficLight.Value) = {
//   if (l == Red) "Stop"
//   else if (l == Yellow) "Prepare"
//   else "Go"
// }
// println(actOnTrafficLight(TrafficLight.Yellow))
// for (l <- TrafficLight.values) { println(actOnTrafficLight(l)) }

// *** CHAPTER 7 - Packages and imports

// val e = new hr.Manager("Vlad")
// e.name = "Volodymyr"
// println(e.name)
// import hr.{Employee => AnEmployee}
// val e2 = new AnEmployee()
// println(e2.name)

// *** CHAPTER 8 - Inheritance

// class Person (val name: String = "nobody") {
//   override def toString = s"${getClass.getName} name=$name"
//   override def equals(b: Any) = b match {
//     case bb: Person => bb.name.toLowerCase() == name.toLowerCase()
//     case _ => false
//   }
// }
// class Employee (name: String = "nobody", val salary: Double = 0.0)
//     extends Person(name) {
//   override def toString = s"${super.toString} salary=$salary"
// }
// val e = new Employee("Vlad", 1.0)
// println(e)
// println(e.isInstanceOf[Person])
// println(e.getClass == classOf[Employee])
// e match {
//   case e: Employee => println("Employee", e)
//   case _ => println("Any")
// }
// val p1 = new Person("a")
// val p2 = new Person("A")
// println(p1 == p2)

// class MilTime private(val time: Int) {
//   def hours = time / 100
//   def minutes = time % 100
//   override def toString = f"$time%04d"
// }
// object MilTime {
//   def apply(t: Int) =
//     if (t >= 0 && t <= 2400 && t % 100 < 60) new MilTime(t)
//     else throw new IllegalArgumentException
// }
// val t = MilTime(123)
// println(t, t.hours, t.minutes)

// *** CHAPTER 9 - Files and regular expressions

import scala.io.Source

// val f = Source.fromFile("./bin/run.sh")
// try { for ((l, i) <- f.getLines().zipWithIndex) { println(f"$i% 3d $l") } }
// finally { f.close() }
// try { println(f.mkString) } finally { f.close() }
// try { for (c <- f) { print(c) } } finally { f.close }
// try { for (w <- f.mkString.split("\\s+")) { print(s"${w.toUpperCase()} ") } }
// finally { f.close() }
// val s = Source.fromString("1 2 3 4\n5 6")
// println(s.mkString.split("\\s+").map(x => x.toInt + 1).mkString)

import scala.sys.process._

// "ls -lah ..".!
// val o = "ls -lah ..".!!
// println(o)
// ("ls -lah" #| "grep scala").!
// #> #>> #< #&& #||

// val s = "one 1 two 2 ten 10"
// val p = """\d+""".r
// println(p.findFirstIn(s))
// for (m <- p.findAllIn(s)) { println(m) }
// println(p.replaceFirstIn(s, "*"))
// println(p.replaceAllIn(s, "*"))
// println(p.replaceSomeIn(s, m => if (m.matched.toInt % 2 == 0) Some("*") else None))
// val p2 = """(\w+) (\d+)""".r("wd", "nm")
// println(p2.findFirstMatchIn(s))
// for (m <- p2.findAllMatchIn(s)) { println(m.group(0), m.group(1), m.group(2)) }
// println(p2.replaceAllIn(s, m => s"${m.group(2)} ${m.group(1)}"))
// println(p2.replaceAllIn(s, m => s"""${m.group("nm")} ${m.group("wd")}"""))
// val p3 = """(\w+) (\d+)""".r
// val p3(wd, nm) = "eleven 11"
// println(wd, nm)
// for (p2(wd, nm) <- p2.findAllIn(s)) { println(wd, nm) }
