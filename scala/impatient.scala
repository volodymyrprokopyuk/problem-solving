import scala.math._
import scala.io._

// *** CHAPTER 2

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

// *** CHAPTER 3

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

// *** CHAPTER 4

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

// *** CHAPTER 5

// class Counter {
//   private var counter = 0
//   def inc(delta: Int = 1) = counter += delta
//   def value() = counter
// }
// val c = new Counter();
// c.inc(); c.inc(2);
// println(c.value())

class Person {
  private var _age = 0
  def age = _age
  def age_= (age: Int) = if (age > _age) { _age = age }
}
val p = new Person()
p.age = 10
p.age = 5
println(p.age)
