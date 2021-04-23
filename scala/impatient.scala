import scala.math._
import scala.io._

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

// import scala.io._
// val name = StdIn.readLine("name > ")
// print("age > ")
// val age = StdIn.readInt()
// println(f"$name is $age\n")

// var i = 0
// while (i < 5) {
//   print(i)
//   i += 1
// }

// for (i <- 0 to 4) {
//   print(i)
// }

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
// println(Vector(0, 1, 2, 3).map((n: Int) => aproduct(4, n)))

// def anexponent(x: Int, n: Int): Int = { if (n < 1) 1 else x * anexponent(x, n - 1) }
// println(Vector(0, 1, 2, 3).map((n: Int) => anexponent(4, n)))
