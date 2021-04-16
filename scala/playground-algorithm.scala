import scala.math._

var a = 1; var b = 2
if (a > 0) { a *= 10; b *= 10 }
else { a = 0; b = 0 }

// println(a, b)

val x = 1
val x0 = 2
val y = 3
val y0 = 4
val d = { val dx = x - x0; val dy = y - y0; sqrt(dx * dx + dy * dy) }

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

val s = "Vlad"
for (i <- 0 to s.length - 1) { print(s(i)) }
for (c <- s) { print(c) }
