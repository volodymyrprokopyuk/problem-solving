package object hr {
  val defaultName = "nobody"
}

package hr {
  class Employee (var name: String = hr.defaultName) { }
  class Manager (n: String = hr.defaultName) extends Employee(n) { }
}
