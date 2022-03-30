import std/streams
import pkg/yaml

var s: FileStream

type
  Person = object
    name: string
    height: int

# Dump object
let person = Person(name: "Vlad", height: 172)
echo person.dump
s = newFileStream("pkg/out.yaml", fmWrite)
person.dump(s)
s.close

# Load objec
var person2: Person
s = newFileStream("pkg/out.yaml", fmRead)
s.load(person2)
echo person2
s.close

type
  AnimalKind = enum akCat, akDog
  Animal = object
    name: string
    case kind: AnimalKind # variant object
    of akCat: purring: int
    of akDog: barking: int

# Dump variant object as a sequence to order descriminator and dependent fields
let cat = Animal(name: "Kotsyub", kind: akCat, purring: 10)
echo cat.dump
s = newFileStream("pkg/out.yaml", fmWrite)
cat.dump(s)
s.close

# Load variant object (use seq[Variant] to load heterogeneous sequence)
var cat2: Animal
s = newFileStream("pkg/out.yaml", fmRead)
s.load(cat2)
echo cat2
s.close

# Load YAML in Nim
var people: seq[tuple[name: string, height: int]]
s = newFileStream("pkg/yaml-sample.yaml", fmRead)
s.load(people)
echo people
s.close
