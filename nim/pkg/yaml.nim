import std/[streams, times]
import pkg/yaml/[serialization, presenter]

var s: FileStream

type
  Person = object
    name: string
    height: int
    creationTime: Time

proc representObject( # dump of custom object Time
  o: Time, ts: TagStyle = tsNone, c: SerializationContext, tag: Tag) =
  c.put(scalarEvent($o, tag, yAnchorNone))

var people: seq[Person]

# Dump object
people = @[
  Person(name: "Vlad", height: 172, creationTime: getTime()),
  Person(name: "Lana", height: 165, creationTime: getTime()),
  Person(name: "Lada", height: 52, creationTime: getTime())]
echo people.dump
s = newFileStream("pkg/out.yaml", fmWrite)
people.dump(s)
s.close

# Load objec
people = @[]
s = newFileStream("pkg/yaml-sample.yaml", fmRead)
s.load(people)
echo people
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
