/*
 * Assumptions
 * - The `officers` array is not empty
 * - Every object in the array has the `age` and `name` fields
 * - The `age` and `name` fields have valid values
 * - The `name` of a general starts with "General ..."
 */

const officers = [
  {age: 20, name: "Captain Piett"},
  {age: 23, name: "General Veers"},
  {age: 57, name: "General Ozzel"},
  {age: 88, name: "Commander Jerjerrod"}
];

// Returns the sum of all ages of all officers
const ageSum = officers.reduce((ageSum, {age: age}) => ageSum + age, 0)
console.log("Sum of all ages of all officers: %d", ageSum)
// Result: Sum of all ages of all officers: 188

// Returns the name of the oldest officer
const oldest = officers.sort(({age: aAge}, {age: bAge}) => bAge - aAge)[0]
console.log("The older officer: %s", oldest.name)
// Result: The older officer: Commander Jerjerrod

// Returns the average age of all generals
const generals = officers.filter(({name: name}) => name.startsWith("General"))
const ageAvg = generals.reduce((ageSum, {age: age}) => ageSum + age, 0) /
      generals.length
console.log("The average age of all generals: %d", ageAvg)
// Result: The average age of all generals: 40
