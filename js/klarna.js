/*
 * Description: calculates balances by category within a period of time in O(n)
 *   - Selects txs from requested categories withing the specified period
 *   - Period start is inclusive and end is exclusive, start < end (optional)
 *   - Initializes balances to zero for all requested categories
 *   - Calculates balances by category for selected txs
 * Assumptions
 *   - All txs are valid
 *   - All txs are in the same currency
 *   - Tx time, period start and end times are in UTC
 */
function getBalanceByCategoryInPeriod(txs, cats, start, end) {
  // Parses tx time, period start and end times
  txs.forEach(tx => tx.time = new Date(tx.time))
  start = new Date(start); end = new Date(end)
  // Selects txs from requested categories withing the specified period
  txs = txs.filter(tx =>
    cats.includes(tx.category) && start <= tx.time && tx.time < end
  )
  // Initializes balances to zero for all requested categories
  const bls = cats.reduce((bls, cat) => (bls[cat] = 0, bls), { })
  // Calculates balances by category for selected txs
  return txs.reduce((bls, tx) => (bls[tx.category] += tx.amount, bls), bls)
}

// const txs = [
//   { amount: -9600, category: "eating", time: "2021-04-08T05:15:56.905Z" },
//   { amount: -5700, category: "entert", time: "2021-04-07T21:16:57.819Z" },
//   { amount: -7400, category: "entert", time: "2021-04-07T22:46:44.071Z" },
//   { amount: -9200, category: "sports", time: "2021-04-05T01:55:16.646Z" },
// ]
// const cats = ["entert", "sports", "education"],
//       start = "2021-04-04T05:15:56.905Z",
//       end = "2021-04-08T21:19:56.905Z"
// const bls = getBalanceByCategoryInPeriod(txs, cats, start, end)
// console.log(bls)

/*
 * Description: categorizes uncategorized txs by thier similarity to already
 *   categorized txs in O(n)
 *   - Builds a categorizer that maps { tx.targetAccount: [{ amount, category }] }
 *   - Categorizes uncategorized txs preserving the original tx order
 * Assumptions
 *   - All txs are valid
 *   - All txs are in the same currency
 *   - A targetAccount can have multiple categories that differ by name or amount
 */
function categorizeSimilarTransactions(txs, tolerance = 1000) {
  // Adds a category to a targetAccount if it is not alredy added
  function addAccCategory(cats, tx) {
    const accCats = cats[tx.targetAccount],
          cat = { amount: tx.amount, category: tx.category }
    if (accCats) {
      if (!accCats.find(({ amount, category }) =>
        tx.amount === amount && tx.category === category)) {
        // Adds a new category
        accCats.push(cat)
      }
    // Adds the first category for a targetAccount
    } else { cats[tx.targetAccount] = [cat] }
    return cats
  }
  // Categorizes an uncategorized transaction to the most specific specific
  // category with the minimal amount difference
  function categorizeTx(tx) {
    if (!tx.category) {
      const accCats = cats[tx.targetAccount]
      if (accCats) {
        // Selects categories within the tolerance of amount diference sorted
        // by the tolerance ascending
        const cats = accCats.reduce((cats, { amount, category }) => {
          const diff = Math.abs(tx.amount - amount)
          if (diff < tolerance) { cats.push({ amount, category, diff }) }
          return cats
        }, []).sort((a, b) => a.diff - b.diff)
        if (cats.length > 0) {
          // Categorizes the txs with the most specific category
          const cat = cats[0]
          tx.category = cat.category //; tx.categorized = true
        }
      }
    }
    return tx
  }
  // Builds a categorizer that maps { tx.targetAccount: [{ amount, category }] }
  const cats = txs.filter(tx => tx.category).reduce(addAccCategory, { })
  // Categorizes uncategorized txs preserving the original tx order
  return txs.map(categorizeTx)
}

const txs = [
  { targetAccount: 'coffee_shop', amount: -620 },
  { targetAccount: 'coffee_shop', amount: -350, category: 'eating_out' },
  { targetAccount: 'coffee_shop', amount: -1690 },
  { targetAccount: 'sports_club', amount: -9200, category: "sports" },
  { targetAccount: 'sports_club', amount: -9100, category: "spa" },
  { targetAccount: 'sports_club', amount: -9000 },
  { targetAccount: 'sports_club', amount: -1500 },
]
const ctxs = categorizeSimilarTransactions(txs)
console.log(ctxs)
