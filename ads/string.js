// O(n) reverses a string
export function reverse(str) {
  let i = 0, j = str.length - 1
  str = str.split("")
  while (i < j) { [str[i++], str[j--]] = [str[j], str[i]] }
  return str.join("")
}

// O(m*n) return index of a sub in a str or -1
export function find(str, sub) {
  const m = sub.length, n = str.length - m
  nextStrPos: for (let i = 0; i <= n; ++i) {
    for (let j = 0; j < m; ++j) {
      if (str[i + j] !== sub[j]) { continue nextStrPos }
    }
    return i
  }
  return -1
}

// O(n) checks if a string is a palindrome
export function palindrome(str) {
  let i = 0, j = str.length - 1
  while (i < j) {
    if (str[i++] !== str[j--]) { return false }
  }
  return true
}
