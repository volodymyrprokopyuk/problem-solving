proc `*`*(a, b: seq[int]): seq[int] =
  newSeq(result, len a)
  for i in 0..<a.len: result[i] = a[i] * b[i]

when isMainModule:
  assert @[1, 2, 3] * @[2, 3, 4] == @[2, 6, 12]
