import std/[strformat, strutils, parseutils, sugar]
import std/threadpool
import pkg/regex

const
  pcFile = "/home/vlad/Downloads/wikipedia/pagecounts-sample"
  # pcFile = "/home/vlad/Downloads/wikipedia/pagecounts-20160101-050000"
  rePageCount = re"^(?P<dom>[\S]+) (?P<tit>[\S]+) (?P<cnt>\d+) (?P<siz>\d+)$"

type
  PageCount = object
    domainCode, pageTitle: string
    viewCount, pageSize: int

func newPageCount(
  domainCode: string,
  pageTitle: string,
  viewCount: int,
  pageSize: int): PageCount =
  PageCount(
    domainCode: domainCode,
    pageTitle: pageTitle,
    viewCount: viewCount,
    pageSize: pageSize)

proc parseRegex(line: string): PageCount = # DSL, slow
  var m: RegexMatch
  if line.find(rePageCount, m):
    try:
      result = newPageCount(
        m.groupFirstCapture("dom", line),
        m.groupFirstCapture("tit", line),
        m.groupFirstCapture("cnt", line).parseInt,
        m.groupFirstCapture("siz", line).parseInt)
    except ValueError as error:
      stderr.write(fmt "ERROR: invalid component {error.msg}\n")
  else:
    stderr.write(fmt "WARNING: invalid line: {line}\n")

proc parseSplit(line: string): PageCount = # simple, slower
  let items = line.split(" ")
  if items.len == 4:
    try:
      result = newPageCount(
        items[0], items[1], items[2].parseInt, items[3].parseInt)
    except ValueError as error:
      stderr.write(fmt "ERROR: invalid component {error.msg}\n")
  else:
    stderr.write(fmt "WARNING: invalid line: {line}\n")

proc parseUntil(line: string): PageCount = # flexible, fast
  var i = 0
  try:
    i.inc parseUntil(line, result.domainCode, {' '}, i)
    i.inc
    i.inc parseUntil(line, result.pageTitle, {' '}, i)
    i.inc
    i.inc parseInt(line, result.viewCount, i)
    i.inc
    i.inc parseInt(line, result.pageSize, i)
  except ValueError as error:
    stderr.write(fmt "ERROR: invalid component {error.msg}\n")

proc parseFile(
  file: string,
  parse: (line: string) -> PageCount): seq[PageCount] =
  for line in file.lines: result.add(line.parse)

proc parseString(
  content: string,
  parse: proc(line: string): PageCount): seq[PageCount] =
  for line in content.splitLines: result.add(line.parse)

proc maxViewCount(pcounts: seq[PageCount], domainCode = "en"): PageCount =
  for pcount in pcounts:
    if pcount.domainCode == domainCode and pcount.viewCount > result.viewCount:
      result = pcount

iterator chunks(file: string, size = 1_000_000): string =
  var
    file = file.open
    buffer = newString(size)
    lastLine = ""
  defer: file.close
  while not file.endOfFile:
    let
      chars = file.readChars(buffer)
      bEnd = buffer.rfind({'\n'})
    if bEnd == -1:
      yield lastLine & buffer[0..<chars]
      lastLine = ""
    else:
      yield lastLine & buffer[0..<bEnd]
      lastLine = buffer[bEnd + 1..<chars]
    buffer = newString(size)

proc parseParallel(
  file: string,
  parse: (string) -> PageCount,
  size = 1_000_000): seq[FlowVar[seq[PageCount]]] =
  for chunk in file.chunks(size):
    result.add(spawn chunk.parseString(parse))

proc maxViewCount(
  pcounts: seq[FlowVar[seq[PageCount]]],
  domainCode = "en"): PageCount =
  for fvPcount in pcounts:
    for pcount in ^fvPcount:
      if pcount.domainCode == domainCode and
         pcount.viewCount > result.viewCount:
        result = pcount

# let
#   # pageCounts = pcFile.parseFile(parseRegex)
#   # pageCounts = pcFile.parseFile(parseSplit)
#   pageCounts = pcFile.parseFile(parseUntil)
# echo pageCounts.maxViewCount

# let
#   # pageCounts = pcFile.parseParallel(parseRegex)
#   # pageCounts = pcFile.parseParallel(parseSplit)
#   pageCounts = pcFile.parseParallel(parseUntil)
# echo pageCounts.maxViewCount
