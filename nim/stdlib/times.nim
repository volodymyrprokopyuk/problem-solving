import std/times

let esLocale = DateTimeLocale(
  MMM: ["Ene", "Feb", "Mar", "Abr", "May", "Jun",
        "Jul", "Ago", "Sep", "Oct", "Nov", "Dec"],
  MMMM: ["Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio",
         "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre"],
  ddd: ["Lun", "Mar", "Mie", "Jue", "Vie", "Sab", "Dom"],
  dddd: ["Luens", "Martes", "Miércoles", "Jueves", "Viernes",
         "Sábado", "Domingo"])

# DateTime = time parts extraction + timezone support
#   - now(), dateTime() constructors
#   - [nanosecond..year] extractors
#   - toTime() DateTime => Time convertor
#   - parse() + format()
let localDt = now() # current local DateTime
echo localDt
echo localDt.utc # current UTC DateTime Zulu
echo localDt.utcOffset # UTC offset in seconds +02:00 = -7200
echo localDt.inZone(utc()) # DateTime in time zone
echo localDt.nanosecond # [nanosecond..year] + .timezone
echo localDt.weekday # .monthday .yearday
echo localDt.isDst # daylight saving time
echo dateTime(2022, mMar, 31, zone = utc()) # create UTC DateTime Zulu
echo dateTime(2022, mMar, 31, 12, 34, 56, 789, local()) # create local DateTime
echo localDt.toTime # Convert DateTime to Time
echo localDt.between(localDt - 1.weeks) # between DateTime in TimeInterval
echo Days.convert(Weeks, 15) # Convert between time units (truncated)
let dt = parse("2022-03-31", "yyyy-MM-dd") # parse DateTime
echo dt
echo dt.format("yyyy, dddd, MMM d", esLocale) # format DateTime with locale

# Time = seconds + nanoseconds + efficient arithmetic (recommended default)
#   - getTime(), initTime() constructors
#   - NO extractors
#   - toUnix(), fromUnix() convertors
#   - inZone(), local(), utc() Time => DateTime convertor
#   - parseTime() + format()
let localT = getTime() # current local Time
echo localT
echo localT.utc # current UTC Time Zulu
echo localT.inZone(utc()) # Time in time zone
echo localT.toUnix # Convert Time to UNIX epoch
echo fromUnix(1648799425) # Convert UNIX epoch to Time
echo initTime(1648799425, 000) # Convert UNIX epoch + nanoseconds to Time
let t = parseTime("2022-03-31", "yyyy-MM-dd", local()) # parse Time
echo t
echo t.format("yyyy, dddd, MMM d") # format Time

# Duration fast (especially with Time) arithmetic
#   - initDuration() constructor
#   - toParts() extractor
# Exact Duration = seconds + nanoseconds = normalized units
# Duration supports [nanoseconds..weeks]
# Use Duration unless support for months and years is needed
echo localDt + initDuration(hours = 1) # create Duration
echo localT - initDuration(hours = 1)
echo initDuration(minutes = 2).inNanoseconds # [inNanoseconds..inWeeks]
let dparts = initDuration(weeks = 1, days = 2, hours = 3, minutes = 4).toParts
echo dparts[Weeks], " ", dparts[Minutes] # Duration parts

# TimeInterval slow (requires timezone information) arithmetic
#   - initTimeInterval(), [nanoseconds..years] constructors
#   - toParts() extractor
# Represents a not-fixed, context-dependent duration in time (not normalized)
# Separate fields for every calendar unit
# TimeInterval supports [nanoseconds..years]
echo localDt + 1.years # create TimeIntervals
echo localT - 1.months
echo localDt + initTimeInterval(weeks = 1, days = 2) # create TimeInterval
let tparts = initTimeInterval(years = 1, nanoseconds = 123).toParts
echo tparts[Years], " ", tparts[Nanoseconds] # TimeInterval parts

# Arithmetic: DateTime/Time +/- Duration/TimeInterval
# Comparison: DateTime/Time </<=/== DateTime/Time
