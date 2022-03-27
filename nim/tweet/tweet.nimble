# Run: nimble c -r src/webcontroller.nim

# Package
version = "0.1.0"
author = "Volodymyr Prokopyuk"
description = "Twitter clone"
license = "MIT"
srcDir = "src"
namedBin["webcontroller"] = "tweet"

# Dependencies
requires "nim >= 1.6.4", "jester >= 0.5.0", "nimja >= 0.5.4"
