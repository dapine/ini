# ini

ini is a configuration file parser for Haskell.
It parses plain text files with `key=value` and `[section]` structure and supports LF and CRLF line endings.

ini also supports numeric (integer, double), string (UTF-8 encoded) and boolean values.

## Examples
```ini
; comments are defined after a semicolon symbol until end of line

[sectionName]
key=value

; strings can be bare glyphs or quoted
foo=John Doe
bar="Jane Doe"

flightNumber=666
pi=3.14159265359

trueish=true
falsy=false
```
