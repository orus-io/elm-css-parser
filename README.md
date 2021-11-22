# Elm CSS Parser

Elm CSS Parser is a CSS parser written in Elm, based on
[elm/parser](https://package.elm-lang.org/packages/elm/parser/latest/).

```Elm
import CSS.Parser

parsed = CSS.Parser.run "body { background: white }"
-- => Ok [{ properties = [("background","white ")], selectors = ["body"] }]

Result.map CSS.Parser.toString parsed
-- => Ok ("body {\n  background: white ;\n}")
```
