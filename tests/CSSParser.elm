module CSSParser exposing (..)

import CSS.Parser exposing (run)
import CSS.Parser.Util exposing (toString)
import Expect exposing (Expectation)
import Test exposing (..)
import Utils exposing (deadEndsToString)


testRunSimple : String -> Expectation
testRunSimple s =
    testRun s s


testRun : String -> String -> Expectation
testRun expects s =
    Expect.equal expects <|
        case run s of
            Ok result ->
                toString result

            Err deadends ->
                "ERROR: " ++ deadEndsToString deadends


simpleTest : String -> String -> Test
simpleTest title s =
    test title <|
        \_ ->
            testRunSimple s


suite : Test
suite =
    describe "The CSS.Parser module"
        [ simpleTest
            "no properties"
            "body {}"
        , simpleTest
            "one property"
            "body {\n  height: 100%;\n}"
        , simpleTest
            "multiple properties"
            "body {\n  height: 100%;\n  width: 40em;\n}"
        , simpleTest
            "multiple blocks"
            """html {
  height: 100%;
  background: white url(/some/file.jpg);
}

body {
  height: 100%;
  width: 40em;
}"""
        , simpleTest
            "complex selectors"
            """a.button:hover, a.button#annoying-button {
  background-color: gray;
}"""
        ]
