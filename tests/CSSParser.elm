module CSSParser exposing (..)

import CSS.Parser exposing (run, toString)
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


identityTest : String -> String -> Test
identityTest title s =
    test title <|
        \_ ->
            testRunSimple s


parseAndRenderTest : String -> String -> String -> Test
parseAndRenderTest title expects s =
    test title <|
        \_ ->
            testRun expects s


suite : Test
suite =
    describe "The CSS.Parser module"
        [ identityTest
            "no properties"
            "body {}"
        , identityTest
            "one property"
            "body {\n  height: 100%;\n}"
        , parseAndRenderTest
            "no trailing ;"
            "body {\n  height: 100px;\n}"
            "body {\n  height: 100px}"
        , identityTest
            "multiple properties"
            "body {\n  height: 100%;\n  width: 40em;\n}"
        , identityTest
            "multiple blocks"
            """html {
  height: 100%;
  background: white url(/some/file.jpg);
}

body {
  height: 100%;
  width: 40em;
}"""
        , identityTest
            "complex selectors"
            """a.button:hover, a.button#annoying-button {
  background-color: gray;
}"""
        , parseAndRenderTest
            "leading spaces"
            "body {}"
            " body {}"
        , parseAndRenderTest
            "head comment"
            """html {}"""
            """
/*
a comment
*/
html {}
"""
        , parseAndRenderTest
            "tail comment"
            """html {}"""
            """
html {}
/*
a comment
*/
"""
        , parseAndRenderTest
            "comment between rules"
            """html {}

body {}"""
            """
html {}
/*
a comment
*/
body {}
"""
        , parseAndRenderTest
            "comment in block"
            """html {
  background-color: gray;
  color: gray;
}

body {}"""
            """
html {
    /* head */
  background-color: gray;
  /* in */
  color: gray;
  /* tail */
}
body {}
"""
        , parseAndRenderTest
            "comment in declaration"
            """html {
  background-color: gray ;
  color: gray;
}

body {}"""
            """
html {
  background-color /* before column */:/* after column */ gray ;
  color: gray;
}
body {}
"""
        ]
