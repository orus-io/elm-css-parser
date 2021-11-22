module CSS.Parser exposing
    ( run
    , Stylesheet, Rule, Declaration
    , stylesheet, toString, toStringCustom
    )

{-|

@docs run


# Data

@docs Stylesheet, Rule, Declaration


# Internals

If you are building a parser of your own using elm/parser and you need to
parse HTML... This section is for you!

@docs stylesheet, toString, toStringCustom

-}

import Char
import Parser
    exposing
        ( (|.)
        , (|=)
        , DeadEnd
        , Parser
        , Step(..)
        , Trailing(..)
        , loop
        , map
        , oneOf
        , sequence
        , spaces
        , succeed
        , symbol
        , variable
        )
import Set


{-| A CSS Stylesheet
-}
type alias Stylesheet =
    List Rule


{-| A CSS Rule
-}
type alias Rule =
    { selectors : List String
    , properties : List Declaration
    }


{-| A CSS declaration
-}
type alias Declaration =
    ( String, String )


{-| Parse CSS stylesheet. See <https://www.w3.org/TR/css-syntax-3/#parser-diagrams>
-}
run : String -> Result (List DeadEnd) Stylesheet
run =
    Parser.run stylesheet


{-| Parse a CSS stylesheet.

You can use this in your own parser to parse CSS parts

-}
stylesheet : Parser Stylesheet
stylesheet =
    loop [] stylesheetHelp


stylesheetHelp : Stylesheet -> Parser (Step Stylesheet Stylesheet)
stylesheetHelp revBlocks =
    oneOf
        [ succeed (\b -> Loop (b :: revBlocks))
            |= rule
            |. spaces
        , succeed ()
            |> map (\_ -> Done (List.reverse revBlocks))
        ]


rule : Parser Rule
rule =
    succeed Rule
        |= selectors
        |= block


selectors : Parser (List String)
selectors =
    sequence
        { start = ""
        , separator = ","
        , end = ""
        , spaces = spaces
        , item = selector
        , trailing = Forbidden
        }


selectorsHelp : List String -> Parser (Step (List String) (List String))
selectorsHelp revSelectors =
    oneOf
        [ succeed (\s -> Loop (s :: revSelectors))
            |= selector
            |. spaces
            |. symbol ","
        , succeed ()
            |> map (\_ -> Done (List.reverse revSelectors))
        ]


selector : Parser String
selector =
    variable
        { start =
            Char.isAlphaNum
        , inner =
            \c ->
                Char.isAlphaNum c
                    || (c == ':')
                    || (c == '#')
                    || (c == '.')
                    || (c == '-')
                    || (c == '_')
        , reserved = Set.empty
        }


block : Parser (List Declaration)
block =
    sequence
        { start = "{"
        , separator = ";"
        , end = "}"
        , spaces = spaces
        , item = property
        , trailing = Optional
        }


property : Parser Declaration
property =
    succeed Tuple.pair
        |= propertyName
        |. spaces
        |. symbol ":"
        |. spaces
        |= propertyValue


propertyName : Parser String
propertyName =
    variable
        { start =
            Char.isAlphaNum
        , inner =
            \c ->
                Char.isAlphaNum c || c == '-'
        , reserved = Set.empty
        }


propertyValue : Parser String
propertyValue =
    variable
        { start =
            Char.isAlphaNum
        , inner =
            \c ->
                c /= ';' && c /= '}'
        , reserved = Set.empty
        }


{-| Turn a stylesheet into a string, with custom indent and rule separator
-}
toStringCustom :
    { indent : String
    , blockSeparator : String
    }
    -> Stylesheet
    -> String
toStringCustom settings =
    List.map (blockToString settings.indent)
        >> String.join settings.blockSeparator


{-| Turn a stylesheet into a string
-}
toString : Stylesheet -> String
toString =
    toStringCustom { indent = "  ", blockSeparator = "\n\n" }


blockToString : String -> Rule -> String
blockToString indent b =
    String.join ", " b.selectors
        ++ " "
        ++ propertyBlockToString indent b.properties


propertyBlockToString : String -> List Declaration -> String
propertyBlockToString indent properties =
    case properties of
        [] ->
            "{}"

        _ ->
            "{\n"
                ++ indent
                ++ (properties
                        |> List.map propertyToString
                        |> String.join ("\n" ++ indent)
                   )
                ++ "\n}"


propertyToString : Declaration -> String
propertyToString ( name, value ) =
    name ++ ": " ++ value ++ ";"
