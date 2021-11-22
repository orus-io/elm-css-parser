module CSS.Parser exposing
    ( run
    , Block, Property
    , parser, toString, toStringCustom
    )

{-|

@docs run


# Data

@docs Block, Property


# Internals

If you are building a parser of your own using elm/parser and you need to
parse HTML... This section is for you!

@docs parser, toString, toStringCustom

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


{-| A CSS Rule
-}
type alias Block =
    { selectors : List String
    , properties : List Property
    }


{-| A CSS property (a declaration)
-}
type alias Property =
    ( String, String )


{-| Parse CSS stylesheet. See <https://www.w3.org/TR/css-syntax-3/#parser-diagrams>
-}
run : String -> Result (List DeadEnd) (List Block)
run =
    Parser.run parser


{-| Parse a CSS stylesheet.

You can use this in your own parser to parse CSS parts

-}
parser : Parser (List Block)
parser =
    blocks


blocks : Parser (List Block)
blocks =
    loop [] blocksHelp


blocksHelp : List Block -> Parser (Step (List Block) (List Block))
blocksHelp revBlocks =
    oneOf
        [ succeed (\b -> Loop (b :: revBlocks))
            |= block
            |. spaces
        , succeed ()
            |> map (\_ -> Done (List.reverse revBlocks))
        ]


block : Parser Block
block =
    succeed Block
        |= selectors
        |= propertyBlock


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


propertyBlock : Parser (List Property)
propertyBlock =
    sequence
        { start = "{"
        , separator = ";"
        , end = "}"
        , spaces = spaces
        , item = property
        , trailing = Optional
        }


property : Parser Property
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


{-| Turn a list of rules into a string, with custom indent and rule separator
-}
toStringCustom :
    { indent : String
    , blockSeparator : String
    }
    -> List Block
    -> String
toStringCustom settings =
    List.map (blockToString settings.indent)
        >> String.join settings.blockSeparator


{-| Turn a list of rules into a string
-}
toString : List Block -> String
toString =
    toStringCustom { indent = "  ", blockSeparator = "\n\n" }


blockToString : String -> Block -> String
blockToString indent b =
    String.join ", " b.selectors
        ++ " "
        ++ propertyBlockToString indent b.properties


propertyBlockToString : String -> List Property -> String
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


propertyToString : Property -> String
propertyToString ( name, value ) =
    name ++ ": " ++ value ++ ";"
