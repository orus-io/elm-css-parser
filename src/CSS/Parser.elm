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
        , Nestable(..)
        , Parser
        , Step(..)
        , Trailing(..)
        , chompUntil
        , chompWhile
        , getChompedString
        , getOffset
        , loop
        , map
        , multiComment
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


comments : Parser ()
comments =
    loop 0 <|
        ifProgress <|
            oneOf
                [ multiComment "/*" "*/" NotNestable
                    |. symbol "*/"
                , chompWhile
                    (\c ->
                        (c == ' ')
                            || (c == '\n')
                            || (c == '\t')
                            || (c == '\u{000D}')
                    )
                ]


commentsOrSpace : Parser ()
commentsOrSpace =
    comments
        |. spaces


{-| Parse a CSS stylesheet.

You can use this in your own parser to parse CSS parts

-}
stylesheet : Parser Stylesheet
stylesheet =
    succeed identity
        |. commentsOrSpace
        |= loop [] stylesheetHelp


stylesheetHelp : Stylesheet -> Parser (Step Stylesheet Stylesheet)
stylesheetHelp revBlocks =
    oneOf
        [ succeed (\b -> Loop (b :: revBlocks))
            |= rule
            |. commentsOrSpace
        , succeed (Done (List.reverse revBlocks))
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
        , spaces = commentsOrSpace
        , item = declaration
        , trailing = Optional
        }


declaration : Parser Declaration
declaration =
    succeed Tuple.pair
        |= identToken
        |. commentsOrSpace
        |. symbol ":"
        |. commentsOrSpace
        |= componentValue


identToken : Parser String
identToken =
    variable
        { start =
            Char.isAlphaNum
        , inner =
            \c ->
                Char.isAlphaNum c || c == '-'
        , reserved = Set.empty
        }


componentValue : Parser String
componentValue =
    variable
        { start =
            Char.isAlphaNum
        , inner =
            \c ->
                c /= ';' && c /= '}'
        , reserved = Set.empty
        }


ifProgress : Parser a -> Int -> Parser (Step Int ())
ifProgress parser offset =
    succeed identity
        |. parser
        |= getOffset
        |> map
            (\newOffset ->
                if offset == newOffset then
                    Done ()

                else
                    Loop newOffset
            )


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
