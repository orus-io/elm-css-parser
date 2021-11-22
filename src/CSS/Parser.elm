module CSS.Parser exposing (Block, Property, run)

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


type alias Block =
    { selectors : List String
    , properties : List Property
    }


type alias Property =
    ( String, String )


run : String -> Result (List DeadEnd) (List Block)
run =
    Parser.run parser


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
