module CSS.Parser.Util exposing (toString, toStringCustom)

import CSS.Parser exposing (..)


toStringCustom :
    { indent : String
    , blockSeparator : String
    }
    -> List Block
    -> String
toStringCustom settings =
    List.map (blockToString settings.indent)
        >> String.join settings.blockSeparator


toString : List Block -> String
toString =
    toStringCustom { indent = "  ", blockSeparator = "\n\n" }


blockToString : String -> Block -> String
blockToString indent block =
    String.join ", " block.selectors
        ++ " "
        ++ propertyBlockToString indent block.properties


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
