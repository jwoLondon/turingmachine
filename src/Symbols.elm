module Symbols exposing (Move(..), Symbol(..), SymbolSet(..), fromDisplaySymbol, moveChar, moveFromChar, moveText, numToSym, stateDisplayText, stateShortText, symbolChar, symbolFromChar, symbolText, tapeChars)


type Move
    = Left
    | Right
    | None


type Symbol
    = Zero
    | One
    | Blank


{-| The symbol set used to encode the turing machine tape.
-}
type SymbolSet
    = Binary
    | CatsAndDogs


{-| Provide a representation of a tape for display.
-}
tapeChars : SymbolSet -> List Symbol -> List Char
tapeChars symbolSet =
    let
        symChar sym =
            case sym of
                Zero ->
                    toDisplaySymbol symbolSet '0'

                One ->
                    toDisplaySymbol symbolSet '1'

                Blank ->
                    -- Non breaking space to ensure cell border.
                    '\u{00A0}'
    in
    List.map symChar


{-| Provide the textual representation of a given symbol for displaying in program.
-}
symbolText : SymbolSet -> Symbol -> String
symbolText symbolSet sym =
    case sym of
        Zero ->
            toDisplaySymbol symbolSet '0' |> String.fromChar

        One ->
            toDisplaySymbol symbolSet '1' |> String.fromChar

        Blank ->
            "blank"


symbolChar : Symbol -> Char
symbolChar sym =
    case sym of
        Zero ->
            '0'

        One ->
            '1'

        _ ->
            '_'


{-| Provide the textual representation of a given move instructions.
-}
moveText : Move -> String
moveText moveInstruction =
    case moveInstruction of
        Left ->
            "L"

        Right ->
            "R"

        None ->
            "none"


moveChar : Move -> Char
moveChar mve =
    case mve of
        Left ->
            'l'

        Right ->
            'r'

        _ ->
            'n'


stateDisplayText : Maybe Char -> String
stateDisplayText state =
    case state of
        Just chr ->
            String.fromChar chr

        Nothing ->
            "HALT"


stateShortText : Maybe Char -> String
stateShortText state =
    case state of
        Just chr ->
            String.fromChar chr

        Nothing ->
            "!"


{-| Provide a symbol from the given character. If character does not match one
of the known symbols it is assumed to represent a blank.
-}
symbolFromChar : SymbolSet -> Char -> Symbol
symbolFromChar symbolSet chr =
    case fromDisplaySymbol symbolSet chr of
        '0' ->
            Zero

        '1' ->
            One

        _ ->
            Blank


{-| Provide a move instruction from the given character. If character does not
match either 'l' or 'r' it is assumed to represent a no move.
-}
moveFromChar : Char -> Move
moveFromChar chr =
    case chr of
        'l' ->
            Left

        'L' ->
            Left

        'r' ->
            Right

        'R' ->
            Right

        _ ->
            None


numToSym : Int -> Symbol
numToSym num =
    case num of
        0 ->
            Zero

        1 ->
            One

        _ ->
            Blank


toDisplaySymbol : SymbolSet -> Char -> Char
toDisplaySymbol symbolSet chr =
    case symbolSet of
        Binary ->
            chr

        CatsAndDogs ->
            case chr of
                '0' ->
                    -- Cat face \u{1f431}
                    '🐱'

                -- Dog face \u{1f436}
                '1' ->
                    '🐶'

                _ ->
                    chr


fromDisplaySymbol : SymbolSet -> Char -> Char
fromDisplaySymbol symbolSet chr =
    case symbolSet of
        Binary ->
            chr

        CatsAndDogs ->
            case chr of
                -- Cat face \u{1f431}
                '🐱' ->
                    '0'

                -- Dog face \u{1f436}
                '🐶' ->
                    '1'

                _ ->
                    chr
