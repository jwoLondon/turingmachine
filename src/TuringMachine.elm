module TuringMachine exposing (Comments, ProgLine, State, TM, Tape, addStateProg, commentFor, headFromQuery, iterate, newTape, numProgLines, prog, removeStateComment, removeStateProg, tapeFromQuery, tmToQuery, updateComment, updateMove, updateTransition, updateWrite, urlToTm, validate)

import Dict exposing (Dict)
import Regex
import Symbols exposing (..)
import Url exposing (Url)
import Url.Builder


{-| A program instruction is uniquely associated with a given state and read symbol,
so these are used to define a dictionary key for program lines.
-}
type alias Hashable a =
    { a | state : State, read : Symbol }


{-| Complete representation of a Turing machine.
-}
type alias TM =
    { tape : Tape
    , prog : Prog
    , currentState : State
    , alphabet : Alphabet
    , comments : Comments
    }


{-| A finite list of symbols to the left of the head, stored in reverse order for
quick access; a 'head' comprising the symbol to read; and a list of symbols to the
right. The tape is assumed to extend blank symbols infinitely to the left and right
of the symbols specified here.
-}
type alias Tape =
    ( List Symbol, Symbol, List Symbol )


{-| A set of program lines where each writes a symbol, possibly moves and sets a new
state. Each program line is contingent on the current state and symbol being read.
The current symbol and state provide the key to the dictionary of program lines.
-}
type alias Prog =
    Dict String ProgLine


{-| Each (non-halting) state can have an optional comment associated with it
referenced by the state label a to z.
-}
type alias Comments =
    Dict Char String


{-| A Turing Machine "5-tuple" instruction. 'write', 'move' and 'next' represent
the actions to be performed when the machine is in the given 'state' and has detected
the given 'read' symbol.
-}
type alias ProgLine =
    { state : State
    , read : Symbol
    , write : Symbol
    , move : Move
    , next : State
    }


{-| Machine state is referenced by Just a lower-case character a to z or Nothing
to represent a halt state.
-}
type alias State =
    Maybe Char


{-| Check the next instruction to execute is valid. If it is not, an error message
is provided; if not, an empty string. -|
-}
validate : TM -> String
validate tm =
    if tm.currentState == Nothing then
        ""

    else
        let
            ( _, scannedSymbol, _ ) =
                tm.tape

            currentHash =
                lineHash { state = tm.currentState, read = scannedSymbol }
        in
        case instruction currentHash tm.prog of
            Nothing ->
                ""

            Just progLine ->
                if progLine.next == Nothing || List.member progLine.next (states tm) then
                    ""

                else
                    "Transition says next state should be '" ++ stateDisplayText progLine.next ++ "' but it is not defined."


{-| Execute an instruction. This is dependent on the current state and the symbol
under the read head.
-}
iterate : TM -> TM
iterate tm =
    if tm.currentState == Nothing then
        tm

    else
        let
            ( _, scannedSymbol, _ ) =
                tm.tape

            currentHash =
                lineHash { state = tm.currentState, read = scannedSymbol }
        in
        case instruction currentHash tm.prog of
            Nothing ->
                tm

            Just progLine ->
                tm
                    |> writeSymbol progLine.write
                    |> move progLine.move
                    |> transition progLine.next


{-| Move the tape head.
-}
move : Move -> TM -> TM
move moveInstruction tm =
    case moveInstruction of
        Left ->
            { tm | tape = tapeRight tm.tape }

        Right ->
            { tm | tape = tapeLeft tm.tape }

        None ->
            tm


{-| Report the optional comment associated with a given state.
-}
commentFor : State -> Comments -> Maybe String
commentFor state comments =
    case state of
        Nothing ->
            Nothing

        Just stateLabel ->
            Dict.get stateLabel comments


{-| Update the comment associated with the given state.
-}
updateComment : State -> String -> Comments -> Comments
updateComment state comment comments =
    case state of
        Nothing ->
            comments

        Just stateLabel ->
            if comment == "" then
                Dict.remove stateLabel comments

            else
                Dict.insert stateLabel (String.left 64 comment) comments


{-| Add a new set of state instructions ot the program.
-}
addStateProg : TM -> TM
addStateProg tm =
    -- Limit to 26 states (a-z) to keep URL query format compact.
    if numProgLines tm.prog >= (26 * 3) then
        tm

    else
        let
            stateLabel =
                Just (Char.toCode 'a' + numProgLines tm.prog // 3 |> Char.fromCode)

            insertStateProg pl1 pl2 pl3 =
                Dict.insert (lineHash pl1) pl1
                    >> Dict.insert (lineHash pl2) pl2
                    >> Dict.insert (lineHash pl3) pl3
        in
        { tm
            | prog =
                insertStateProg
                    { state = stateLabel, read = Blank, write = Blank, move = None, next = Nothing }
                    { state = stateLabel, read = Zero, write = Zero, move = None, next = Nothing }
                    { state = stateLabel, read = One, write = One, move = None, next = Nothing }
                    tm.prog
        }


{-| Remove the last state program instructions (set of 3 program lines relating
to the most recent added state).
-}
removeStateProg : Prog -> Prog
removeStateProg =
    Dict.toList
        >> List.reverse
        >> List.drop 3
        >> List.reverse
        >> Dict.fromList


{-| Remove the comments associated with the last (highest) state if they exist.
-}
removeStateComment : Prog -> Comments -> Comments
removeStateComment program comments =
    let
        highestStateLabel =
            case program |> Dict.values |> List.reverse |> List.head of
                Nothing ->
                    'a'

                Just pl ->
                    case pl.state of
                        Nothing ->
                            'a'

                        Just chr ->
                            chr
    in
    Dict.remove highestStateLabel comments


{-| Transition to the next state
-}
transition : Maybe Char -> TM -> TM
transition state tm =
    { tm | currentState = state }


{-| Write a symobl to the tape at the current head position.
-}
writeSymbol : Symbol -> TM -> TM
writeSymbol sym tm =
    let
        ( l, _, r ) =
            tm.tape
    in
    { tm | tape = ( l, sym, r ) }


{-| Number of instuction lines stored in the given program. This will be a multiple
of the number of available symbols inclunding blanks.
-}
numProgLines : Prog -> Int
numProgLines =
    Dict.size


{-| Provide the program instruction for the given state-symbol hash.
-}
instruction : String -> Prog -> Maybe ProgLine
instruction hash =
    Dict.get hash


{-| Provide all program instructions in alphabetic state order.
-}
prog : Prog -> List ProgLine
prog =
    Dict.values


{-| Update the symbol to write in a program instruction with the given program line.
-}
updateWrite : ProgLine -> Symbol -> Prog -> Prog
updateWrite progLine sym =
    let
        newSym pl =
            { pl | write = sym }
    in
    Dict.update (lineHash progLine) (Maybe.map newSym)


{-| Update the move instruction in a program instruction with the given program line.
-}
updateMove : ProgLine -> Move -> Prog -> Prog
updateMove progLine mve =
    let
        newMove pl =
            { pl | move = mve }
    in
    Dict.update (lineHash progLine) (Maybe.map newMove)


{-| Update the 'next state' in a transition instruction with the given program line.
-}
updateTransition : ProgLine -> State -> Prog -> Prog
updateTransition progLine state =
    let
        newNextState pl =
            { pl | next = state }
    in
    Dict.update (lineHash progLine) (Maybe.map newNextState)


{-| All the states defined in the Turing Machine program.
-}
states : TM -> List State
states tm =
    List.map .state (Dict.values tm.prog)



-- CONVERSION FUNCTIONS
{-
   Compact textual representation of a turning machine is in the form of a URL
   query string. For example,

   tape=00011_0&head=0&&state=b&a=0n!1n!_n!&b=0n!1n!_n!

   where:

    symbols on the tape are one of 0, 1 or _ (a blank);

    head is the position in the tape of the cell to read (0 = start of tape,
    1 = one cell to the right of start etc.);

    state is the current state (with ! indicating halt state)

    a=, b= etc. are the states followed by 3x3 tuples of (write, move, next) in
    0-1-blank order;

    l = move head left, r = move head right, n = no move;

    ! = halt state.

    Slightly more wordy representations are used for the GUI and this module includes
    some functions to translate to and from them.
-}


{-| Generates a URL query string reflecting the Turing Machine including the current state,
the tape, the program and the alphabet. Useful for permalinking the Turing Machine.
-}
tmToQuery : TM -> String
tmToQuery tm =
    let
        ( tLeft, tHead, tRight ) =
            tm.tape

        tapeQuery =
            Just (Url.Builder.string "tape" (List.reverse tLeft ++ tHead :: tRight |> List.map symbolChar |> String.fromList))

        headQuery =
            case List.length tLeft of
                0 ->
                    Nothing

                s ->
                    Just (Url.Builder.string "head" (String.fromInt s))

        currentStateQuery =
            case tm.currentState of
                Just 'a' ->
                    Nothing

                _ ->
                    Just (Url.Builder.string "state" (stateShortText tm.currentState))

        alphabetQuery =
            case tm.alphabet of
                CatsAndDogs ->
                    Just (Url.Builder.string "alphabet" "CatsAndDogs")

                Binary ->
                    Nothing

        progQuery state progQueries =
            let
                tripletText progLine =
                    [ symbolChar progLine.write
                    , moveChar progLine.move
                    , progLine.next |> Maybe.withDefault '!'
                    ]
                        |> String.fromList

                incState st =
                    case st of
                        Just chr ->
                            Just (Char.fromCode (Char.toCode chr + 1))

                        Nothing ->
                            Nothing

                maybeL0 =
                    Dict.get (lineHash { state = state, read = Zero }) tm.prog

                maybeL1 =
                    Dict.get (lineHash { state = state, read = One }) tm.prog

                maybeLb =
                    Dict.get (lineHash { state = state, read = Blank }) tm.prog
            in
            case ( maybeL0, maybeL1, maybeLb ) of
                ( Just l0, Just l1, Just lb ) ->
                    progQuery (incState state) (progQueries ++ [ Just (Url.Builder.string (stateShortText state) (tripletText l0 ++ tripletText l1 ++ tripletText lb)) ])

                _ ->
                    progQueries

        commentQuery =
            let
                stateLabels =
                    [ 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z' ]

                commentText stateLabel =
                    case commentFor (Just stateLabel) tm.comments of
                        Just txt ->
                            Just (Url.Builder.string (String.fromChar stateLabel ++ "Comment") txt)

                        Nothing ->
                            Nothing
            in
            List.map commentText stateLabels
    in
    Url.Builder.toQuery
        ([ tapeQuery, headQuery, currentStateQuery, alphabetQuery ]
            ++ progQuery (Just 'a') []
            ++ commentQuery
            |> List.filterMap identity
        )


{-| Creates a turing machine from a given url.
-}
urlToTm : Url -> TM
urlToTm url =
    let
        query =
            url.query |> Maybe.withDefault "" |> String.toLower

        alphabet =
            alphabetFromQuery query
    in
    { tape = query |> tapeFromQuery alphabet |> newTape (headFromQuery query)
    , prog = progFromQuery alphabet query
    , currentState = stateFromQuery query
    , alphabet = alphabet

    -- TODO: XXX Read comments from URL
    , comments = commentsFromQuery url
    }


newTape : Int -> List Symbol -> Tape
newTape hdPos syms =
    case syms of
        [] ->
            ( [], Symbols.Blank, [] )

        hd :: tl ->
            List.foldl (always tapeLeft) ( [], hd, tl ) (List.range 0 (hdPos - 1))


tapeLeft : Tape -> Tape
tapeLeft ( l, h, r ) =
    case r of
        [] ->
            ( h :: l, Blank, [] )

        hd :: tl ->
            ( h :: l, hd, tl )


tapeRight : Tape -> Tape
tapeRight ( l, h, r ) =
    case l of
        [] ->
            ( [], Blank, h :: r )

        hd :: tl ->
            ( tl, hd, h :: r )


{-| Generate a list of symbols from a given query string representation. If a
query string is poorly formed or does not define tape, generates a blank tape.
-}
tapeFromQuery : Alphabet -> String -> List Symbol
tapeFromQuery alphabet txt =
    case submatches "tape=([01_]+)" txt of
        [ Just tTxt ] ->
            tTxt |> String.toList |> List.map (symbolFromChar alphabet)

        _ ->
            []


{-| Determine the requested alphabet to use from a URL query.
-}
alphabetFromQuery : String -> Alphabet
alphabetFromQuery txt =
    if String.contains "alphabet=catsanddogs" (String.toLower txt) then
        CatsAndDogs

    else
        Binary


{-| Provide a state from a given query string representation. If a
query string is poorly formed or does not define state, generates a start state ('a').
-}
stateFromQuery : String -> State
stateFromQuery txt =
    let
        stateSymbol =
            case submatches "state=([a-z!])" txt of
                [ Just sTxt ] ->
                    sTxt |> String.toList |> List.head |> Maybe.withDefault 'a'

                _ ->
                    'a'
    in
    case stateSymbol of
        '!' ->
            Nothing

        s ->
            Just s


{-| Provide a state from a given query string representation. If a
query string is poorly formed or does not define state, generates a start state ('a').
-}
commentsFromQuery : Url -> Comments
commentsFromQuery url =
    case url.query of
        Nothing ->
            Dict.empty

        Just query ->
            let
                commentQuery str comments =
                    case submatches "([a-z])Comment=(.+)" str of
                        [ Just sLabel, Just sComment ] ->
                            case ( sLabel |> String.toList |> List.head, sComment |> Url.percentDecode ) of
                                ( Just chr, Just txt ) ->
                                    if txt == "" then
                                        comments

                                    else
                                        Dict.insert chr (String.left 64 txt) comments

                                _ ->
                                    comments

                        _ ->
                            comments
            in
            List.foldl commentQuery Dict.empty (String.split "&" query)


{-| Generate a read head position from a given query string representation. If a
query string is poorly formed or does not define a head position, generates a 0.
-}
headFromQuery : String -> Int
headFromQuery txt =
    case submatches "head=(\\d+)" txt of
        [ Just n ] ->
            String.toInt n |> Maybe.withDefault 0

        _ ->
            0


{-| Generate a set of program instructions from a given query string representation.
If a query string is poorly formed or does not define a program, generates and
empty program.
-}
progFromQuery : Alphabet -> String -> Prog
progFromQuery alphabet =
    let
        triplet xss =
            case xss of
                [ a ] :: b :: [] ->
                    records ( a, b )

                _ ->
                    []

        toState chr =
            case chr of
                '!' ->
                    Nothing

                _ ->
                    Just chr

        records ( st, chrs ) =
            case chrs of
                [ w0, m0, n0, w1, m1, n1, w2, m2, n2 ] ->
                    let
                        l1 =
                            { state = toState st, read = Zero, write = symbolFromChar alphabet w0, move = moveFromChar m0, next = toState n0 }

                        l2 =
                            { state = toState st, read = One, write = symbolFromChar alphabet w1, move = moveFromChar m1, next = toState n1 }

                        l3 =
                            { state = toState st, read = Blank, write = symbolFromChar alphabet w2, move = moveFromChar m2, next = toState n2 }
                    in
                    [ ( lineHash l1, l1 ), ( lineHash l2, l2 ), ( lineHash l3, l3 ) ]

                _ ->
                    []
    in
    submatchesUngrouped "&*([a-z])(?:=)((?:[01_][nlr][a-z!]){3})"
        >> List.map (List.filterMap identity)
        >> List.map (List.map String.toList)
        >> -- TODO: Insert integrity check (states a to XXX with no gaps)
           List.concatMap triplet
        >> Dict.fromList


{-| Generate a unique hash from a record containing 'state' and
'read' fields.
-}
lineHash : Hashable a -> String
lineHash progLine =
    String.fromChar (Maybe.withDefault ' ' progLine.state) ++ symbolText Binary progLine.read


{-| Given a regex containing groups (first parameter), will provide a list of sub
(grouped) matches found in the text of the second parameter. Allows regex groups
to be identified and where matched, will be `Just` a match or `Nothing` if the
group does not match. Unlike `submatchesUngrouped` this will concatenate the
matches within each subgroup.
-}
submatches : String -> String -> List (Maybe String)
submatches regex =
    Regex.find
        (Regex.fromString regex |> Maybe.withDefault Regex.never)
        >> List.concatMap .submatches


{-| Given a regex containing groups (first parameter), will provide a list of sub
(grouped) matches found in the text of the second parameter. Allows regex groups
to be identified and where matched, will be `Just` a match or `Nothing` if the
group does not match. Unlike `submatches` this will keep the matches within each
subgroup separate.
-}
submatchesUngrouped : String -> String -> List (List (Maybe String))
submatchesUngrouped regex =
    Regex.find
        (Regex.fromString regex |> Maybe.withDefault Regex.never)
        >> List.map .submatches
