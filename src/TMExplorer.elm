module TMExplorer exposing (main)

import Browser
import Browser.Navigation as Nav
import Element exposing (Element)
import Element.Background
import Element.Border
import Element.Font
import Element.Input
import Html
import Random
import Symbols exposing (..)
import TuringMachine exposing (..)
import Url exposing (Url)


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , subscriptions = always Sub.none
        , update = update
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }



-- MODEL


{-| Turing machine and URL in order to respond to and generate query URLs dynamically.
-}
type alias Model =
    { tm : TM
    , errorMsg : String
    , key : Nav.Key
    , url : Url
    }


init : () -> Url -> Nav.Key -> ( Model, Cmd msg )
init flags url key =
    let
        tm =
            urlToTm url
    in
    if numProgLines tm.prog == 0 then
        ( { url = url, key = key, errorMsg = "", tm = addStateProg tm }, Cmd.none )

    else
        ( { url = url, key = key, errorMsg = "", tm = tm }, Cmd.none )



-- UPDATE


type Msg
    = DoNothing
    | NewSymbol ProgLine Symbol
    | NewMove ProgLine Move
    | NewTransition ProgLine (Maybe Char)
    | NewComment State String
    | AddState
    | RemoveState
    | Advance
    | ResetTape
    | RandomSymbols
    | NewTape (List Int)
    | Store
    | UrlChanged Url
    | LinkClicked Browser.UrlRequest


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        tm =
            model.tm
    in
    case msg of
        DoNothing ->
            ( model, Cmd.none )

        NewSymbol progLine symbol ->
            ( { model | tm = { tm | prog = tm.prog |> updateWrite progLine symbol } }, Cmd.none )

        NewMove progLine mve ->
            ( { model | tm = { tm | prog = tm.prog |> updateMove progLine mve } }, Cmd.none )

        NewComment state comment ->
            ( { model | tm = { tm | comments = tm.comments |> updateComment state comment } }, Cmd.none )

        NewTransition progLine state ->
            let
                newTm =
                    { tm | prog = tm.prog |> updateTransition progLine state }
            in
            ( { model | tm = newTm, errorMsg = validate newTm }, Cmd.none )

        AddState ->
            ( { model | tm = addStateProg tm }, Cmd.none )

        RemoveState ->
            ( { model | tm = { tm | prog = removeStateProg tm.prog, comments = removeStateComment tm.prog tm.comments } }, Cmd.none )

        Advance ->
            case validate tm of
                "" ->
                    ( { model | tm = iterate tm, errorMsg = "" }, Cmd.none )

                errorMsg ->
                    ( { model | errorMsg = errorMsg }, Cmd.none )

        Store ->
            let
                tmToStore =
                    urlToTm model.url
            in
            -- Only store the latest program with comments, leaving the remaining properties (tape, state, alphabet) unchanged.
            ( model, Nav.pushUrl model.key (model.url.path ++ tmToQuery { tmToStore | prog = tm.prog, comments = tm.comments }) )

        ResetTape ->
            let
                query =
                    model.url.query |> Maybe.withDefault "" |> String.toLower

                originalTape =
                    query |> tapeFromQuery tm.alphabet |> newTape (headFromQuery query)

                resetTm =
                    { tm | currentState = Just 'a', tape = originalTape }
            in
            ( { model | tm = resetTm, errorMsg = validate resetTm }, Cmd.none )

        RandomSymbols ->
            let
                resetTm =
                    { tm | currentState = Just 'a' }
            in
            ( { model | tm = resetTm }, Random.generate NewTape (Random.list 20 (Random.int 0 2)) )

        NewTape ints ->
            let
                newTm =
                    { tm | tape = newTape 0 (List.map numToSym ints) }
            in
            ( { model | tm = newTm, errorMsg = validate newTm }, Cmd.none )

        UrlChanged url ->
            -- If we want to respond to a URL being changed, we can do so here.
            ( model, Cmd.none )

        LinkClicked urlRequest ->
            -- If we want to have URL links in page, we need to respond to them here.
            ( model, Cmd.none )



-- VIEW


type alias Document msg =
    { title : String
    , body : List (Html.Html msg)
    }


highlightColor : Element.Color
highlightColor =
    rgb255 253 233 225


tapeColor : Element.Color
tapeColor =
    rgb255 255 255 255


bgColor : Element.Color
bgColor =
    rgb255 173 228 222


inputColor : Element.Color
inputColor =
    rgb255 255 220 200


commentBoxColor : Element.Color
commentBoxColor =
    rgb255 162 214 208


commentTextColor : Element.Color
commentTextColor =
    rgb255 100 100 100


disabledColor : Element.Color
disabledColor =
    rgb255 221 222 209


buttonShadow : Element.Color
buttonShadow =
    rgb255 180 170 160


view : Model -> Document Msg
view model =
    { title = "The Turing Machine"
    , body =
        [ Element.column
            [ Element.Font.family
                [ Element.Font.external
                    { name = "Special Elite"
                    , url = "https://fonts.googleapis.com/css?family=Special+Elite"
                    }
                , Element.Font.monospace
                ]
            , Element.padding 40
            , Element.centerX
            ]
            (Element.el [ Element.Font.size 48, Element.padding 50, Element.centerX ] (Element.text "The Turing Machine")
                :: tmLabel model.tm.currentState model.tm.comments
                :: viewTape model.tm.alphabet 20 model.tm.tape
                ++ controlButtons model.errorMsg model.tm.currentState
                ++ [ Element.el [ Element.Font.italic, Element.centerX, Element.paddingEach { top = 30, left = 0, right = 0, bottom = 0 } ]
                        (Element.text model.errorMsg)
                   ]
                ++ viewProgram model.tm.comments model.tm.alphabet (prog model.tm.prog)
            )
            |> Element.el
                [ Element.width Element.fill
                , Element.height Element.fill
                , Element.Background.color bgColor
                ]
            |> Element.layoutWith
                { options =
                    [ Element.focusStyle
                        { borderColor = Just (rgb255 117 150 154)
                        , backgroundColor = Nothing
                        , shadow =
                            Just
                                { color = rgb255 117 150 154
                                , offset = ( 0, 0 )
                                , blur = 4
                                , size = 2
                                }
                        }
                    ]
                }
                []
        ]
    }


tmLabel : State -> Comments -> Element msg
tmLabel state comments =
    let
        commentText =
            case commentFor state comments of
                Nothing ->
                    ""

                Just comment ->
                    "  " ++ String.trim comment
    in
    Element.row [ Element.width Element.fill ]
        [ Element.el [ Element.centerX ] (Element.text "State: ")
        , Element.el [ Element.centerX, Element.Font.bold ] (Element.text (stateDisplayText state))
        , Element.el [ Element.centerX, Element.Font.italic, Element.Font.size 18, Element.Font.color commentTextColor ] (Element.text commentText)
        ]


viewTape : Alphabet -> Int -> Tape -> List (Element msg)
viewTape alphabet halfSize ( l, h, r ) =
    let
        leftOfHead =
            l ++ List.repeat halfSize Blank |> List.take halfSize

        rightOfHead =
            r ++ List.repeat halfSize Blank |> List.take halfSize

        tapeText =
            tapeChars alphabet (List.reverse leftOfHead ++ (h :: rightOfHead))

        data =
            [ List.repeat halfSize '\u{00A0}' ++ ('▼' :: List.repeat halfSize '\u{00A0}')
            , tapeText
            , List.repeat (2 * halfSize + 1) '\u{00A0}'
            ]

        cellStyle row index =
            let
                borders =
                    if row /= 1 then
                        { bottom = 0, right = 0, left = 0, top = 0 }

                    else if index == 0 then
                        { bottom = 1, right = 0, left = 0, top = 1 }

                    else
                        { bottom = 1, right = 0, left = 1, top = 1 }

                bg =
                    if row /= 1 then
                        bgColor

                    else if index == 20 then
                        highlightColor

                    else
                        tapeColor
            in
            [ Element.Background.color bg
            , Element.Border.widthEach borders
            , Element.Border.color (rgb255 0 0 0)
            , Element.centerY
            , Element.paddingEach { bottom = 0, right = 0, left = 3, top = 6 }
            ]

        col index =
            { header = Element.none
            , width = Element.fill
            , view = \row tape -> Element.text (stringAt index tape) |> Element.el (cellStyle row index)
            }
    in
    [ Element.indexedTable []
        { data = data
        , columns = List.map col (List.range 0 (List.length tapeText - 1))
        }
    ]


controlButtons : String -> Maybe Char -> List (Element Msg)
controlButtons errMsg currentState =
    [ Element.row [ Element.spacingXY 40 0, Element.centerX ]
        [ if currentState == Nothing || errMsg /= "" then
            Element.Input.button disabledButtonStyle { onPress = Nothing, label = Element.text "Advance" }

          else
            Element.Input.button activeButtonStyle { onPress = Just Advance, label = Element.text "Advance" }
        , Element.Input.button activeButtonStyle { onPress = Just ResetTape, label = Element.text "Reset tape" }
        , Element.Input.button activeButtonStyle { onPress = Just RandomSymbols, label = Element.text "Random tape" }
        ]
    ]


viewProgram : Comments -> Alphabet -> List ProgLine -> List (Element Msg)
viewProgram comments alphabet prog =
    Element.el
        [ Element.centerX
        , Element.paddingEach { top = 30, left = 0, right = 0, bottom = 30 }
        ]
        (progToTable comments alphabet prog)
        :: [ Element.row [ Element.spacing 40, Element.centerX ]
                [ if List.length prog == (26 * 3) then
                    Element.Input.button disabledButtonStyle
                        { onPress = Nothing, label = Element.text "Add state" }

                  else
                    Element.Input.button activeButtonStyle { onPress = Just AddState, label = Element.text "Add state" }
                , if List.length prog == 3 then
                    Element.Input.button disabledButtonStyle
                        { onPress = Nothing, label = Element.text "Remove state" }

                  else
                    Element.Input.button activeButtonStyle
                        { onPress = Just RemoveState, label = Element.text "Remove state" }
                , Element.Input.button activeButtonStyle { onPress = Just Store, label = Element.text "Store program" }
                ]
           ]


progToTable : Comments -> Alphabet -> List ProgLine -> Element Msg
progToTable comments alphabet progLines =
    let
        symbolEdit pl old new =
            let
                oldText =
                    old |> String.toList |> List.map (fromDisplaySymbol alphabet) |> String.fromList

                newText =
                    new |> String.toList |> List.map (fromDisplaySymbol alphabet) |> String.fromList
            in
            case oldText of
                "0" ->
                    if String.contains "1" newText then
                        NewSymbol pl One

                    else if String.contains " " newText || String.contains "blank" newText then
                        NewSymbol pl Blank

                    else
                        DoNothing

                "1" ->
                    if String.contains "0" newText then
                        NewSymbol pl Zero

                    else if String.contains " " newText || String.contains "blank" newText then
                        NewSymbol pl Blank

                    else
                        DoNothing

                "blank" ->
                    if String.contains "0" newText then
                        NewSymbol pl Zero

                    else if String.contains "1" newText then
                        NewSymbol pl One

                    else
                        DoNothing

                _ ->
                    DoNothing

        moveEdit pl oldText newText =
            case oldText of
                "L" ->
                    if newText |> String.toUpper |> String.contains "R" then
                        NewMove pl Right

                    else if String.contains " " newText || String.contains "none" newText then
                        NewMove pl None

                    else
                        DoNothing

                "R" ->
                    if newText |> String.toUpper |> String.contains "L" then
                        NewMove pl Left

                    else if String.contains " " newText || String.contains "none" newText then
                        NewMove pl None

                    else
                        DoNothing

                "none" ->
                    if newText |> String.toUpper |> String.contains "L" then
                        NewMove pl Left

                    else if newText |> String.toUpper |> String.contains "R" then
                        NewMove pl Right

                    else
                        DoNothing

                _ ->
                    DoNothing

        nextStateEdit pl oldText newText =
            let
                lcText =
                    String.filter (\c -> (c >= 'a' && c <= 'z') || c == ' ') newText
            in
            if lcText |> String.contains " " then
                NewTransition pl Nothing

            else
                case lcText |> String.toList of
                    [ c ] ->
                        NewTransition pl (Just c)

                    [ c1, c2 ] ->
                        if [ c1 ] == String.toList oldText then
                            NewTransition pl (Just c2)

                        else
                            NewTransition pl (Just c1)

                    _ ->
                        NewTransition pl Nothing

        readStyle chrsToDisplay =
            let
                -- Necessary to ensure emoji characters are aligned nicely.
                topPad =
                    case List.maximum (List.map Char.toCode chrsToDisplay) of
                        Just n ->
                            if n > 255 then
                                8

                            else
                                0

                        Nothing ->
                            0
            in
            [ Element.centerY
            , Element.paddingEach { top = topPad, right = 0, bottom = 0, left = 8 }
            , Element.Border.color (rgb255 180 180 180)
            , Element.Font.bold
            , Element.Border.width 0
            ]

        inputStyle =
            [ Element.Background.color inputColor
            , Element.width (Element.maximum 65 Element.fill)
            , Element.height (Element.maximum 10 Element.fill)
            ]

        commentStyle =
            [ Element.Background.color commentBoxColor
            , Element.width Element.fill
            , Element.height Element.fill
            , Element.Border.width 0
            , Element.Font.italic
            , Element.Font.size 14
            , Element.padding 12
            , Element.Font.color commentTextColor
            ]

        stateFromTriplet : List ProgLine -> State
        stateFromTriplet triplet =
            case List.head triplet of
                Nothing ->
                    Nothing

                Just pl ->
                    pl.state

        stateComment : List ProgLine -> String
        stateComment triplet =
            case List.head triplet of
                Nothing ->
                    ""

                Just pl ->
                    commentFor pl.state comments |> Maybe.withDefault ""

        stateProg showHeader progLineTriplet =
            Element.row [ Element.spacing 10, Element.width Element.fill ]
                [ Element.table [ Element.Font.size 14, Element.width (Element.fillPortion 2), Element.spacingXY 4 1 ]
                    { data = progLineTriplet
                    , columns =
                        [ { header =
                                if showHeader then
                                    Element.text "State" |> Element.el [ Element.Font.bold, Element.paddingEach { top = 0, left = 0, right = 0, bottom = 10 } ]

                                else
                                    Element.text " " |> Element.el []
                          , width = Element.fill
                          , view =
                                \pl ->
                                    pl.state
                                        |> Maybe.withDefault ' '
                                        |> String.fromChar
                                        |> Element.text
                                        |> Element.el (readStyle [])
                          }
                        , { header =
                                if showHeader then
                                    Element.text "Symbol" |> Element.el [ Element.Font.bold ]

                                else
                                    Element.text " " |> Element.el []
                          , width = Element.fill
                          , view =
                                \pl ->
                                    let
                                        txt =
                                            symbolText alphabet pl.read
                                    in
                                    txt
                                        |> Element.text
                                        |> Element.el (readStyle (String.toList txt))
                          }
                        , { header =
                                if showHeader then
                                    Element.text "Write" |> Element.el [ Element.Font.bold ]

                                else
                                    Element.text " " |> Element.el []
                          , width = Element.shrink
                          , view =
                                \pl ->
                                    Element.Input.text inputStyle
                                        { text = symbolText alphabet pl.write
                                        , placeholder = Nothing
                                        , onChange = symbolEdit pl (symbolText alphabet pl.write)
                                        , label = Element.Input.labelAbove [] Element.none
                                        }
                          }
                        , { header =
                                if showHeader then
                                    Element.text "Move" |> Element.el [ Element.Font.bold ]

                                else
                                    Element.text " " |> Element.el []
                          , width = Element.shrink
                          , view =
                                \pl ->
                                    Element.Input.text inputStyle
                                        { text = moveText pl.move
                                        , placeholder = Nothing
                                        , onChange = moveEdit pl (moveText pl.move)
                                        , label = Element.Input.labelAbove [] Element.none
                                        }
                          }
                        , { header =
                                if showHeader then
                                    Element.text "Next" |> Element.el [ Element.Font.bold ]

                                else
                                    Element.text " " |> Element.el []
                          , width = Element.shrink
                          , view =
                                \pl ->
                                    Element.Input.text inputStyle
                                        { text = stateDisplayText pl.next
                                        , placeholder = Nothing
                                        , onChange = nextStateEdit pl (stateDisplayText pl.next)
                                        , label = Element.Input.labelAbove [] Element.none
                                        }
                          }
                        ]
                    }
                , Element.Input.multiline commentStyle
                    { text = stateComment progLineTriplet
                    , placeholder = Nothing
                    , onChange = \txt -> NewComment (stateFromTriplet progLineTriplet) txt
                    , label =
                        if showHeader then
                            Element.Input.labelAbove [ Element.Font.size 14, Element.Font.bold, Element.paddingEach { top = 0, left = 0, right = 0, bottom = 10 } ] (Element.text "Comment")

                        else
                            Element.Input.labelAbove [] (Element.text " ")
                    , spellcheck = True
                    }
                ]
    in
    Element.column [] (stateProg True (List.take 3 progLines) :: List.map (stateProg False) (triplets (List.drop 3 progLines) []))


activeButtonStyle : List (Element.Attr () msg)
activeButtonStyle =
    [ Element.Background.color inputColor
    , Element.Border.shadow { blur = 10, color = buttonShadow, offset = ( 4, 4 ), size = 2 }
    , Element.Border.rounded 4
    , Element.Font.size 14
    , Element.Font.bold
    , Element.paddingEach { top = 8, left = 8, bottom = 6, right = 8 }
    ]


disabledButtonStyle : List (Element.Attr () msg)
disabledButtonStyle =
    [ Element.Background.color disabledColor
    , Element.Border.rounded 4
    , Element.Font.size 14
    , Element.Font.color (rgb255 150 150 150)
    , Element.padding 8
    ]


stringAt : Int -> List Char -> String
stringAt i =
    if i < 0 then
        always " "

    else
        List.drop i >> List.head >> Maybe.withDefault ' ' >> String.fromChar


rgb255 : Float -> Float -> Float -> Element.Color
rgb255 r g b =
    Element.rgb (r / 255) (g / 255) (b / 255)


triplets : List a -> List (List a) -> List (List a)
triplets xs xss =
    case xs of
        [] ->
            List.reverse xss

        h1 :: h2 :: h3 :: tail ->
            triplets tail ([ h1, h2, h3 ] :: xss)

        _ ->
            []
