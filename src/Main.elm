module Main exposing (main)

import Browser exposing (sandbox)
import Data exposing (..)
import Dict as Dict
import Html as Html
import Html.Attributes as HAttr
import Html.Events as HtmlE
import List as List
import Svg exposing (Svg, line, rect, svg)
import Svg.Attributes exposing (cx, cy, fill, height, r, rx, ry, stroke, strokeWidth, style, width, x, x1, x2, y, y1, y2)
import Svg.Events as SvgE



-- Result.map2 throws away some errors if both sides are Err


addResult : (t -> r -> s) -> Result (List e) t -> Result (List e) r -> Result (List e) s
addResult f rl rr =
    case ( rl, rr ) of
        ( Err l, Err r ) ->
            Err (l ++ r)

        -- At most one side is Err now, so map2 is fine
        ( l, r ) ->
            Result.map2 f l r


type alias Checkpoint =
    { time : Float
    , name : String
    , metadata : CheckpointMetadata
    }


type alias CheckpointMetadata =
    { runtime : String
    , runtimeIndex : Int
    }


type alias Edge =
    ( Checkpoint, Checkpoint, { work : Bool } )


type CheckpointVis
    = Single Checkpoint
    | Group (List Checkpoint)



-- paste from hive
-- manual


runtimeToIndex : String -> Result (List String) Int
runtimeToIndex runtime =
    let
        go i list =
            case list of
                [] ->
                    Err [ "Could not find runtime '" ++ runtime ++ "'" ]

                x :: xs ->
                    if x == runtime then
                        Ok i

                    else
                        go (i + 1) xs
    in
    go 0 [ "UI", "HTML", "TS" ]


checkpointColor : Checkpoint -> String
checkpointColor c =
    let
        colors =
            Dict.fromList [ ( "UI", "lightgreen" ), ( "HTML", "lightblue" ), ( "TS", "orange" ) ]
    in
    Dict.get c.metadata.runtime colors |> Maybe.withDefault "lightgrey"


checkpointX : Checkpoint -> Float
checkpointX c =
    toFloat (c.metadata.runtimeIndex * 250 + 40)


checkpointY : ViewConfig -> Checkpoint -> Float
checkpointY viewConfig c =
    50 + c.time * 450 * toFloat (2 ^ viewConfig.zoom)


checkpointWidth : Checkpoint -> Float
checkpointWidth _ =
    20


checkpointHeight : Checkpoint -> Float
checkpointHeight _ =
    20


renderCheckpoint : ViewConfig -> CheckpointVis -> Svg Msg
renderCheckpoint viewConfig ca =
    case ca of
        Single c ->
            let
                rect =
                    Svg.ellipse
                        [ cx <| String.fromFloat (checkpointX c + checkpointWidth c / 2)
                        , cy <| String.fromFloat (checkpointY viewConfig c + checkpointHeight c / 2)
                        , rx <| String.fromFloat (checkpointWidth c / 2)
                        , ry <| String.fromFloat (checkpointHeight c / 2)
                        , fill <| checkpointColor c
                        , stroke "grey"
                        , strokeWidth "2"
                        , SvgE.onClick (Focus c)
                        ]
                        []

                text =
                    Svg.text_
                        [ x <| String.fromFloat (checkpointX c + checkpointWidth c + 5)
                        , y <| String.fromFloat (checkpointY viewConfig c + checkpointHeight c / 2 + 10 / 2)
                        ]
                        [ Svg.text c.name ]
            in
            case viewConfig.showNames of
                True ->
                    Svg.g [] [ rect, text ]

                False ->
                    Svg.g [] [ rect ]

        Group _ ->
            Svg.g [] []


renderEdge : ViewConfig -> Edge -> Svg msg
renderEdge viewConfig ( from, to, { work } ) =
    let
        color =
            if work then
                "red"

            else
                "grey"

        width =
            if work then
                String.fromFloat (checkpointWidth from * 0.25)

            else
                "2"
    in
    line
        [ x1 <| String.fromFloat (checkpointX from + checkpointWidth from / 2)
        , x2 <| String.fromFloat (checkpointX to + checkpointWidth from / 2)
        , y1 <| String.fromFloat (checkpointY viewConfig from + checkpointHeight from / 2)
        , y2 <| String.fromFloat (checkpointY viewConfig to + checkpointHeight to / 2)
        , stroke color
        , strokeWidth width
        ]
        []


graph : Result (List String) ( List Checkpoint, List Edge )
graph =
    let
        findPoint : String -> Result (List String) Checkpoint
        findPoint e =
            let
                timeR =
                    Result.fromMaybe [ "Could not find checkpoint time for '" ++ e ++ "'" ] <| Dict.get e checkpoints

                checkpointMetadataDefR : Result (List String) CheckpointMetadataDefinition
                checkpointMetadataDefR =
                    Dict.get e checkpointMetadata
                        |> Result.fromMaybe [ "Could not find checkpoint metadata for '" ++ e ++ "'" ]

                metaRuntimeIndexR : Result (List String) Int
                metaRuntimeIndexR =
                    checkpointMetadataDefR
                        |> Result.andThen (\meta -> runtimeToIndex meta.runtime)

                metaR : Result (List String) CheckpointMetadata
                metaR =
                    Result.map2 (\def index -> { runtime = def.runtime, runtimeIndex = index }) checkpointMetadataDefR metaRuntimeIndexR
            in
            addResult (\time meta -> { time = time, metadata = meta, name = e }) timeR metaR

        findEdge ( a, b, work ) =
            addResult (\l r -> ( l, r, work )) (findPoint a) (findPoint b)

        validateEdge : Edge -> Result (List String) Edge
        validateEdge edge =
            let
                ( from, to, { work } ) =
                    edge
            in
            if not work then
                Ok edge

            else if from.metadata.runtime == to.metadata.runtime then
                Ok edge

            else
                Err
                    [ "Checkpoints not in same runtime: " ++ from.name ++ " is in " ++ from.metadata.runtime ++ ", but " ++ to.name ++ " is in " ++ to.metadata.runtime
                    ]

        maybeEachEdge : List (Result (List String) Edge)
        maybeEachEdge =
            List.map findEdge edgeList
                |> List.map (Result.andThen validateEdge)

        maybeEachNode : List (Result (List String) Checkpoint)
        maybeEachNode =
            List.map findPoint (Dict.keys checkpoints)

        maybeEdges =
            List.foldl
                (addResult (::))
                (Ok [])
                maybeEachEdge

        maybeNodes =
            List.foldl (addResult (::)) (Ok []) maybeEachNode
    in
    addResult (\a b -> ( a, b )) maybeNodes maybeEdges


postProcess : ( List Checkpoint, List Edge ) -> ( List CheckpointVis, List Edge )
postProcess ( nodes, edges ) =
    ( List.map Single nodes, edges )


type Msg
    = Focus Checkpoint
    | ChangeZoom String
    | ToggleShowName Bool


type Focus
    = NoFocus
    | FocusedOn Checkpoint


type alias Model =
    { focus : Focus
    , viewConfig : ViewConfig
    }


type alias ViewConfig =
    { zoom : Int
    , showNames : Bool
    }


validView : Model -> ( List CheckpointVis, List Edge ) -> Html.Html Msg
validView model ( nodes, edges ) =
    let
        controls =
            [ Html.div []
                [ Html.text "Zoom"
                , Html.input
                    [ HAttr.type_ "range"
                    , HAttr.min "0"
                    , HAttr.max "10"
                    , HAttr.value (String.fromInt model.viewConfig.zoom)
                    , HtmlE.onInput ChangeZoom
                    ]
                    []
                , Html.text (String.fromInt (2 ^ model.viewConfig.zoom))
                ]
            , Html.div
                []
                [ Html.text "Show checkpoint names"
                , Html.input [ HAttr.type_ "checkbox", HAttr.checked model.viewConfig.showNames, HtmlE.onCheck ToggleShowName ] []
                ]
            ]

        focusedContent =
            case model.focus of
                NoFocus ->
                    [ Html.text "click a node to see details" ]

                FocusedOn c ->
                    [ Html.table []
                        [ Html.tr []
                            [ Html.td [] [ Html.text "name" ]
                            , Html.td []
                                [ Html.text c.name
                                ]
                            ]
                        , Html.tr []
                            [ Html.td [] [ Html.text "runtime" ]
                            , Html.td [] [ Html.text c.metadata.runtime ]
                            ]
                        ]
                    ]

        vis =
            [ svg [ width "800", height <| String.fromInt (2000 * 2 ^ model.viewConfig.zoom) ] <|
                List.concat
                    [ List.map (renderEdge model.viewConfig) edges
                    , List.map (renderCheckpoint model.viewConfig) nodes
                    ]
            ]
    in
    Html.main_ []
        [ Html.section [] controls
        , Html.section [] vis
        , Html.section [] focusedContent
        ]


view : Model -> Html.Html Msg
view model =
    case graph of
        Ok data ->
            validView model (postProcess data)

        Err whys ->
            Html.table [] <|
                List.map (\why -> Html.tr [] [ Html.td [] [ Html.text why ] ]) whys


update : Msg -> Model -> Model
update msg model =
    case msg of
        Focus k ->
            { model | focus = FocusedOn k }

        ChangeZoom s ->
            let
                vc =
                    model.viewConfig
            in
            case String.toInt s of
                Just i ->
                    { model | viewConfig = { vc | zoom = i } }

                Nothing ->
                    model

        ToggleShowName showNames ->
            let
                vc =
                    model.viewConfig
            in
            { model | viewConfig = { vc | showNames = showNames } }


main : Platform.Program () Model Msg
main =
    Browser.sandbox
        { init = { focus = NoFocus, viewConfig = { zoom = 0, showNames = True } }
        , view = view
        , update = update
        }
