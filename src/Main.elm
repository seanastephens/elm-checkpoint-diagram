module Main exposing (main)

import Browser exposing (sandbox)
import Dict as Dict
import Html as Html
import Html.Attributes as HAttr
import List as List
import Svg exposing (Svg, line, rect, svg)
import Svg.Attributes exposing (fill, height, stroke, strokeWidth, style, width, x, x1, x2, y, y1, y2)
import Svg.Events as SvgE


addResult : (t -> r -> s) -> Result (List e) t -> Result (List e) r -> Result (List e) s
addResult f rl rr =
    case ( rl, rr ) of
        ( Ok l, Ok r ) ->
            Ok (f l r)

        ( Ok _, Err r ) ->
            Err r

        ( Err l, Ok _ ) ->
            Err l

        ( Err l, Err r ) ->
            Err (l ++ r)


type alias Checkpoint =
    { time : Float
    , name : String
    , metadata : CheckpointMetadata
    }


type alias CheckpointMetadata =
    { runtime : String
    }


type alias EdgeDefinition =
    ( String, String, { work : Bool } )


type alias Edge =
    ( Checkpoint, Checkpoint, { work : Bool } )



-- paste from hive


checkpoints : Dict.Dict String Float
checkpoints =
    Dict.fromList [ ( "A", 1.0 ), ( "B", 2.5 ), ( "C", 4.1 ) ]



-- manual


checkpointMetadata : Dict.Dict String CheckpointMetadata
checkpointMetadata =
    Dict.fromList
        [ ( "A", { runtime = "py" } )
        , ( "B", { runtime = "ts" } )
        , ( "C", { runtime = "py" } )
        ]


edgeList : List EdgeDefinition
edgeList =
    [ ( "A", "B", { work = False } )
    , ( "B", "C", { work = False } )
    , ( "A", "C", { work = True } )
    ]


checkpointColor : String -> String
checkpointColor name =
    let
        colors =
            Dict.fromList [ ( "A", "pink" ), ( "B", "blue" ), ( "C", "green" ) ]
    in
    Dict.get name colors |> Maybe.withDefault "grey"


checkpointX : Checkpoint -> Float
checkpointX c =
    let
        runtimeToX =
            Dict.fromList [ ( "py", 50 ), ( "ts", 120 ) ]
    in
    Dict.get c.metadata.runtime runtimeToX |> Maybe.withDefault 200


checkpointY : Checkpoint -> Float
checkpointY c =
    50 + c.time * 50


checkpointWidth : Checkpoint -> Float
checkpointWidth _ =
    50


checkpointHeight : Checkpoint -> Float
checkpointHeight _ =
    50


renderCheckpoint : Checkpoint -> Svg Msg
renderCheckpoint c =
    rect
        [ x <| String.fromFloat (checkpointX c)
        , y <| String.fromFloat (50 + c.time * 50)
        , width <| String.fromFloat (checkpointWidth c)
        , height <| String.fromFloat (checkpointHeight c)
        , fill <| checkpointColor c.name
        , SvgE.onClick (Focus c.name)
        ]
        []


renderEdge : Edge -> Svg msg
renderEdge ( from, to, { work } ) =
    line
        [ x1 <| String.fromFloat (checkpointX from + checkpointWidth from / 2)
        , x2 <| String.fromFloat (checkpointX to + checkpointWidth from / 2)
        , y1 <| String.fromFloat (checkpointY from + 50)
        , y2 <| String.fromFloat (checkpointY to)
        , stroke
            (if work then
                "red"

             else
                "grey"
            )
        , strokeWidth "2"
        ]
        []


graph =
    let
        findPoint : String -> Result (List String) Checkpoint
        findPoint e =
            let
                timeR =
                    Result.fromMaybe [ "Could not find checkpoint time for '" ++ e ++ "'" ] <| Dict.get e checkpoints

                metaR =
                    Result.fromMaybe [ "Could not find checkpoint metadata for '" ++ e ++ "'" ] <| Dict.get e checkpointMetadata
            in
            addResult (\time meta -> { time = time, metadata = meta, name = e }) timeR metaR

        findEdge ( a, b, work ) =
            addResult (\l r -> ( l, r, work )) (findPoint a) (findPoint b)

        maybeEachEdge : List (Result (List String) Edge)
        maybeEachEdge =
            List.map findEdge edgeList

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

        maybeGraph =
            addResult (\a b -> ( a, b )) maybeNodes maybeEdges
    in
    case maybeGraph of
        Ok ( nodes, edges ) ->
            svg [ width "400", height "400" ] <|
                List.map renderCheckpoint nodes
                    ++ List.map renderEdge edges

        Err whys ->
            Html.table []
                [ Html.tr [] <|
                    List.map
                        (\why -> Html.td [] [ Html.text why ])
                        whys
                ]


type Msg
    = Focus String


type Model
    = NoFocus
    | FocusedOn String


view : Model -> Html.Html Msg
view model =
    let
        focusedContent =
            case model of
                NoFocus ->
                    [ Html.text "click a node to see details" ]

                FocusedOn k ->
                    [ Html.text k ]
    in
    Html.main_ []
        [ Html.section [] [ graph ]
        , Html.section [] focusedContent
        ]


update : Msg -> Model -> Model
update msg model =
    case msg of
        Focus k ->
            FocusedOn k


main : Platform.Program () Model Msg
main =
    Browser.sandbox
        { init = NoFocus
        , view = view
        , update = update
        }
