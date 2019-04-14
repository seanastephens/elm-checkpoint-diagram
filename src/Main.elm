module Main exposing (main)

import Browser
import Debug
import Dict
import Html
import Html.Attributes as HtmlA
import Html.Events as HtmlE
import Http
import Json.Decode as D
import Json.Encode as E
import List
import Svg exposing (Svg, line, rect, svg)
import Svg.Attributes exposing (cx, cy, fill, fillOpacity, height, r, rx, ry, stroke, strokeWidth, style, width, x, x1, x2, y, y1, y2)
import Svg.Events as SvgE



-- Utility class


type alias NonEmptyList t =
    { head : t
    , tail : List t
    }


nonEmptyMap : (t -> s) -> NonEmptyList t -> NonEmptyList s
nonEmptyMap f { head, tail } =
    { head = f head, tail = List.map f tail }


nonEmptyMaximum : NonEmptyList comparable -> comparable
nonEmptyMaximum { head, tail } =
    List.maximum (head :: tail) |> Maybe.withDefault head


nonEmptyMinimum : NonEmptyList comparable -> comparable
nonEmptyMinimum { head, tail } =
    List.minimum (head :: tail) |> Maybe.withDefault head



-- *Definition types (e.g. GraphDefinition) define the JSON representation of a graph


type alias CheckpointMetadataDefinition =
    { runtime : String
    }


type alias EdgeMetadata =
    { work : Bool }


type alias EdgeDefinition =
    ( String, String, EdgeMetadata )


type alias RuntimeDefinition =
    { index : Int, color : String }


type alias GraphDefinition =
    { checkpointToTime : Dict.Dict String Float
    , checkpointToMetadata : Dict.Dict String CheckpointMetadataDefinition
    , edgeList : List EdgeDefinition
    , runtimes : Dict.Dict String RuntimeDefinition
    }



-- Plain name types (e.g. Graph) represent a validated graph; these can be produced from *Definition types.


type alias Checkpoint =
    { time : Float
    , name : String
    , metadata : CheckpointMetadata
    }


type alias CheckpointMetadata =
    { runtime : String
    , runtimeColor : String
    , runtimeIndex : Int
    }


type alias Edge =
    ( Checkpoint, Checkpoint, { work : Bool } )


type alias Graph =
    { checkpoints : List Checkpoint
    , edges : List Edge
    }



-- Finally, *Vis types (e.g. GraphVis) represent a graph transformed for rendering.


type CheckpointVis
    = Single Checkpoint
    | Group (NonEmptyList Checkpoint)


type alias GraphVis =
    { checkpoints : List CheckpointVis
    , edges : List Edge
    }


validateGraphDefinition : GraphDefinition -> Result String Graph
validateGraphDefinition graph =
    let
        findPoint : String -> Result String Checkpoint
        findPoint name =
            Result.map2 (\time metadata -> { name = name, time = time, metadata = metadata })
                (Dict.get name graph.checkpointToTime
                    |> Result.fromMaybe ("Could not find checkpoint time for '" ++ name ++ "'")
                )
                (Dict.get name graph.checkpointToMetadata
                    |> Result.fromMaybe ("Could not find checkpoint metadata for '" ++ name ++ "'")
                    |> Result.andThen
                        (\{ runtime } ->
                            Dict.get runtime graph.runtimes
                                |> Maybe.map
                                    (\def ->
                                        { runtime = runtime, runtimeIndex = def.index, runtimeColor = def.color }
                                    )
                                |> Result.fromMaybe ("Could not find runtime definition for " ++ runtime)
                        )
                )

        findEdge ( a, b, work ) =
            Result.map2 (\l r -> ( l, r, work )) (findPoint a) (findPoint b)

        validateEdge : Edge -> Result String Edge
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
                    ("Checkpoints not in same runtime: " ++ from.name ++ " is in " ++ from.metadata.runtime ++ ", but " ++ to.name ++ " is in " ++ to.metadata.runtime)

        maybeEachEdge : List (Result String Edge)
        maybeEachEdge =
            List.map findEdge graph.edgeList
                |> List.map (Result.andThen validateEdge)

        maybeEachNode : List (Result String Checkpoint)
        maybeEachNode =
            List.map findPoint (Dict.keys graph.checkpointToTime)

        maybeEdges =
            List.foldl
                (Result.map2 (::))
                (Ok [])
                maybeEachEdge

        maybeNodes =
            List.foldl (Result.map2 (::)) (Ok []) maybeEachNode
    in
    Result.map2 (\n e -> { checkpoints = n, edges = e }) maybeNodes maybeEdges


mergeNearbyCheckpoints : ViewConfig -> Graph -> GraphVis
mergeNearbyCheckpoints viewConfig { checkpoints, edges } =
    let
        -- TODO: This is super confusing because Y toward bottom of screen
        upper : Checkpoint -> Float
        upper c =
            checkpointY viewConfig c

        listUpper : NonEmptyList Checkpoint -> Float
        listUpper list =
            nonEmptyMaximum (nonEmptyMap upper list)

        lower : Checkpoint -> Float
        lower c =
            checkpointY viewConfig c

        emit : NonEmptyList Checkpoint -> CheckpointVis
        emit cs =
            case cs.tail of
                [] ->
                    Single cs.head

                _ ->
                    Group cs

        go : Checkpoint -> { runtimeToCheckpoints : Dict.Dict String (NonEmptyList Checkpoint), emittedCheckpoints : List CheckpointVis } -> { runtimeToCheckpoints : Dict.Dict String (NonEmptyList Checkpoint), emittedCheckpoints : List CheckpointVis }
        go next state =
            case Dict.get next.metadata.runtime state.runtimeToCheckpoints of
                -- first checkpoint in this runtime
                Nothing ->
                    { state | runtimeToCheckpoints = Dict.insert next.metadata.runtime (NonEmptyList next []) state.runtimeToCheckpoints }

                -- other checkpoints exist
                Just others ->
                    if lower next < listUpper others + checkpointHeight then
                        -- they are close
                        let
                            newList =
                                NonEmptyList next (others.head :: others.tail)
                        in
                        { state | runtimeToCheckpoints = Dict.insert next.metadata.runtime newList state.runtimeToCheckpoints }

                    else
                        -- they are far away
                        { runtimeToCheckpoints = Dict.insert next.metadata.runtime (NonEmptyList next []) state.runtimeToCheckpoints
                        , emittedCheckpoints = emit others :: state.emittedCheckpoints
                        }

        result =
            List.sortBy upper checkpoints
                |> List.foldl go { runtimeToCheckpoints = Dict.empty, emittedCheckpoints = [] }

        -- And don't forget to "emit" all the pending checkpoints for each runtime
        newCheckpoints =
            result.emittedCheckpoints
                ++ List.map emit (Dict.values result.runtimeToCheckpoints)
    in
    { checkpoints = newCheckpoints, edges = edges }


checkpointX : Checkpoint -> Float
checkpointX c =
    toFloat (c.metadata.runtimeIndex * 250 + 40)


checkpointY : ViewConfig -> Checkpoint -> Float
checkpointY viewConfig c =
    50 + c.time * 450 * (2 ^ (toFloat viewConfig.zoom / 10))


checkpointWidth : Float
checkpointWidth =
    20


checkpointHeight : Float
checkpointHeight =
    20


viewCheckpoint : ViewConfig -> CheckpointVis -> Svg Msg
viewCheckpoint viewConfig ca =
    case ca of
        Single c ->
            let
                rect =
                    Svg.ellipse
                        [ cx <| String.fromFloat (checkpointX c + checkpointWidth / 2)
                        , cy <| String.fromFloat (checkpointY viewConfig c)
                        , rx <| String.fromFloat (checkpointWidth / 2)
                        , ry <| String.fromFloat (checkpointHeight / 2)
                        , fill c.metadata.runtimeColor
                        , stroke "grey"
                        , strokeWidth "2"
                        ]
                        []

                text =
                    Svg.text_
                        [ x <| String.fromFloat (checkpointX c + checkpointWidth + 5)
                        , y <| String.fromFloat (checkpointY viewConfig c + checkpointHeight / 2 + 10 / 2)
                        ]
                        [ Svg.text c.name ]
            in
            case viewConfig.showNames of
                True ->
                    Svg.g [] [ rect, text ]

                False ->
                    Svg.g [] [ rect ]

        Group cs ->
            let
                getCenterY =
                    checkpointY viewConfig

                loY =
                    nonEmptyMinimum (nonEmptyMap getCenterY cs) - checkpointHeight / 2

                hiY =
                    nonEmptyMaximum (nonEmptyMap getCenterY cs) + checkpointHeight / 2

                midY =
                    (hiY + loY) / 2

                rect =
                    Svg.rect
                        [ x <| String.fromFloat (checkpointX cs.head)
                        , y <| String.fromFloat loY
                        , width <| String.fromFloat checkpointWidth
                        , height <| String.fromFloat (hiY - loY)
                        , rx <| String.fromFloat (checkpointWidth / 2)
                        , ry <| String.fromFloat (checkpointWidth / 2)
                        , fill cs.head.metadata.runtimeColor
                        , stroke "grey"
                        , strokeWidth "2"
                        ]
                        []

                text =
                    Svg.text_
                        [ x <| String.fromFloat (checkpointX cs.head + checkpointWidth + 5)
                        , y <| String.fromFloat (midY + checkpointHeight / 2)
                        ]
                        [ Svg.text (String.fromInt (List.length cs.tail + 1) ++ " nodes") ]
            in
            case viewConfig.showNames of
                True ->
                    Svg.g [] [ rect, text ]

                False ->
                    Svg.g [] [ rect ]


viewEdge : ViewConfig -> Edge -> Svg msg
viewEdge viewConfig ( from, to, { work } ) =
    let
        color =
            if work then
                "red"

            else
                "grey"

        width =
            if work then
                String.fromFloat (checkpointWidth * 0.25)

            else
                "2"
    in
    line
        [ x1 <| String.fromFloat (checkpointX from + checkpointWidth / 2)
        , x2 <| String.fromFloat (checkpointX to + checkpointWidth / 2)
        , y1 <| String.fromFloat (checkpointY viewConfig from)
        , y2 <| String.fromFloat (checkpointY viewConfig to)
        , stroke color
        , strokeWidth width
        ]
        []


viewGraph : ViewConfig -> Graph -> List (Svg.Svg Msg)
viewGraph viewConfig graph =
    let
        { edges, checkpoints } =
            mergeNearbyCheckpoints viewConfig graph
    in
    List.map (viewEdge viewConfig) edges
        ++ List.map (viewCheckpoint viewConfig) checkpoints


sectionStyles =
    [ HtmlA.style "float" "left", HtmlA.style "width" "50%" ]


view : Model -> Html.Html Msg
view model =
    let
        headerText =
            case model.graph of
                Loading ->
                    "loading..."

                Broken { lastConfig, error } ->
                    "Broken: " ++ error

                Loaded _ ->
                    "Ok! :)"

        controls =
            [ Html.div []
                [ Html.text "Zoom"
                , Html.input
                    [ HtmlA.type_ "range"
                    , HtmlA.min "0"
                    , HtmlA.max "100"
                    , HtmlA.value (String.fromInt model.viewConfig.zoom)
                    , HtmlE.onInput ChangeZoom
                    ]
                    []
                , Html.text (String.left 6 <| String.fromFloat (2 ^ (toFloat model.viewConfig.zoom / 10)))
                ]
            , Html.div
                []
                [ Html.text "Show checkpoint names"
                , Html.input [ HtmlA.type_ "checkbox", HtmlA.checked model.viewConfig.showNames, HtmlE.onCheck ToggleShowName ] []
                ]
            ]

        errorBackground =
            Svg.rect
                [ width "100%"
                , height "100%"
                , fill <|
                    case model.graph of
                        Broken _ ->
                            "#FEE"

                        _ ->
                            "white"
                ]
                []

        graph =
            case model.graph of
                Loading ->
                    []

                Loaded c ->
                    viewGraph model.viewConfig c

                Broken { lastConfig } ->
                    case lastConfig of
                        Just c ->
                            viewGraph model.viewConfig c

                        Nothing ->
                            []

        graphContainer =
            svg
                [ width "800"
                , height <| String.fromInt (2000 * 2 ^ model.viewConfig.zoom)
                ]
                (errorBackground :: graph)
    in
    Html.main_ []
        [ Html.div [] [ Html.text headerText ]
        , Html.section sectionStyles <|
            controls
                ++ [ graphContainer ]
        , Html.section sectionStyles
            [ Html.textarea
                [ HtmlA.style "width" "100%"
                , HtmlA.style "height" "500px"
                , HtmlA.spellcheck False
                , HtmlE.onInput UpdateGraphDefinitionFrom
                ]
                [ Html.text model.input ]
            ]
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        broken why =
            case model.graph of
                Loaded validConfig ->
                    Broken { lastConfig = Just validConfig, error = why }

                Loading ->
                    Broken { lastConfig = Nothing, error = why }

                Broken { lastConfig } ->
                    Broken { lastConfig = lastConfig, error = why }
    in
    case msg of
        FailedToLoad { error, contents } ->
            ( { model | graph = broken error, input = contents }, Cmd.none )

        UpdateGraphDefinitionFrom s ->
            case D.decodeString decodeConfig s |> Result.mapError (\_ -> "bad json") |> Result.andThen validateGraphDefinition of
                Ok graph ->
                    ( { model | graph = Loaded graph, input = s }, Cmd.none )

                Err e ->
                    ( { model | graph = broken e, input = s }, Cmd.none )

        ChangeZoom s ->
            case String.toInt s of
                Just i ->
                    let
                        vc =
                            model.viewConfig
                    in
                    ( { model | viewConfig = { vc | zoom = i } }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        ToggleShowName showNames ->
            let
                vc =
                    model.viewConfig
            in
            ( { model | viewConfig = { vc | showNames = showNames } }, Cmd.none )


encodeConfig : GraphDefinition -> E.Value
encodeConfig graph =
    let
        encodeRuntimeDef : RuntimeDefinition -> E.Value
        encodeRuntimeDef r =
            E.object [ ( "index", E.int r.index ), ( "color", E.string r.color ) ]

        runtimeDef : E.Value
        runtimeDef =
            E.object <| List.map (Tuple.mapSecond encodeRuntimeDef) <| Dict.toList graph.runtimes

        encodeEdge : EdgeDefinition -> E.Value
        encodeEdge ( from, to, metadata ) =
            E.object
                [ ( "from", E.string from )
                , ( "to", E.string to )
                , ( "work", E.bool metadata.work )
                ]

        edgeDef =
            E.list encodeEdge graph.edgeList

        checkpointTimeDef =
            E.object <| List.map (Tuple.mapSecond E.float) <| Dict.toList graph.checkpointToTime

        encodeCheckpointMeta : CheckpointMetadataDefinition -> E.Value
        encodeCheckpointMeta meta =
            E.object [ ( "runtime", E.string meta.runtime ) ]

        checkpointMetaDef =
            E.object <| List.map (Tuple.mapSecond encodeCheckpointMeta) <| Dict.toList graph.checkpointToMetadata
    in
    E.object
        [ ( "checkpointToTime", checkpointTimeDef )
        , ( "checkpointToMetadata", checkpointMetaDef )
        , ( "edgeList", edgeDef )
        , ( "runtimes", runtimeDef )
        ]


decodeConfig : D.Decoder GraphDefinition
decodeConfig =
    let
        metadataDecoder : D.Decoder CheckpointMetadataDefinition
        metadataDecoder =
            D.map CheckpointMetadataDefinition
                (D.field "runtime" D.string)

        edgeDecoder : D.Decoder EdgeDefinition
        edgeDecoder =
            D.map3 (\from to metadata -> ( from, to, metadata ))
                (D.field "from" D.string)
                (D.field "to" D.string)
                (D.map EdgeMetadata (D.field "work" D.bool))

        runtimeDefDecoder : D.Decoder RuntimeDefinition
        runtimeDefDecoder =
            D.map2 RuntimeDefinition
                (D.field "index" D.int)
                (D.field "color" D.string)
    in
    D.map4 GraphDefinition
        (D.field "checkpointToTime" (D.dict D.float))
        (D.field "checkpointToMetadata" (D.dict metadataDecoder))
        (D.field "edgeList" (D.list edgeDecoder))
        (D.field "runtimes" (D.dict runtimeDefDecoder))


loadData : Cmd Msg
loadData =
    let
        handleLoadResult : Result Http.Error String -> Msg
        handleLoadResult response =
            case response of
                Ok body ->
                    UpdateGraphDefinitionFrom body

                Err e ->
                    FailedToLoad { error = "Failed to load: " ++ Debug.toString e, contents = "" }
    in
    Http.get { url = "data.json", expect = Http.expectString handleLoadResult }


type Msg
    = FailedToLoad { error : String, contents : String }
    | UpdateGraphDefinitionFrom String
    | ChangeZoom String
    | ToggleShowName Bool


type LoadState
    = Loading
    | Loaded Graph
    | Broken { lastConfig : Maybe Graph, error : String }


type alias Model =
    { graph : LoadState
    , input : String
    , viewConfig : ViewConfig
    }


type alias ViewConfig =
    { zoom : Int
    , showNames : Bool
    }


main : Platform.Program () Model Msg
main =
    Browser.element
        { init =
            \_ ->
                ( { graph = Loading
                  , viewConfig = { zoom = 0, showNames = True }
                  , input = ""
                  }
                , loadData
                )
        , subscriptions = \_ -> Sub.none
        , view = view
        , update = update
        }
