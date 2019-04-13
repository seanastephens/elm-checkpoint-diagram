module Main exposing (main)

import Browser exposing (sandbox)
import Debug as Debug
import Dict as Dict
import Html as Html
import Html.Attributes as HAttr
import Html.Events as HtmlE
import Http as Http
import Json.Decode as D
import List as List
import Svg exposing (Svg, line, rect, svg)
import Svg.Attributes exposing (cx, cy, fill, height, r, rx, ry, stroke, strokeWidth, style, width, x, x1, x2, y, y1, y2)
import Svg.Events as SvgE


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


type alias CheckpointMetadataDefinition =
    { runtime : String
    }


type alias EdgeMetadata =
    { work : Bool }


type alias EdgeDefinition =
    ( String, String, EdgeMetadata )


type alias RuntimeDefinition =
    { index : Int, color : String }


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


type CheckpointVis
    = Single Checkpoint
    | Group (NonEmptyList Checkpoint)


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


renderCheckpoint : ViewConfig -> CheckpointVis -> Svg StateMsg
renderCheckpoint viewConfig ca =
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
                        , SvgE.onClick (Focus c)
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


graph : Config -> Result String ( List Checkpoint, List Edge )
graph config =
    let
        findPoint : String -> Result String Checkpoint
        findPoint name =
            Result.map2 (\time metadata -> { name = name, time = time, metadata = metadata })
                (Dict.get name config.checkpointToTime
                    |> Result.fromMaybe ("Could not find checkpoint time for '" ++ name ++ "'")
                )
                (Dict.get name config.checkpointToMetadata
                    |> Result.fromMaybe ("Could not find checkpoint metadata for '" ++ name ++ "'")
                    |> Result.andThen
                        (\{ runtime } ->
                            Dict.get runtime config.runtimes
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
            List.map findEdge config.edgeList
                |> List.map (Result.andThen validateEdge)

        maybeEachNode : List (Result String Checkpoint)
        maybeEachNode =
            List.map findPoint (Dict.keys config.checkpointToTime)

        maybeEdges =
            List.foldl
                (Result.map2 (::))
                (Ok [])
                maybeEachEdge

        maybeNodes =
            List.foldl (Result.map2 (::)) (Ok []) maybeEachNode
    in
    Result.map2 Tuple.pair maybeNodes maybeEdges


postProcess : ViewConfig -> ( List Checkpoint, List Edge ) -> ( List CheckpointVis, List Edge )
postProcess viewConfig ( nodes, edges ) =
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
            List.sortBy upper nodes
                |> List.foldl go { runtimeToCheckpoints = Dict.empty, emittedCheckpoints = [] }

        -- And don't forget to "emit" all the pending checkpoints for each runtime
        newNodes =
            result.emittedCheckpoints
                ++ List.map emit (Dict.values result.runtimeToCheckpoints)
    in
    ( newNodes, edges )


type StateMsg
    = Focus Checkpoint
    | ChangeZoom String
    | ToggleShowName Bool


type Msg
    = FailedToLoad String
    | ConfigLoaded Config
    | StateUpdate StateMsg


type alias Config =
    { checkpointToTime : Dict.Dict String Float
    , checkpointToMetadata : Dict.Dict String CheckpointMetadataDefinition
    , edgeList : List EdgeDefinition
    , runtimes : Dict.Dict String RuntimeDefinition
    }


type Focus
    = NoFocus
    | FocusedOn Checkpoint


type Model
    = Loading
    | Loaded State
    | Broken String


type alias State =
    { config : Config
    , focus : Focus
    , viewConfig : ViewConfig
    }


type alias ViewConfig =
    { zoom : Int
    , showNames : Bool
    }


validView : State -> ( List CheckpointVis, List Edge ) -> Html.Html StateMsg
validView state ( nodes, edges ) =
    let
        controls =
            [ Html.div []
                [ Html.text "Zoom"
                , Html.input
                    [ HAttr.type_ "range"
                    , HAttr.min "0"
                    , HAttr.max "100"
                    , HAttr.value (String.fromInt state.viewConfig.zoom)
                    , HtmlE.onInput ChangeZoom
                    ]
                    []
                , Html.text (String.fromFloat (2 ^ (toFloat state.viewConfig.zoom / 10)))
                ]
            , Html.div
                []
                [ Html.text "Show checkpoint names"
                , Html.input [ HAttr.type_ "checkbox", HAttr.checked state.viewConfig.showNames, HtmlE.onCheck ToggleShowName ] []
                ]
            ]

        focusedContent =
            case state.focus of
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
            [ svg [ width "800", height <| String.fromInt (2000 * 2 ^ state.viewConfig.zoom) ] <|
                List.concat
                    [ List.map (renderEdge state.viewConfig) edges
                    , List.map (renderCheckpoint state.viewConfig) nodes
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
    case model of
        Loading ->
            Html.text "loading..."

        Broken s ->
            Html.text ("Broken: " ++ s)

        Loaded state ->
            case graph state.config of
                Ok data ->
                    Html.map StateUpdate <| validView state (postProcess state.viewConfig data)

                Err why ->
                    Html.text ("Bad data: " ++ why)


updateState : StateMsg -> State -> ( State, Cmd StateMsg )
updateState msg model =
    case msg of
        Focus k ->
            ( { model | focus = FocusedOn k }, Cmd.none )

        ChangeZoom s ->
            let
                vc =
                    model.viewConfig
            in
            case String.toInt s of
                Just i ->
                    ( { model | viewConfig = { vc | zoom = i } }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        ToggleShowName showNames ->
            let
                vc =
                    model.viewConfig
            in
            ( { model | viewConfig = { vc | showNames = showNames } }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg wholeModel =
    case ( msg, wholeModel ) of
        ( ConfigLoaded config, Loading ) ->
            ( Loaded { config = config, focus = NoFocus, viewConfig = { zoom = 0, showNames = True } }, Cmd.none )

        ( FailedToLoad e, Loading ) ->
            ( Broken e, Cmd.none )

        ( StateUpdate u, Loaded state ) ->
            let
                ( newState, stateCmd ) =
                    updateState u state
            in
            ( Loaded newState, Cmd.map StateUpdate stateCmd )

        ( _, Broken _ ) ->
            ( wholeModel, Cmd.none )

        ( ConfigLoaded _, Loaded _ ) ->
            ( Broken "Config reloaded", Cmd.none )

        ( FailedToLoad _, Loaded _ ) ->
            ( Broken "Load failure after load success", Cmd.none )

        ( StateUpdate _, Loading ) ->
            ( Broken "UI update before UI loaded", Cmd.none )


loadData : Cmd Msg
loadData =
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

        configDecoder : D.Decoder Config
        configDecoder =
            D.map4 Config
                (D.field "checkpointToTime" (D.dict D.float))
                (D.field "checkpointToMetadata" (D.dict metadataDecoder))
                (D.field "edgeList" (D.list edgeDecoder))
                (D.field "runtimes" (D.dict runtimeDefDecoder))

        handleLoadResult : Result Http.Error Config -> Msg
        handleLoadResult response =
            case response of
                Ok config ->
                    ConfigLoaded config

                Err e ->
                    FailedToLoad ("Failed to load data: " ++ Debug.toString e)
    in
    Http.get { url = "data.json", expect = Http.expectJson handleLoadResult configDecoder }


main : Platform.Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( Loading, loadData )
        , subscriptions = \_ -> Sub.none
        , view = view
        , update = update
        }
