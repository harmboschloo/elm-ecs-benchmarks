port module Main exposing (main)

import Browser
import Color exposing (Color)
import Ecs
import HarmBoschlooEcs as HarmBoschlooEcs
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Html.Lazy
import Json.Encode as Encode
import LineChart
import LineChart.Area as Area
import LineChart.Axis as Axis
import LineChart.Axis.Intersection as Intersection
import LineChart.Colors as Colors
import LineChart.Container as Container
import LineChart.Dots as Dots
import LineChart.Events as Events
import LineChart.Grid as Grid
import LineChart.Interpolation as Interpolation
import LineChart.Junk as Junk
import LineChart.Legends as Legends
import LineChart.Line as Line
import List.NonEmpty


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


port runBenchmark : Encode.Value -> Cmd msg


port initBenchmark : (() -> msg) -> Sub msg


port updateBenchmark : (() -> msg) -> Sub msg


port benchmarkEnded : (Data -> msg) -> Sub msg


type BenchmarkType
    = Iterate1
    | Iterate2
    | Iterate3
    | Update1
    | Update2
    | Update3


benchmarkTypes : ( BenchmarkType, List BenchmarkType )
benchmarkTypes =
    ( Iterate1
    , [ Iterate2
      , Iterate3
      , Update1
      , Update2
      , Update3
      ]
    )


benchmarkTypeToString : BenchmarkType -> String
benchmarkTypeToString benchmarkType =
    case benchmarkType of
        Iterate1 ->
            "Iterate1"

        Iterate2 ->
            "Iterate2"

        Iterate3 ->
            "Iterate3"

        Update1 ->
            "Update1"

        Update2 ->
            "Update2"

        Update3 ->
            "Update3"


benchmarkTypeFromString : String -> Maybe BenchmarkType
benchmarkTypeFromString =
    fromString benchmarkTypeToString benchmarkTypes


type EcsFramework
    = HarmBoschlooEcs


ecsFrameworks : ( EcsFramework, List EcsFramework )
ecsFrameworks =
    ( HarmBoschlooEcs
    , []
    )


ecsFrameworkToString : EcsFramework -> String
ecsFrameworkToString ecsFramework =
    case ecsFramework of
        HarmBoschlooEcs ->
            "HarmBoschlooEcs"


ecsFrameworkFromString : String -> Maybe EcsFramework
ecsFrameworkFromString =
    fromString ecsFrameworkToString ecsFrameworks


entityCounts : ( Int, List Int )
entityCounts =
    ( 1000
    , [ 2000
      , 4000
      , 8000
      , 16000
      , 32000
      , 64000
      ]
    )


entityCountToString : Int -> String
entityCountToString entityCount =
    String.fromInt entityCount ++ " entities"


entityCountFromString : String -> Maybe Int
entityCountFromString =
    fromString entityCountToString entityCounts


updateCounts : ( Int, List Int )
updateCounts =
    ( 500
    , [ 1000
      , 2000
      ]
    )


updateCountToString : Int -> String
updateCountToString updateCount =
    String.fromInt updateCount ++ "x"


updateCountFromString : String -> Maybe Int
updateCountFromString =
    fromString updateCountToString updateCounts


type UpdateType
    = LoopUpdate
    | TimerUpdate
    | AnimationFrameUpdate


updateTypes : ( UpdateType, List UpdateType )
updateTypes =
    ( LoopUpdate
    , [ TimerUpdate
      , AnimationFrameUpdate
      ]
    )


updateTypeFromString : String -> Maybe UpdateType
updateTypeFromString =
    fromString updateTypeToString updateTypes


updateTypeToString : UpdateType -> String
updateTypeToString updateType =
    case updateType of
        LoopUpdate ->
            "LoopUpdate"

        TimerUpdate ->
            "TimerUpdate"

        AnimationFrameUpdate ->
            "AnimationFrameUpdate"


type alias Model =
    { ui : UiModel
    , benchmark : BenchmarkModel
    }


type alias UiModel =
    { selectedBenchmark : BenchmarkType
    , selectedFramework : EcsFramework
    , selectedEntityCount : Int
    , selectedUpdateCount : Int
    , selectedUpdateType : UpdateType
    , results : List BenchmarkResult
    , hinted : List Sample
    }


type alias BenchmarkModel =
    { state : BenchmarkState
    , nextId : Int
    }


type BenchmarkState
    = Idle
    | Initializing BenchmarkProperties
    | Running BenchmarkProperties EcsModel


type alias BenchmarkProperties =
    { id : Int
    , type_ : BenchmarkType
    , framework : EcsFramework
    , entityCount : Int
    , updateCount : Int
    , updateType : UpdateType
    }


type EcsModel
    = HarmBoschlooEcsIterate1 (Ecs.World Int HarmBoschlooEcs.Components3 ())
    | HarmBoschlooEcsIterate2 (Ecs.World Int HarmBoschlooEcs.Components3 ())
    | HarmBoschlooEcsIterate3 (Ecs.World Int HarmBoschlooEcs.Components3 ())
    | HarmBoschlooEcsUpdate1 (Ecs.World Int HarmBoschlooEcs.Components3 ())
    | HarmBoschlooEcsUpdate2 (Ecs.World Int HarmBoschlooEcs.Components3 ())
    | HarmBoschlooEcsUpdate3 (Ecs.World Int HarmBoschlooEcs.Components3 ())


type alias BenchmarkResult =
    { properties : BenchmarkProperties
    , data : Data
    , enabled : Bool
    }


type alias Data =
    { samples : List Sample
    }


type alias Sample =
    { index : Int
    , dt : Float
    , usedHeapSize : Int
    , totalHeapSize : Int
    }


init : () -> ( Model, Cmd msg )
init _ =
    ( { ui =
            { selectedBenchmark = Iterate1
            , selectedFramework = HarmBoschlooEcs
            , selectedEntityCount = List.NonEmpty.head entityCounts
            , selectedUpdateCount = List.NonEmpty.head updateCounts
            , selectedUpdateType = LoopUpdate
            , results = []
            , hinted = []
            }
      , benchmark =
            { state = Idle
            , nextId = 0
            }
      }
    , Cmd.none
    )


type Msg
    = BenchmarkInit ()
    | BenchmarkUpdate ()
    | BenchmarkEnded Data
    | ChangedBenchmarkType String
    | ChangedEcsFramework String
    | ChangedEntityCount String
    | ChangedUpdateCount String
    | ChangedUpdateType String
    | PressedRun
    | ToggledResult Int Bool
    | Hinted (List Sample)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        { ui, benchmark } =
            model
    in
    case msg of
        BenchmarkInit _ ->
            case benchmark.state of
                Initializing properties ->
                    ( { model | benchmark = { benchmark | state = Running properties (initEcs properties) } }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        BenchmarkUpdate _ ->
            case model.benchmark.state of
                Running properties ecsModel ->
                    ( { model | benchmark = { benchmark | state = Running properties (updateEcs properties ecsModel) } }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        BenchmarkEnded data ->
            case model.benchmark.state of
                Running properties _ ->
                    ( { benchmark = { benchmark | state = Idle }
                      , ui =
                            { ui
                                | results =
                                    model.ui.results
                                        ++ [ { properties = properties
                                             , data = data
                                             , enabled = True
                                             }
                                           ]
                            }
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        ChangedBenchmarkType string ->
            updateInput benchmarkTypeFromString (\value m -> { m | selectedBenchmark = value }) string model

        ChangedEcsFramework string ->
            updateInput ecsFrameworkFromString (\value m -> { m | selectedFramework = value }) string model

        ChangedEntityCount string ->
            updateInput entityCountFromString (\value m -> { m | selectedEntityCount = value }) string model

        ChangedUpdateCount string ->
            updateInput updateCountFromString (\value m -> { m | selectedUpdateCount = value }) string model

        ChangedUpdateType string ->
            updateInput updateTypeFromString (\value m -> { m | selectedUpdateType = value }) string model

        PressedRun ->
            case benchmark.state of
                Idle ->
                    let
                        properties =
                            { id = benchmark.nextId
                            , type_ = ui.selectedBenchmark
                            , framework = ui.selectedFramework
                            , entityCount = ui.selectedEntityCount
                            , updateCount = ui.selectedUpdateCount
                            , updateType = ui.selectedUpdateType
                            }
                    in
                    ( { model
                        | benchmark =
                            { state = Initializing properties
                            , nextId = benchmark.nextId + 1
                            }
                      }
                    , runBenchmark (encodeProperties properties)
                    )

                _ ->
                    ( model, Cmd.none )

        ToggledResult benchmarkId enabled ->
            case benchmark.state of
                Idle ->
                    ( { model
                        | ui =
                            { ui
                                | results =
                                    List.map
                                        (\result ->
                                            if result.properties.id == benchmarkId then
                                                { result | enabled = enabled }

                                            else
                                                result
                                        )
                                        ui.results
                            }
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        Hinted hinted ->
            case benchmark.state of
                Idle ->
                    ( { model | ui = { ui | hinted = hinted } }, Cmd.none )

                _ ->
                    ( model, Cmd.none )


updateInput : (String -> Maybe a) -> (a -> UiModel -> UiModel) -> String -> Model -> ( Model, Cmd Msg )
updateInput valueFromString updateUi stringValue model =
    case model.benchmark.state of
        Idle ->
            case valueFromString stringValue of
                Just value ->
                    ( { model | ui = updateUi value model.ui }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ initBenchmark BenchmarkInit
        , updateBenchmark BenchmarkUpdate
        , benchmarkEnded BenchmarkEnded
        ]


initEcs : BenchmarkProperties -> EcsModel
initEcs properties =
    case properties.framework of
        HarmBoschlooEcs ->
            case properties.type_ of
                Iterate1 ->
                    HarmBoschlooEcsIterate1 (HarmBoschlooEcs.initIterate1 properties)

                Iterate2 ->
                    HarmBoschlooEcsIterate2 (HarmBoschlooEcs.initIterate2 properties)

                Iterate3 ->
                    HarmBoschlooEcsIterate3 (HarmBoschlooEcs.initIterate3 properties)

                Update1 ->
                    HarmBoschlooEcsUpdate1 (HarmBoschlooEcs.initIterate1 properties)

                Update2 ->
                    HarmBoschlooEcsUpdate2 (HarmBoschlooEcs.initIterate1 properties)

                Update3 ->
                    HarmBoschlooEcsUpdate3 (HarmBoschlooEcs.initIterate1 properties)


updateEcs : BenchmarkProperties -> EcsModel -> EcsModel
updateEcs _ ecsModel =
    case ecsModel of
        HarmBoschlooEcsIterate1 world ->
            HarmBoschlooEcsIterate1 (HarmBoschlooEcs.updateIterate1 world)

        HarmBoschlooEcsIterate2 world ->
            HarmBoschlooEcsIterate2 (HarmBoschlooEcs.updateIterate2 world)

        HarmBoschlooEcsIterate3 world ->
            HarmBoschlooEcsIterate3 (HarmBoschlooEcs.updateIterate3 world)

        HarmBoschlooEcsUpdate1 world ->
            HarmBoschlooEcsUpdate1 (HarmBoschlooEcs.updateUpdate1 world)

        HarmBoschlooEcsUpdate2 world ->
            HarmBoschlooEcsUpdate2 (HarmBoschlooEcs.updateUpdate2 world)

        HarmBoschlooEcsUpdate3 world ->
            HarmBoschlooEcsUpdate3 (HarmBoschlooEcs.updateUpdate3 world)


propertiesToString : BenchmarkProperties -> String
propertiesToString properties =
    [ String.fromInt properties.id
    , benchmarkTypeToString properties.type_
    , ecsFrameworkToString properties.framework
    , String.fromInt properties.entityCount
    , String.fromInt properties.updateCount ++ "x" ++ updateTypeToString properties.updateType
    ]
        |> String.join "/"


encodeProperties : BenchmarkProperties -> Encode.Value
encodeProperties properties =
    Encode.object
        [ ( "id", Encode.int properties.id )
        , ( "type", Encode.string (benchmarkTypeToString properties.type_) )
        , ( "framework", Encode.string (ecsFrameworkToString properties.framework) )
        , ( "entityCount", Encode.int properties.entityCount )
        , ( "updateCount", Encode.int properties.updateCount )
        , ( "updateType", Encode.string (updateTypeToString properties.updateType) )
        ]


view : Model -> Browser.Document Msg
view model =
    { title = "elm-ecs-benchmarks"
    , body =
        [ Html.div [ Html.Attributes.style "font-family" "monospace" ]
            (case model.benchmark.state of
                Idle ->
                    [ Html.Lazy.lazy viewInputs model.ui
                    , Html.Lazy.lazy2 viewResults model.ui.results model.ui.hinted
                    ]

                _ ->
                    [ Html.text "running..." ]
            )
        ]
    }


viewInputs : UiModel -> Html Msg
viewInputs model =
    Html.div []
        [ viewSelect ChangedBenchmarkType benchmarkTypeToString model.selectedBenchmark benchmarkTypes
        , Html.text " "
        , viewSelect ChangedEcsFramework ecsFrameworkToString model.selectedFramework ecsFrameworks
        , Html.text " "
        , viewSelect ChangedEntityCount entityCountToString model.selectedEntityCount entityCounts
        , Html.text " "
        , viewSelect ChangedUpdateCount updateCountToString model.selectedUpdateCount updateCounts
        , Html.text " "
        , viewSelect ChangedUpdateType updateTypeToString model.selectedUpdateType updateTypes
        , Html.text " "
        , Html.button [ Html.Events.onClick PressedRun ] [ Html.text "run" ]
        ]


viewSelect : (String -> Msg) -> (a -> String) -> a -> ( a, List a ) -> Html Msg
viewSelect toMsg toString selected values =
    Html.select
        [ Html.Events.onInput toMsg
        ]
        (values
            |> List.NonEmpty.toList
            |> List.map
                (\value ->
                    let
                        stringValue =
                            toString value
                    in
                    Html.option
                        [ Html.Attributes.value stringValue
                        , Html.Attributes.selected (value == selected)
                        ]
                        [ Html.text stringValue ]
                )
        )


viewResults : List BenchmarkResult -> List Sample -> Html Msg
viewResults results hinted =
    let
        enabledResults =
            results |> List.filter .enabled
    in
    Html.div []
        [ Html.h2 [] [ Html.text "Results" ]
        , Html.div [] (results |> List.map viewResultItem)
        , case enabledResults of
            [] ->
                Html.h3 [] [ Html.text "no data" ]

            _ ->
                Html.div []
                    [ Html.h3 [] [ Html.text "update time (milliseconds)" ]
                    , LineChart.viewCustom (dtChartConfig hinted) (List.map2 toSeries enabledResults colors)
                    , Html.p [] [ Html.text "Note: 60 FPS is equivalent to 16.7 milliseconds per frame" ]
                    , Html.h3 [] [ Html.text "used heap size (bytes)" ]
                    , LineChart.viewCustom (usedHeapSizeChartConfig hinted) (List.map2 toSeries enabledResults colors)
                    , Html.h3 [] [ Html.text "total heap size (bytes)" ]
                    , LineChart.viewCustom (totalHeapSizeChartConfig hinted) (List.map2 toSeries enabledResults colors)
                    ]
        ]


viewResultItem : BenchmarkResult -> Html Msg
viewResultItem result =
    Html.label
        [ Html.Events.onCheck (ToggledResult result.properties.id)
        , Html.Attributes.style "display" "block"
        ]
        [ Html.input
            [ Html.Attributes.type_ "checkbox"
            , Html.Attributes.checked result.enabled
            ]
            []
        , Html.text (propertiesToString result.properties)
        ]


toSeries : BenchmarkResult -> Color -> LineChart.Series Sample
toSeries result color =
    LineChart.line
        color
        Dots.none
        (propertiesToString result.properties)
        result.data.samples


dtChartConfig : List Sample -> LineChart.Config Sample Msg
dtChartConfig hinted =
    { y = Axis.default 450 "time (ms)" .dt
    , x = Axis.default 1270 "index" (.index >> toFloat)
    , container = containerConfig "line-chart-area-ds"
    , interpolation = Interpolation.monotone
    , intersection = Intersection.default
    , legends = Legends.default
    , events = Events.hoverMany Hinted
    , junk = Junk.hoverMany hinted indexFormat dtFormat
    , grid = Grid.dots 1 Colors.gray
    , area = Area.default
    , line = Line.default
    , dots = Dots.default
    }


usedHeapSizeChartConfig : List Sample -> LineChart.Config Sample Msg
usedHeapSizeChartConfig hinted =
    { y = Axis.default 450 "size (bytes)" (.usedHeapSize >> toFloat)
    , x = Axis.default 1270 "update index" (.index >> toFloat)
    , container = containerConfig "line-chart-area-used-heap-size"
    , interpolation = Interpolation.monotone
    , intersection = Intersection.default
    , legends = Legends.default
    , events = Events.hoverMany Hinted
    , junk = Junk.hoverMany hinted indexFormat usedHeapSizeFormat
    , grid = Grid.dots 1 Colors.gray
    , area = Area.normal 0.05
    , line = Line.default
    , dots = Dots.default
    }


totalHeapSizeChartConfig : List Sample -> LineChart.Config Sample Msg
totalHeapSizeChartConfig hinted =
    { y = Axis.default 450 "size (bytes)" (.totalHeapSize >> toFloat)
    , x = Axis.default 1270 "update index" (.index >> toFloat)
    , container = containerConfig "line-chart-area-total-heap-size"
    , interpolation = Interpolation.monotone
    , intersection = Intersection.default
    , legends = Legends.default
    , events = Events.hoverMany Hinted
    , junk = Junk.hoverMany hinted indexFormat totalHeapSizeFormat
    , grid = Grid.dots 1 Colors.gray
    , area = Area.normal 0.05
    , line = Line.default
    , dots = Dots.default
    }


containerConfig : String -> Container.Config Msg
containerConfig id =
    Container.custom
        { attributesHtml = []
        , attributesSvg = []
        , size = Container.relative
        , margin = Container.Margin 30 400 30 100
        , id = id
        }


indexFormat : { a | index : Int } -> String
indexFormat sample =
    String.fromInt sample.index


dtFormat : Sample -> String
dtFormat sample =
    String.fromFloat (round100 sample.dt) ++ " ms"


usedHeapSizeFormat : Sample -> String
usedHeapSizeFormat sample =
    String.fromInt sample.usedHeapSize ++ " bytes"


totalHeapSizeFormat : Sample -> String
totalHeapSizeFormat sample =
    String.fromInt sample.totalHeapSize ++ " bytes"


round100 : Float -> Float
round100 float =
    toFloat (round (float * 100)) / 100


colors : List Color
colors =
    [ Colors.pink
    , Colors.blue
    , Colors.gold
    , Colors.red
    , Colors.green
    , Colors.cyan
    , Colors.teal
    , Colors.purple
    , Colors.rust
    , Colors.strongBlue
    , Colors.pinkLight
    , Colors.blueLight
    , Colors.goldLight
    , Colors.redLight
    , Colors.greenLight
    , Colors.cyanLight
    , Colors.tealLight
    , Colors.purpleLight
    , Colors.black
    , Colors.gray
    , Colors.grayLight
    , Colors.grayLightest
    ]


fromString : (a -> String) -> ( a, List a ) -> String -> Maybe a
fromString toString values stringValue =
    values
        |> List.NonEmpty.toList
        |> List.filter (\value -> stringValue == toString value)
        |> List.head
