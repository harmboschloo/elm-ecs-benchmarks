port module Main exposing (main)

import Browser
import Color exposing (Color)
import Ecs
import HarmBoschlooEcs as HarmBoschlooEcs
import Html exposing (Html)
import Html.Attributes
import Html.Events
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


benchmarkTypes : List BenchmarkType
benchmarkTypes =
    [ Iterate1
    , Iterate2
    , Iterate3
    ]


benchmarkTypeToString : BenchmarkType -> String
benchmarkTypeToString benchmarkType =
    case benchmarkType of
        Iterate1 ->
            "Iterate1"

        Iterate2 ->
            "Iterate2"

        Iterate3 ->
            "Iterate3"


benchmarkTypeFromString : String -> Maybe BenchmarkType
benchmarkTypeFromString value =
    benchmarkTypes
        |> List.filter (\type_ -> value == benchmarkTypeToString type_)
        |> List.head


type EcsFramework
    = HarmBoschlooEcs


ecsFrameworks : List EcsFramework
ecsFrameworks =
    [ HarmBoschlooEcs
    ]


ecsFrameworkToString : EcsFramework -> String
ecsFrameworkToString ecsFramework =
    case ecsFramework of
        HarmBoschlooEcs ->
            "HarmBoschlooEcs"


ecsFrameworkFromString : String -> Maybe EcsFramework
ecsFrameworkFromString value =
    ecsFrameworks
        |> List.filter (\framwork -> value == ecsFrameworkToString framwork)
        |> List.head


type alias Model =
    { selectedBenchmark : BenchmarkType
    , selectedFramework : EcsFramework
    , state : BenchmarkState
    , nextBenchmarkId : Int
    , results : List BenchmarkResult
    , hinted : List Sample
    }


type BenchmarkState
    = Idle
    | Initializing Benchmark
    | Running Benchmark EcsModel


type alias Benchmark =
    { id : Int
    , updateCount : Int
    , entityCount : Int
    , type_ : BenchmarkType
    , framework : EcsFramework
    }


type EcsModel
    = HarmBoschlooEcsIterate1 (Ecs.World Int HarmBoschlooEcs.Components3 ())
    | HarmBoschlooEcsIterate2 (Ecs.World Int HarmBoschlooEcs.Components3 ())
    | HarmBoschlooEcsIterate3 (Ecs.World Int HarmBoschlooEcs.Components3 ())


type alias BenchmarkResult =
    { benchmark : Benchmark
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
    ( { selectedBenchmark = Iterate1
      , selectedFramework = HarmBoschlooEcs
      , state = Idle
      , nextBenchmarkId = 0
      , results = []
      , hinted = []
      }
    , Cmd.none
    )


type Msg
    = BenchmarkInit ()
    | BenchmarkUpdate ()
    | BenchmarkEnded Data
    | ChangedBenchmarkType String
    | ChangedEcsFramework String
    | PressedRun
    | ToggledResult Int Bool
    | Hinted (List Sample)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        BenchmarkInit _ ->
            case model.state of
                Initializing benchmark ->
                    ( { model | state = Running benchmark (initEcs benchmark) }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        BenchmarkUpdate _ ->
            case model.state of
                Running benchmark ecsModel ->
                    ( { model | state = Running benchmark (updateEcs benchmark ecsModel) }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        BenchmarkEnded data ->
            case model.state of
                Running benchmark _ ->
                    ( { model
                        | state = Idle
                        , results =
                            model.results
                                ++ [ { benchmark = benchmark
                                     , data = data
                                     , enabled = True
                                     }
                                   ]
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        ChangedBenchmarkType value ->
            case model.state of
                Idle ->
                    case benchmarkTypeFromString value of
                        Just benchmarkType ->
                            ( { model | selectedBenchmark = benchmarkType }, Cmd.none )

                        Nothing ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ChangedEcsFramework value ->
            case model.state of
                Idle ->
                    case ecsFrameworkFromString value of
                        Just ecsFramework ->
                            ( { model | selectedFramework = ecsFramework }, Cmd.none )

                        Nothing ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        PressedRun ->
            case model.state of
                Idle ->
                    let
                        benchmark =
                            { id = model.nextBenchmarkId
                            , updateCount = 1000
                            , entityCount = 1000
                            , type_ = model.selectedBenchmark
                            , framework = model.selectedFramework
                            }
                    in
                    ( { model
                        | state = Initializing benchmark
                        , nextBenchmarkId = model.nextBenchmarkId + 1
                      }
                    , runBenchmark (encodeBenchmark benchmark)
                    )

                _ ->
                    ( model, Cmd.none )

        ToggledResult benchmarkId enabled ->
            case model.state of
                Idle ->
                    ( { model
                        | results =
                            List.map
                                (\result ->
                                    if result.benchmark.id == benchmarkId then
                                        { result | enabled = enabled }

                                    else
                                        result
                                )
                                model.results
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        Hinted hinted ->
            case model.state of
                Idle ->
                    ( { model | hinted = hinted }, Cmd.none )

                _ ->
                    ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ initBenchmark BenchmarkInit
        , updateBenchmark BenchmarkUpdate
        , benchmarkEnded BenchmarkEnded
        ]


initEcs : Benchmark -> EcsModel
initEcs benchmark =
    case benchmark.framework of
        HarmBoschlooEcs ->
            case benchmark.type_ of
                Iterate1 ->
                    HarmBoschlooEcsIterate1 (HarmBoschlooEcs.initIterate1 benchmark)

                Iterate2 ->
                    HarmBoschlooEcsIterate2 (HarmBoschlooEcs.initIterate2 benchmark)

                Iterate3 ->
                    HarmBoschlooEcsIterate3 (HarmBoschlooEcs.initIterate3 benchmark)


updateEcs : Benchmark -> EcsModel -> EcsModel
updateEcs _ ecsModel =
    case ecsModel of
        HarmBoschlooEcsIterate1 world ->
            HarmBoschlooEcsIterate1 (HarmBoschlooEcs.updateIterate1 world)

        HarmBoschlooEcsIterate2 world ->
            HarmBoschlooEcsIterate2 (HarmBoschlooEcs.updateIterate2 world)

        HarmBoschlooEcsIterate3 world ->
            HarmBoschlooEcsIterate3 (HarmBoschlooEcs.updateIterate3 world)


benchmarkToString : Benchmark -> String
benchmarkToString benchmark =
    [ String.fromInt benchmark.id
    , benchmarkTypeToString benchmark.type_
    , ecsFrameworkToString benchmark.framework
    , String.fromInt benchmark.entityCount
    , String.fromInt benchmark.updateCount
    ]
        |> String.join "/"


encodeBenchmark : Benchmark -> Encode.Value
encodeBenchmark benchmark =
    Encode.object
        [ ( "id", Encode.int benchmark.id )
        , ( "type", Encode.string (benchmarkTypeToString benchmark.type_) )
        , ( "framework", Encode.string (ecsFrameworkToString benchmark.framework) )
        , ( "entityCount", Encode.int benchmark.entityCount )
        , ( "updateCount", Encode.int benchmark.updateCount )
        ]


view : Model -> Browser.Document Msg
view model =
    { title = "elm-ecs-benchmarks"
    , body =
        [ viewInputs model
        , viewResults model
        ]
    }


viewInputs : Model -> Html Msg
viewInputs model =
    let
        disabled =
            Html.Attributes.disabled (model.state /= Idle)
    in
    Html.div []
        [ Html.select
            [ Html.Events.onInput ChangedBenchmarkType
            , disabled
            ]
            (benchmarkTypes
                |> List.map
                    (\type_ ->
                        let
                            value =
                                benchmarkTypeToString type_
                        in
                        Html.option
                            [ Html.Attributes.value value
                            , Html.Attributes.selected (type_ == model.selectedBenchmark)
                            ]
                            [ Html.text value ]
                    )
            )
        , Html.text " "
        , Html.select
            [ Html.Events.onInput ChangedEcsFramework
            , disabled
            ]
            (ecsFrameworks
                |> List.map
                    (\framework ->
                        let
                            value =
                                ecsFrameworkToString framework
                        in
                        Html.option
                            [ Html.Attributes.value value
                            , Html.Attributes.selected (framework == model.selectedFramework)
                            ]
                            [ Html.text value ]
                    )
            )
        , Html.text " "
        , Html.button
            [ Html.Events.onClick PressedRun
            , disabled
            ]
            [ Html.text "run" ]
        ]


viewResults : Model -> Html Msg
viewResults model =
    let
        results =
            model.results |> List.filter .enabled
    in
    case results of
        [] ->
            Html.div [] [ Html.text "no data" ]

        _ ->
            Html.div []
                [ Html.h3 [] [ Html.text "update time (milliseconds)" ]
                , LineChart.viewCustom (dtChartConfig model) (List.map2 toSeries results colors)
                , Html.p [] [ Html.text "Note: 60 FPS is equivalent to 16.7 milliseconds per frame" ]
                , Html.h3 [] [ Html.text "used heap size (bytes)" ]
                , LineChart.viewCustom (usedHeapSizeChartConfig model) (List.map2 toSeries results colors)
                , Html.h3 [] [ Html.text "total heap size (bytes)" ]
                , LineChart.viewCustom (totalHeapSizeChartConfig model) (List.map2 toSeries results colors)
                ]


toSeries : BenchmarkResult -> Color -> LineChart.Series Sample
toSeries result color =
    LineChart.line
        color
        Dots.none
        (benchmarkToString result.benchmark)
        result.data.samples


dtChartConfig : Model -> LineChart.Config Sample Msg
dtChartConfig model =
    { y = Axis.default 450 "time (ms)" .dt
    , x = Axis.default 1270 "index" (.index >> toFloat)
    , container = containerConfig "line-chart-area-ds"
    , interpolation = Interpolation.monotone
    , intersection = Intersection.default
    , legends = Legends.default
    , events = Events.hoverMany Hinted
    , junk = Junk.hoverMany model.hinted indexFormat dtFormat
    , grid = Grid.dots 1 Colors.gray
    , area = Area.default
    , line = Line.default
    , dots = Dots.default
    }


usedHeapSizeChartConfig : Model -> LineChart.Config Sample Msg
usedHeapSizeChartConfig model =
    { y = Axis.default 450 "size (bytes)" (.usedHeapSize >> toFloat)
    , x = Axis.default 1270 "update index" (.index >> toFloat)
    , container = containerConfig "line-chart-area-used-heap-size"
    , interpolation = Interpolation.monotone
    , intersection = Intersection.default
    , legends = Legends.default
    , events = Events.hoverMany Hinted
    , junk = Junk.hoverMany model.hinted indexFormat usedHeapSizeFormat
    , grid = Grid.dots 1 Colors.gray
    , area = Area.normal 0.05
    , line = Line.default
    , dots = Dots.default
    }


totalHeapSizeChartConfig : Model -> LineChart.Config Sample Msg
totalHeapSizeChartConfig model =
    { y = Axis.default 450 "size (bytes)" (.totalHeapSize >> toFloat)
    , x = Axis.default 1270 "update index" (.index >> toFloat)
    , container = containerConfig "line-chart-area-total-heap-size"
    , interpolation = Interpolation.monotone
    , intersection = Intersection.default
    , legends = Legends.default
    , events = Events.hoverMany Hinted
    , junk = Junk.hoverMany model.hinted indexFormat totalHeapSizeFormat
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
