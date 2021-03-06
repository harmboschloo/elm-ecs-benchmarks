port module Main exposing (main)

import Browser
import Color exposing (Color)
import Ecs
import ElmEcs as ElmEcs
import ElmEcsX
import ElmEcsXArrayBaseline
import ElmEcsXDictBaseline
import ElmGameLogic
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


port benchmarkFailed : (String -> msg) -> Sub msg


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


benchmarkTypeDescription : BenchmarkType -> String
benchmarkTypeDescription benchmarkType =
    case benchmarkType of
        Iterate1 ->
            "All entities have 3 components a b c. Iterate over all entities with a."

        Iterate2 ->
            "All entities have 3 components a b c. Iterate over all entities with a & b."

        Iterate3 ->
            "All entities have 3 components a b c. Iterate over all entities with a & b & c."

        Update1 ->
            "All entities have 3 components a b c. Iterate over all entities with a and update a."

        Update2 ->
            "All entities have 3 components a b c. Iterate over all entities with a & b and update a & b."

        Update3 ->
            "All entities have 3 components a b c. Iterate over all entities with a & b & c and update a & b & c."


type EcsFramework
    = ElmEcsX
    | ElmEcs
    | ElmGameLogic
    | JsEcsy
    | JsHyperrEcs
    | ElmEcsXArrayBaseline
    | ElmEcsXDictBaseline


ecsFrameworks : ( EcsFramework, List EcsFramework )
ecsFrameworks =
    ( ElmEcsX
    , [ ElmEcs
      , ElmGameLogic
      , JsEcsy
      , JsHyperrEcs
      , ElmEcsXArrayBaseline
      , ElmEcsXDictBaseline
      ]
    )


ecsFrameworkToString : EcsFramework -> String
ecsFrameworkToString ecsFramework =
    case ecsFramework of
        ElmEcsX ->
            "ElmEcsX"

        ElmEcs ->
            "ElmEcs"

        ElmGameLogic ->
            "ElmGameLogic"

        JsEcsy ->
            "JsEcsy"

        JsHyperrEcs ->
            "JsHyperrEcs"

        ElmEcsXArrayBaseline ->
            "ElmEcsXArrayBaseline"

        ElmEcsXDictBaseline ->
            "ElmEcsXDictBaseline"


ecsFrameworkFromString : String -> Maybe EcsFramework
ecsFrameworkFromString =
    fromString ecsFrameworkToString ecsFrameworks


ecsFrameworkDescription : EcsFramework -> Html msg
ecsFrameworkDescription ecsFramework =
    case ecsFramework of
        ElmEcsX ->
            Html.text "Experimental elm-ecs, archetype based"

        ElmEcs ->
            Html.a
                [ Html.Attributes.href "https://package.elm-lang.org/packages/harmboschloo/elm-ecs/latest/" ]
                [ Html.text "harmboschloo/elm-ecs" ]

        ElmGameLogic ->
            Html.a
                [ Html.Attributes.href "https://package.elm-lang.org/packages/justgook/elm-game-logic/latest/" ]
                [ Html.text "justgook/elm-game-logic" ]

        JsEcsy ->
            Html.a
                [ Html.Attributes.href "https://ecsy.io/" ]
                [ Html.text "https://ecsy.io/" ]

        JsHyperrEcs ->
            Html.a
                [ Html.Attributes.href "https://github.com/gohyperr/hyperr-ecs" ]
                [ Html.text "https://github.com/gohyperr/hyperr-ecs" ]

        ElmEcsXArrayBaseline ->
            Html.text "Array of entity records"

        ElmEcsXDictBaseline ->
            Html.text "Dict of entity records"


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
    = TimerUpdate
    | AnimationFrameUpdate
    | LoopUpdate


updateTypes : ( UpdateType, List UpdateType )
updateTypes =
    ( TimerUpdate
    , [ AnimationFrameUpdate
      , LoopUpdate
      ]
    )


updateTypeToString : UpdateType -> String
updateTypeToString updateType =
    case updateType of
        TimerUpdate ->
            "TimerUpdate"

        AnimationFrameUpdate ->
            "AnimationFrameUpdate"

        LoopUpdate ->
            "LoopUpdate"


updateTypeFromString : String -> Maybe UpdateType
updateTypeFromString =
    fromString updateTypeToString updateTypes


updateTypeDescription : UpdateType -> Html msg
updateTypeDescription updateType =
    case updateType of
        TimerUpdate ->
            Html.span []
                [ Html.text "One update per "
                , Html.i [] [ Html.text "setTimeout" ]
                , Html.text " callback, as fast as possible."
                ]

        AnimationFrameUpdate ->
            Html.span []
                [ Html.text "One update per "
                , Html.i [] [ Html.text "requestAnimationFrame" ]
                , Html.text " callback."
                ]

        LoopUpdate ->
            Html.span []
                [ Html.text "All updates withing one "
                , Html.i [] [ Html.text "for" ]
                , Html.text " loop."
                , Html.text " Fast but doesn't give realistc benchmark results compared to a timer update,"
                , Html.text " because a browser can optimize the loop."
                ]


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
    , error : Maybe Error
    , results : List BenchmarkResult
    , hinted : List Sample
    }


type Error
    = ErrorMessage String


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
    = ElmEcsXIterate1 ElmEcsX.World3
    | ElmEcsXIterate2 ElmEcsX.World3
    | ElmEcsXIterate3 ElmEcsX.World3
    | ElmEcsXUpdate1 ElmEcsX.World3
    | ElmEcsXUpdate2 ElmEcsX.World3
    | ElmEcsXUpdate3 ElmEcsX.World3
    | ElmEcsIterate1 (Ecs.World Int ElmEcs.Components3 ())
    | ElmEcsIterate2 (Ecs.World Int ElmEcs.Components3 ())
    | ElmEcsIterate3 (Ecs.World Int ElmEcs.Components3 ())
    | ElmEcsUpdate1 (Ecs.World Int ElmEcs.Components3 ())
    | ElmEcsUpdate2 (Ecs.World Int ElmEcs.Components3 ())
    | ElmEcsUpdate3 (Ecs.World Int ElmEcs.Components3 ())
    | ElmGameLogicIterate1 ElmGameLogic.World3
    | ElmGameLogicIterate2 ElmGameLogic.World3
    | ElmGameLogicIterate3 ElmGameLogic.World3
    | ElmGameLogicUpdate1 ElmGameLogic.World3
    | ElmGameLogicUpdate2 ElmGameLogic.World3
    | ElmGameLogicUpdate3 ElmGameLogic.World3
    | ElmEcsXArrayBaselineIterate1 ElmEcsXArrayBaseline.World3
    | ElmEcsXArrayBaselineIterate2 ElmEcsXArrayBaseline.World3
    | ElmEcsXArrayBaselineIterate3 ElmEcsXArrayBaseline.World3
    | ElmEcsXArrayBaselineUpdate1 ElmEcsXArrayBaseline.World3
    | ElmEcsXArrayBaselineUpdate2 ElmEcsXArrayBaseline.World3
    | ElmEcsXArrayBaselineUpdate3 ElmEcsXArrayBaseline.World3
    | ElmEcsXDictBaselineIterate1 ElmEcsXDictBaseline.World3
    | ElmEcsXDictBaselineIterate2 ElmEcsXDictBaseline.World3
    | ElmEcsXDictBaselineIterate3 ElmEcsXDictBaseline.World3
    | ElmEcsXDictBaselineUpdate1 ElmEcsXDictBaseline.World3
    | ElmEcsXDictBaselineUpdate2 ElmEcsXDictBaseline.World3
    | ElmEcsXDictBaselineUpdate3 ElmEcsXDictBaseline.World3
    | ExternalModel


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
            , selectedFramework = ElmEcsX
            , selectedEntityCount = List.NonEmpty.head entityCounts
            , selectedUpdateCount = List.NonEmpty.head updateCounts
            , selectedUpdateType = TimerUpdate
            , error = Nothing
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
    | BenchmarkFailed String
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
                    case initEcs properties of
                        Just ecsModel ->
                            ( { model | benchmark = { benchmark | state = Running properties ecsModel } }
                            , Cmd.none
                            )

                        Nothing ->
                            ( { ui = { ui | error = Just (ErrorMessage "Benchmark not available.") }
                              , benchmark = { benchmark | state = Idle }
                              }
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

        BenchmarkFailed message ->
            ( { ui = { ui | error = Just (ErrorMessage message) }
              , benchmark = { benchmark | state = Idle }
              }
            , Cmd.none
            )

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
                    ( { ui = { ui | error = Nothing }
                      , benchmark =
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
                    let
                        ui =
                            updateUi value model.ui
                    in
                    ( { model | ui = { ui | error = Nothing } }, Cmd.none )

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
        , benchmarkFailed BenchmarkFailed
        ]


initEcs : BenchmarkProperties -> Maybe EcsModel
initEcs properties =
    case properties.framework of
        ElmEcsX ->
            Just <|
                case properties.type_ of
                    Iterate1 ->
                        ElmEcsXIterate1 (ElmEcsX.initIterate1 properties)

                    Iterate2 ->
                        ElmEcsXIterate2 (ElmEcsX.initIterate2 properties)

                    Iterate3 ->
                        ElmEcsXIterate3 (ElmEcsX.initIterate3 properties)

                    Update1 ->
                        ElmEcsXUpdate1 (ElmEcsX.initUpdate1 properties)

                    Update2 ->
                        ElmEcsXUpdate2 (ElmEcsX.initUpdate2 properties)

                    Update3 ->
                        ElmEcsXUpdate3 (ElmEcsX.initUpdate3 properties)

        ElmEcs ->
            Just <|
                case properties.type_ of
                    Iterate1 ->
                        ElmEcsIterate1 (ElmEcs.initIterate1 properties)

                    Iterate2 ->
                        ElmEcsIterate2 (ElmEcs.initIterate2 properties)

                    Iterate3 ->
                        ElmEcsIterate3 (ElmEcs.initIterate3 properties)

                    Update1 ->
                        ElmEcsUpdate1 (ElmEcs.initUpdate1 properties)

                    Update2 ->
                        ElmEcsUpdate2 (ElmEcs.initUpdate2 properties)

                    Update3 ->
                        ElmEcsUpdate3 (ElmEcs.initUpdate3 properties)

        ElmGameLogic ->
            Just <|
                case properties.type_ of
                    Iterate1 ->
                        ElmGameLogicIterate1 (ElmGameLogic.initIterate1 properties)

                    Iterate2 ->
                        ElmGameLogicIterate2 (ElmGameLogic.initIterate2 properties)

                    Iterate3 ->
                        ElmGameLogicIterate3 (ElmGameLogic.initIterate3 properties)

                    Update1 ->
                        ElmGameLogicUpdate1 (ElmGameLogic.initUpdate1 properties)

                    Update2 ->
                        ElmGameLogicUpdate2 (ElmGameLogic.initUpdate2 properties)

                    Update3 ->
                        ElmGameLogicUpdate3 (ElmGameLogic.initUpdate3 properties)

        ElmEcsXArrayBaseline ->
            case properties.type_ of
                Iterate1 ->
                    Just <| ElmEcsXArrayBaselineIterate1 (ElmEcsXArrayBaseline.initIterate1 properties)

                Iterate2 ->
                    Just <| ElmEcsXArrayBaselineIterate2 (ElmEcsXArrayBaseline.initIterate2 properties)

                Iterate3 ->
                    Just <| ElmEcsXArrayBaselineIterate3 (ElmEcsXArrayBaseline.initIterate3 properties)

                Update1 ->
                    Just <| ElmEcsXArrayBaselineUpdate1 (ElmEcsXArrayBaseline.initUpdate1 properties)

                Update2 ->
                    Just <| ElmEcsXArrayBaselineUpdate2 (ElmEcsXArrayBaseline.initUpdate2 properties)

                Update3 ->
                    Just <| ElmEcsXArrayBaselineUpdate3 (ElmEcsXArrayBaseline.initUpdate3 properties)

        ElmEcsXDictBaseline ->
            case properties.type_ of
                Iterate1 ->
                    Just <| ElmEcsXDictBaselineIterate1 (ElmEcsXDictBaseline.initIterate1 properties)

                Iterate2 ->
                    Just <| ElmEcsXDictBaselineIterate2 (ElmEcsXDictBaseline.initIterate2 properties)

                Iterate3 ->
                    Just <| ElmEcsXDictBaselineIterate3 (ElmEcsXDictBaseline.initIterate3 properties)

                Update1 ->
                    Just <| ElmEcsXDictBaselineUpdate1 (ElmEcsXDictBaseline.initUpdate1 properties)

                Update2 ->
                    Just <| ElmEcsXDictBaselineUpdate2 (ElmEcsXDictBaseline.initUpdate2 properties)

                Update3 ->
                    Just <| ElmEcsXDictBaselineUpdate3 (ElmEcsXDictBaseline.initUpdate3 properties)

        JsEcsy ->
            Just ExternalModel

        JsHyperrEcs ->
            Just ExternalModel


updateEcs : BenchmarkProperties -> EcsModel -> EcsModel
updateEcs _ ecsModel =
    case ecsModel of
        ElmEcsXIterate1 world ->
            ElmEcsXIterate1 (ElmEcsX.updateIterate1 world)

        ElmEcsXIterate2 world ->
            ElmEcsXIterate2 (ElmEcsX.updateIterate2 world)

        ElmEcsXIterate3 world ->
            ElmEcsXIterate3 (ElmEcsX.updateIterate3 world)

        ElmEcsXUpdate1 world ->
            ElmEcsXUpdate1 (ElmEcsX.updateUpdate1 world)

        ElmEcsXUpdate2 world ->
            ElmEcsXUpdate2 (ElmEcsX.updateUpdate2 world)

        ElmEcsXUpdate3 world ->
            ElmEcsXUpdate3 (ElmEcsX.updateUpdate3 world)

        ElmEcsIterate1 world ->
            ElmEcsIterate1 (ElmEcs.updateIterate1 world)

        ElmEcsIterate2 world ->
            ElmEcsIterate2 (ElmEcs.updateIterate2 world)

        ElmEcsIterate3 world ->
            ElmEcsIterate3 (ElmEcs.updateIterate3 world)

        ElmEcsUpdate1 world ->
            ElmEcsUpdate1 (ElmEcs.updateUpdate1 world)

        ElmEcsUpdate2 world ->
            ElmEcsUpdate2 (ElmEcs.updateUpdate2 world)

        ElmEcsUpdate3 world ->
            ElmEcsUpdate3 (ElmEcs.updateUpdate3 world)

        ElmGameLogicIterate1 world ->
            ElmGameLogicIterate1 (ElmGameLogic.updateIterate1 world)

        ElmGameLogicIterate2 world ->
            ElmGameLogicIterate2 (ElmGameLogic.updateIterate2 world)

        ElmGameLogicIterate3 world ->
            ElmGameLogicIterate3 (ElmGameLogic.updateIterate3 world)

        ElmGameLogicUpdate1 world ->
            ElmGameLogicUpdate1 (ElmGameLogic.updateUpdate1 world)

        ElmGameLogicUpdate2 world ->
            ElmGameLogicUpdate2 (ElmGameLogic.updateUpdate2 world)

        ElmGameLogicUpdate3 world ->
            ElmGameLogicUpdate3 (ElmGameLogic.updateUpdate3 world)

        ElmEcsXArrayBaselineIterate1 world ->
            ElmEcsXArrayBaselineIterate1 (ElmEcsXArrayBaseline.updateIterate1 world)

        ElmEcsXArrayBaselineIterate2 world ->
            ElmEcsXArrayBaselineIterate2 (ElmEcsXArrayBaseline.updateIterate2 world)

        ElmEcsXArrayBaselineIterate3 world ->
            ElmEcsXArrayBaselineIterate3 (ElmEcsXArrayBaseline.updateIterate3 world)

        ElmEcsXArrayBaselineUpdate1 world ->
            ElmEcsXArrayBaselineUpdate1 (ElmEcsXArrayBaseline.updateUpdate1 world)

        ElmEcsXArrayBaselineUpdate2 world ->
            ElmEcsXArrayBaselineUpdate2 (ElmEcsXArrayBaseline.updateUpdate2 world)

        ElmEcsXArrayBaselineUpdate3 world ->
            ElmEcsXArrayBaselineUpdate3 (ElmEcsXArrayBaseline.updateUpdate3 world)

        ElmEcsXDictBaselineIterate1 world ->
            ElmEcsXDictBaselineIterate1 (ElmEcsXDictBaseline.updateIterate1 world)

        ElmEcsXDictBaselineIterate2 world ->
            ElmEcsXDictBaselineIterate2 (ElmEcsXDictBaseline.updateIterate2 world)

        ElmEcsXDictBaselineIterate3 world ->
            ElmEcsXDictBaselineIterate3 (ElmEcsXDictBaseline.updateIterate3 world)

        ElmEcsXDictBaselineUpdate1 world ->
            ElmEcsXDictBaselineUpdate1 (ElmEcsXDictBaseline.updateUpdate1 world)

        ElmEcsXDictBaselineUpdate2 world ->
            ElmEcsXDictBaselineUpdate2 (ElmEcsXDictBaseline.updateUpdate2 world)

        ElmEcsXDictBaselineUpdate3 world ->
            ElmEcsXDictBaselineUpdate3 (ElmEcsXDictBaseline.updateUpdate3 world)

        ExternalModel ->
            ExternalModel


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
        , case model.error of
            Just (ErrorMessage message) ->
                Html.h3 [ Html.Attributes.style "color" "#f00" ] [ Html.text message ]

            Nothing ->
                Html.text ""
        , Html.dl []
            [ Html.dt [] [ Html.text (benchmarkTypeToString model.selectedBenchmark) ]
            , Html.dd [] [ Html.text (benchmarkTypeDescription model.selectedBenchmark) ]
            , Html.dt [] [ Html.text (ecsFrameworkToString model.selectedFramework) ]
            , Html.dd [] [ ecsFrameworkDescription model.selectedFramework ]
            , Html.dt [] [ Html.text (updateTypeToString model.selectedUpdateType) ]
            , Html.dd [] [ updateTypeDescription model.selectedUpdateType ]
            ]
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
                    , Html.p [] [ Html.text "Note: Memory profile is only available in Chrome. You need to run Chrome with the `--enable-precise-memory-info` argument." ]
                    , Html.h3 [] [ Html.text "total heap size (bytes)" ]
                    , LineChart.viewCustom (totalHeapSizeChartConfig hinted) (List.map2 toSeries enabledResults colors)
                    ]
        , Html.p [] [ Html.a [ Html.Attributes.href "https://github.com/harmboschloo/elm-ecs-benchmarks" ] [ Html.text "code" ] ]
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
