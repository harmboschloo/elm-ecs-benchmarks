module ElmGameLogic exposing
    ( World3
    , initIterate1
    , initIterate2
    , initIterate3
    , initUpdate1
    , initUpdate2
    , initUpdate3
    , updateIterate1
    , updateIterate2
    , updateIterate3
    , updateUpdate1
    , updateUpdate2
    , updateUpdate3
    )

{-| <https://package.elm-lang.org/packages/justgook/elm-game-logic/latest/>
-}

import Logic.Component
import Logic.Entity
import Logic.System



-- BENCHMARK Iterate1 --


initIterate1 : { benchmark | entityCount : Int } -> World3
initIterate1 =
    initWorld3


updateIterate1 : World3 -> World3
updateIterate1 world =
    Logic.System.foldl
        applyIterate1
        world.a
        world


applyIterate1 : ComponentA -> World3 -> World3
applyIterate1 _ world =
    world



-- BENCHMARK Iterate2 --


initIterate2 : { benchmark | entityCount : Int } -> World3
initIterate2 =
    initWorld3


updateIterate2 : World3 -> World3
updateIterate2 world =
    Logic.System.foldl2
        applyIterate2
        world.a
        world.b
        world


applyIterate2 : ComponentA -> ComponentB -> World3 -> World3
applyIterate2 _ _ world =
    world



-- BENCHMARK Iterate3 --


initIterate3 : { benchmark | entityCount : Int } -> World3
initIterate3 =
    initWorld3


updateIterate3 : World3 -> World3
updateIterate3 world =
    Logic.System.foldl3
        applyIterate3
        world.a
        world.b
        world.c
        world


applyIterate3 : ComponentA -> ComponentB -> ComponentC -> World3 -> World3
applyIterate3 _ _ _ world =
    world



-- BENCHMARK Update1 --


initUpdate1 : { benchmark | entityCount : Int } -> World3
initUpdate1 =
    initWorld3


updateUpdate1 : World3 -> World3
updateUpdate1 world =
    Logic.System.step
        applyUpdate1
        aSpecWorld3
        world


applyUpdate1 : ComponentA -> ComponentA
applyUpdate1 a =
    { a1 = a.a1 + 1 }



-- BENCHMARK Update2 --


initUpdate2 : { benchmark | entityCount : Int } -> World3
initUpdate2 =
    initWorld3


updateUpdate2 : World3 -> World3
updateUpdate2 world =
    Logic.System.step2
        applyUpdate2
        aSpecWorld3
        bSpecWorld3
        world


applyUpdate2 :
    ( ComponentA, ComponentA -> World2 -> World2 )
    -> ( ComponentB, ComponentB -> World2 -> World2 )
    -> World2
    -> World2
applyUpdate2 ( a, setA ) ( b, setB ) world =
    world
        |> setA { a1 = a.a1 + 1 }
        |> setB { b1 = b.b1 + 1, b2 = b.b2 - 1 }



-- BENCHMARK Update3 --


initUpdate3 : { benchmark | entityCount : Int } -> World3
initUpdate3 =
    initWorld3


updateUpdate3 : World3 -> World3
updateUpdate3 world =
    Logic.System.step3
        applyUpdate3
        aSpecWorld3
        bSpecWorld3
        cSpecWorld3
        world


applyUpdate3 :
    ( ComponentA, ComponentA -> World3 -> World3 )
    -> ( ComponentB, ComponentB -> World3 -> World3 )
    -> ( ComponentC, ComponentC -> World3 -> World3 )
    -> World3
    -> World3
applyUpdate3 ( a, setA ) ( b, setB ) ( c, setC ) world =
    world
        |> setA { a1 = a.a1 + 1 }
        |> setB { b1 = b.b1 + 1, b2 = b.b2 - 1 }
        |> setC { c1 = c.c1 + 1, c2 = c.c2 - 1, c3 = not c.c3 }



-- HELPERS --


type alias World2 =
    { a : Logic.Component.Set ComponentA
    , b : Logic.Component.Set ComponentB
    }


type alias World3 =
    { a : Logic.Component.Set ComponentA
    , b : Logic.Component.Set ComponentB
    , c : Logic.Component.Set ComponentC
    }


initWorld3 : { benchmark | entityCount : Int } -> World3
initWorld3 { entityCount } =
    List.range 1 entityCount
        |> List.foldl
            (\id world ->
                world
                    |> Logic.Entity.create id
                    |> Logic.Entity.with ( aSpecWorld3, { a1 = 1 } )
                    |> Logic.Entity.with ( bSpecWorld3, { b1 = 1, b2 = 2 } )
                    |> Logic.Entity.with ( cSpecWorld3, { c1 = 1, c2 = 2, c3 = False } )
                    |> Tuple.second
            )
            { a = Logic.Component.empty
            , b = Logic.Component.empty
            , c = Logic.Component.empty
            }


aSpecWorld3 : Logic.Component.Spec ComponentA World3
aSpecWorld3 =
    Logic.Component.Spec .a
        (\a w ->
            { a = a
            , b = w.b
            , c = w.c
            }
        )


bSpecWorld3 : Logic.Component.Spec ComponentB World3
bSpecWorld3 =
    Logic.Component.Spec .b
        (\b w ->
            { a = w.a
            , b = b
            , c = w.c
            }
        )


cSpecWorld3 : Logic.Component.Spec ComponentC World3
cSpecWorld3 =
    Logic.Component.Spec .c
        (\c w ->
            { a = w.a
            , b = w.b
            , c = c
            }
        )



-- COMPONENTS --


type alias ComponentA =
    { a1 : Int
    }


type alias ComponentB =
    { b1 : Int
    , b2 : Float
    }


type alias ComponentC =
    { c1 : Int
    , c2 : Float
    , c3 : Bool
    }
