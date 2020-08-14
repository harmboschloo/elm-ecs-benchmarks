module ElmEcs exposing
    ( Components3
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

import Ecs
import Ecs.Components3
import Ecs.EntityComponents



-- BENCHMARK Iterate1 --


initIterate1 : { benchmark | entityCount : Int } -> Ecs.World Int Components3 ()
initIterate1 =
    initWorld3


updateIterate1 : Ecs.World Int Components3 () -> Ecs.World Int Components3 ()
updateIterate1 world =
    Ecs.EntityComponents.foldFromLeft
        specs.componentA
        applyIterate1
        world
        world


applyIterate1 : Int -> ComponentA -> Ecs.World Int Components3 () -> Ecs.World Int Components3 ()
applyIterate1 _ _ world =
    world



-- BENCHMARK Iterate2 --


initIterate2 : { benchmark | entityCount : Int } -> Ecs.World Int Components3 ()
initIterate2 =
    initWorld3


updateIterate2 : Ecs.World Int Components3 () -> Ecs.World Int Components3 ()
updateIterate2 world =
    Ecs.EntityComponents.foldFromLeft2
        specs.componentA
        specs.componentB
        applyIterate2
        world
        world


applyIterate2 : Int -> ComponentA -> ComponentB -> Ecs.World Int Components3 () -> Ecs.World Int Components3 ()
applyIterate2 _ _ _ world =
    world



-- BENCHMARK Iterate3 --


initIterate3 : { benchmark | entityCount : Int } -> Ecs.World Int Components3 ()
initIterate3 =
    initWorld3


updateIterate3 : Ecs.World Int Components3 () -> Ecs.World Int Components3 ()
updateIterate3 world =
    Ecs.EntityComponents.foldFromLeft3
        specs.componentA
        specs.componentB
        specs.componentC
        applyIterate3
        world
        world


applyIterate3 : Int -> ComponentA -> ComponentB -> ComponentC -> Ecs.World Int Components3 () -> Ecs.World Int Components3 ()
applyIterate3 _ _ _ _ world =
    world



-- BENCHMARK Update1 --


initUpdate1 : { benchmark | entityCount : Int } -> Ecs.World Int Components3 ()
initUpdate1 =
    initWorld3


updateUpdate1 : Ecs.World Int Components3 () -> Ecs.World Int Components3 ()
updateUpdate1 world =
    Ecs.EntityComponents.processFromLeft
        specs.componentA
        applyUpdate1
        world


applyUpdate1 : Int -> ComponentA -> Ecs.World Int Components3 () -> Ecs.World Int Components3 ()
applyUpdate1 _ a world =
    world
        |> Ecs.insertComponent specs.componentA { a1 = a.a1 + 1, a2 = a.a2 - 1 }



-- BENCHMARK Update2 --


initUpdate2 : { benchmark | entityCount : Int } -> Ecs.World Int Components3 ()
initUpdate2 =
    initWorld3


updateUpdate2 : Ecs.World Int Components3 () -> Ecs.World Int Components3 ()
updateUpdate2 world =
    Ecs.EntityComponents.processFromLeft2
        specs.componentA
        specs.componentB
        applyUpdate2
        world


applyUpdate2 : Int -> ComponentA -> ComponentB -> Ecs.World Int Components3 () -> Ecs.World Int Components3 ()
applyUpdate2 _ a b world =
    world
        |> Ecs.insertComponent specs.componentA { a1 = a.a1 + 1, a2 = a.a2 - 1 }
        |> Ecs.insertComponent specs.componentB { b1 = not b.b1, b2 = String.reverse b.b2 }



-- BENCHMARK Update3 --


initUpdate3 : { benchmark | entityCount : Int } -> Ecs.World Int Components3 ()
initUpdate3 =
    initWorld3


updateUpdate3 : Ecs.World Int Components3 () -> Ecs.World Int Components3 ()
updateUpdate3 world =
    Ecs.EntityComponents.processFromLeft3
        specs.componentA
        specs.componentB
        specs.componentC
        applyUpdate3
        world


applyUpdate3 : Int -> ComponentA -> ComponentB -> ComponentC -> Ecs.World Int Components3 () -> Ecs.World Int Components3 ()
applyUpdate3 _ a b c world =
    world
        |> Ecs.insertComponent specs.componentA { a1 = a.a1 + 1, a2 = a.a2 - 1 }
        |> Ecs.insertComponent specs.componentB { b1 = not b.b1, b2 = String.reverse b.b2 }
        |> Ecs.insertComponent specs.componentC { c1 = Char.toUpper c.c1, c2 = List.reverse c.c2 }



-- HELPERS --


initWorld3 : { benchmark | entityCount : Int } -> Ecs.World Int Components3 ()
initWorld3 { entityCount } =
    List.range 1 entityCount
        |> List.foldl
            (\id world ->
                world
                    |> Ecs.insertEntity id
                    |> Ecs.insertComponent specs.componentA { a1 = 1, a2 = 2 }
                    |> Ecs.insertComponent specs.componentB { b1 = True, b2 = "" }
                    |> Ecs.insertComponent specs.componentC { c1 = 'c', c2 = [] }
            )
            (Ecs.emptyWorld specs.components ())



-- COMPONENTS --


type alias Components3 =
    Ecs.Components3.Components3 Int ComponentA ComponentB ComponentC


type alias ComponentA =
    { a1 : Int
    , a2 : Float
    }


type alias ComponentB =
    { b1 : Bool
    , b2 : String
    }


type alias ComponentC =
    { c1 : Char
    , c2 : List Int
    }


type alias Specs =
    { components : Ecs.AllComponentsSpec Int Components3
    , componentA : Ecs.ComponentSpec Int ComponentA Components3
    , componentB : Ecs.ComponentSpec Int ComponentB Components3
    , componentC : Ecs.ComponentSpec Int ComponentC Components3
    }


specs : Specs
specs =
    Specs |> Ecs.Components3.specs
