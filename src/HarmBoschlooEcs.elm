module HarmBoschlooEcs exposing
    ( Components3
    , initIterate1
    , initIterate2
    , initIterate3
    , updateIterate1
    , updateIterate2
    , updateIterate3
    )

import Ecs
import Ecs.Components3
import Ecs.EntityComponents



-- BENCHMARK IterationWithoutUpdate --


initIterate1 : { benchmark | entityCount : Int } -> Ecs.World Int Components3 ()
initIterate1 =
    initWorld3


updateIterate1 : Ecs.World Int Components3 () -> Ecs.World Int Components3 ()
updateIterate1 world =
    Ecs.EntityComponents.processFromLeft
        specs.componentA
        applyIterate1
        world


applyIterate1 : Int -> ComponentA -> Ecs.World Int Components3 () -> Ecs.World Int Components3 ()
applyIterate1 _ _ world =
    world


initIterate2 : { benchmark | entityCount : Int } -> Ecs.World Int Components3 ()
initIterate2 =
    initWorld3


updateIterate2 : Ecs.World Int Components3 () -> Ecs.World Int Components3 ()
updateIterate2 world =
    Ecs.EntityComponents.processFromLeft2
        specs.componentA
        specs.componentB
        applyIterate2
        world


applyIterate2 : Int -> ComponentA -> ComponentB -> Ecs.World Int Components3 () -> Ecs.World Int Components3 ()
applyIterate2 _ _ _ world =
    world


initIterate3 : { benchmark | entityCount : Int } -> Ecs.World Int Components3 ()
initIterate3 =
    initWorld3


updateIterate3 : Ecs.World Int Components3 () -> Ecs.World Int Components3 ()
updateIterate3 world =
    Ecs.EntityComponents.processFromLeft3
        specs.componentA
        specs.componentB
        specs.componentC
        applyIterate3
        world


applyIterate3 : Int -> ComponentA -> ComponentB -> ComponentC -> Ecs.World Int Components3 () -> Ecs.World Int Components3 ()
applyIterate3 _ _ _ _ world =
    world


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
