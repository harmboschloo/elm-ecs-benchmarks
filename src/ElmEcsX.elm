module ElmEcsX exposing
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

import Array
import Dict exposing (Dict)
import ElmEcsX.Ecs3 as Ecs3



-- BENCHMARK Iterate1 --


initIterate1 : { benchmark | entityCount : Int } -> World3
initIterate1 =
    initWorld3


updateIterate1 : World3 -> World3
updateIterate1 world =
    Ecs3.foldlA applyIterate1 world world


applyIterate1 : Int -> { a : Ecs3.ComponentA } -> World3 -> World3
applyIterate1 _ _ world =
    world



-- BENCHMARK Iterate2 --


initIterate2 : { benchmark | entityCount : Int } -> World3
initIterate2 =
    initWorld3


updateIterate2 : World3 -> World3
updateIterate2 world =
    Ecs3.foldlAb applyIterate2 world world


applyIterate2 : Int -> { a : Ecs3.ComponentA, b : Ecs3.ComponentB } -> World3 -> World3
applyIterate2 _ _ world =
    world



-- BENCHMARK Iterate3 --


initIterate3 : { benchmark | entityCount : Int } -> World3
initIterate3 =
    initWorld3


updateIterate3 : World3 -> World3
updateIterate3 world =
    Ecs3.foldlAbc applyIterate3 world world


applyIterate3 : Int -> { a : Ecs3.ComponentA, b : Ecs3.ComponentB, c : Ecs3.ComponentC } -> World3 -> World3
applyIterate3 _ _ world =
    world



-- BENCHMARK Update1 --


initUpdate1 : { benchmark | entityCount : Int } -> World3
initUpdate1 =
    initWorld3


updateUpdate1 : World3 -> World3
updateUpdate1 world =
    Ecs3.updateA applyUpdate1 world


applyUpdate1 : Int -> { a : Ecs3.ComponentA } -> { a : Ecs3.ComponentA }
applyUpdate1 _ entity =
    { a = { a1 = entity.a.a1 + 1 } }



-- BENCHMARK Update2 --


initUpdate2 : { benchmark | entityCount : Int } -> World3
initUpdate2 =
    initWorld3


updateUpdate2 : World3 -> World3
updateUpdate2 world =
    Ecs3.updateAb applyUpdate2 world


applyUpdate2 :
    Int
    -> { a : Ecs3.ComponentA, b : Ecs3.ComponentB }
    -> { a : Ecs3.ComponentA, b : Ecs3.ComponentB }
applyUpdate2 _ entity =
    { a = { a1 = entity.a.a1 + 1 }
    , b = { b1 = entity.b.b1 + 1, b2 = entity.b.b2 - 1 }
    }



-- BENCHMARK Update3 --


initUpdate3 : { benchmark | entityCount : Int } -> World3
initUpdate3 =
    initWorld3


updateUpdate3 : World3 -> World3
updateUpdate3 world =
    Ecs3.updateAbc applyUpdate3 world


applyUpdate3 :
    Int
    -> { a : Ecs3.ComponentA, b : Ecs3.ComponentB, c : Ecs3.ComponentC }
    -> { a : Ecs3.ComponentA, b : Ecs3.ComponentB, c : Ecs3.ComponentC }
applyUpdate3 _ entity =
    { a = { a1 = entity.a.a1 + 1 }
    , b = { b1 = entity.b.b1 + 1, b2 = entity.b.b2 - 1 }
    , c = { c1 = entity.c.c1 + 1, c2 = entity.c.c2 - 1, c3 = not entity.c.c3 }
    }



-- HELPERS --


type alias World3 =
    Ecs3.World


initWorld3 : { benchmark | entityCount : Int } -> World3
initWorld3 { entityCount } =
    List.range 1 entityCount
        |> List.foldl
            (\count world ->
                Ecs3.addAbcEntity
                    { a = { a1 = count }
                    , b = { b1 = count, b2 = 1 }
                    , c = { c1 = count, c2 = 1, c3 = False }
                    }
                    world
                    |> Tuple.second
            )
            Ecs3.empty
