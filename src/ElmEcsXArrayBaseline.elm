module ElmEcsXArrayBaseline exposing
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

import Array exposing (Array)



-- BENCHMARK Iterate1 --


initIterate1 : { benchmark | entityCount : Int } -> World3
initIterate1 =
    initWorld3


updateIterate1 : World3 -> World3
updateIterate1 world =
    Array.foldl applyIterate1 world world


applyIterate1 : { entity | a : ComponentA } -> World3 -> World3
applyIterate1 _ world =
    world



-- BENCHMARK Iterate2 --


initIterate2 : { benchmark | entityCount : Int } -> World3
initIterate2 =
    initWorld3


updateIterate2 : World3 -> World3
updateIterate2 world =
    Array.foldl applyIterate2 world world


applyIterate2 : { entity | a : ComponentA, b : ComponentB } -> World3 -> World3
applyIterate2 _ world =
    world



-- BENCHMARK Iterate3 --


initIterate3 : { benchmark | entityCount : Int } -> World3
initIterate3 =
    initWorld3


updateIterate3 : World3 -> World3
updateIterate3 world =
    Array.foldl applyIterate3 world world


applyIterate3 : { entity | a : ComponentA, b : ComponentB, c : ComponentC } -> World3 -> World3
applyIterate3 _ world =
    world



-- BENCHMARK Update1 --


initUpdate1 : { benchmark | entityCount : Int } -> World3
initUpdate1 =
    initWorld3


updateUpdate1 : World3 -> World3
updateUpdate1 world =
    Array.map applyUpdate1 world


applyUpdate1 : { entity | a : ComponentA } -> { entity | a : ComponentA }
applyUpdate1 entity =
    { entity | a = { a1 = entity.a.a1 + 1 } }



-- BENCHMARK Update2 --


initUpdate2 : { benchmark | entityCount : Int } -> World3
initUpdate2 =
    initWorld3


updateUpdate2 : World3 -> World3
updateUpdate2 world =
    Array.map applyUpdate2 world


applyUpdate2 : { entity | a : ComponentA, b : ComponentB } -> { entity | a : ComponentA, b : ComponentB }
applyUpdate2 entity =
    { entity
        | a = { a1 = entity.a.a1 + 1 }
        , b = { b1 = entity.b.b1 + 1, b2 = entity.b.b2 - 1 }
    }



-- BENCHMARK Update3 --


initUpdate3 : { benchmark | entityCount : Int } -> World3
initUpdate3 =
    initWorld3


updateUpdate3 : World3 -> World3
updateUpdate3 world =
    Array.map applyUpdate3 world


applyUpdate3 :
    { entity | a : ComponentA, b : ComponentB, c : ComponentC }
    -> { entity | a : ComponentA, b : ComponentB, c : ComponentC }
applyUpdate3 entity =
    { entity
        | a = { a1 = entity.a.a1 + 1 }
        , b = { b1 = entity.b.b1 + 1, b2 = entity.b.b2 - 1 }
        , c = { c1 = entity.c.c1 + 1, c2 = entity.c.c2 - 1, c3 = not entity.c.c3 }
    }



-- HELPERS --


type alias World3 =
    Array Entity3


type alias Entity3 =
    { a : ComponentA
    , b : ComponentB
    , c : ComponentC
    }


initWorld3 : { benchmark | entityCount : Int } -> World3
initWorld3 { entityCount } =
    Array.repeat entityCount
        { a = { a1 = 1 }
        , b = { b1 = 1, b2 = 2 }
        , c = { c1 = 1, c2 = 2, c3 = False }
        }



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
