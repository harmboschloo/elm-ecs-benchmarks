module ElmEcsXDictBaseline exposing
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

import Dict exposing (Dict)



-- BENCHMARK Iterate1 --


initIterate1 : { benchmark | entityCount : Int } -> World3
initIterate1 =
    initWorld3


updateIterate1 : World3 -> World3
updateIterate1 world =
    Dict.foldl applyIterate1 world world


applyIterate1 : Int -> { entity | a : ComponentA } -> World3 -> World3
applyIterate1 _ _ world =
    world



-- BENCHMARK Iterate2 --


initIterate2 : { benchmark | entityCount : Int } -> World3
initIterate2 =
    initWorld3


updateIterate2 : World3 -> World3
updateIterate2 world =
    Dict.foldl applyIterate2 world world


applyIterate2 : Int -> { entity | a : ComponentA, b : ComponentB } -> World3 -> World3
applyIterate2 _ _ world =
    world



-- BENCHMARK Iterate3 --


initIterate3 : { benchmark | entityCount : Int } -> World3
initIterate3 =
    initWorld3


updateIterate3 : World3 -> World3
updateIterate3 world =
    Dict.foldl applyIterate3 world world


applyIterate3 : Int -> { entity | a : ComponentA, b : ComponentB, c : ComponentC } -> World3 -> World3
applyIterate3 _ _ world =
    world



-- BENCHMARK Update1 --


initUpdate1 : { benchmark | entityCount : Int } -> World3
initUpdate1 =
    initWorld3


updateUpdate1 : World3 -> World3
updateUpdate1 world =
    Dict.map applyUpdate1 world


applyUpdate1 : Int -> { entity | a : ComponentA } -> { entity | a : ComponentA }
applyUpdate1 _ entity =
    { entity | a = { a1 = entity.a.a1 + 1 } }



-- BENCHMARK Update2 --


initUpdate2 : { benchmark | entityCount : Int } -> World3
initUpdate2 =
    initWorld3


updateUpdate2 : World3 -> World3
updateUpdate2 world =
    Dict.map applyUpdate2 world


applyUpdate2 : Int -> { entity | a : ComponentA, b : ComponentB } -> { entity | a : ComponentA, b : ComponentB }
applyUpdate2 _ entity =
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
    Dict.map applyUpdate3 world


applyUpdate3 :
    Int
    -> { entity | a : ComponentA, b : ComponentB, c : ComponentC }
    -> { entity | a : ComponentA, b : ComponentB, c : ComponentC }
applyUpdate3 _ entity =
    { entity
        | a = { a1 = entity.a.a1 + 1 }
        , b = { b1 = entity.b.b1 + 1, b2 = entity.b.b2 - 1 }
        , c = { c1 = entity.c.c1 + 1, c2 = entity.c.c2 - 1, c3 = not entity.c.c3 }
    }



-- HELPERS --


type alias World3 =
    Dict Int Entity3


type alias Entity3 =
    { a : ComponentA
    , b : ComponentB
    , c : ComponentC
    }


initWorld3 : { benchmark | entityCount : Int } -> World3
initWorld3 { entityCount } =
    List.range 1 entityCount
        |> List.map
            (\id ->
                ( id
                , { a = { a1 = 1 }
                  , b = { b1 = 1, b2 = 2 }
                  , c = { c1 = 1, c2 = 2, c3 = False }
                  }
                )
            )
        |> Dict.fromList



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
