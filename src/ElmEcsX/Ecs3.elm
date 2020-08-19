module ElmEcsX.Ecs3 exposing
    ( AbEntity
    , AbcEntity
    , ComponentA
    , ComponentB
    , ComponentC
    , EntityId
    , EntityType(..)
    , World
    , addAbEntity
    , addAbcEntity
    , empty
    , foldlA
    , foldlAb
    , foldlAbc
    , updateA
    , updateAb
    , updateAbc
    )

import Array exposing (Array)
import Dict exposing (Dict)


type World
    = World Model


type alias Model =
    { entityTypes : Array EntityType
    , abEntities : Dict Int AbEntity
    , abcEntities : Dict Int AbcEntity
    }


type EntityType
    = AbType
    | AbcType


type EntityId
    = EntityId Int


type alias AbEntity =
    { a : ComponentA
    , b : ComponentB
    }


type alias AbcEntity =
    { a : ComponentA
    , b : ComponentB
    , c : ComponentC
    }


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


empty : World
empty =
    World
        { entityTypes = Array.empty
        , abEntities = Dict.empty
        , abcEntities = Dict.empty
        }


addAbEntity : AbEntity -> World -> ( EntityId, World )
addAbEntity entity (World world) =
    let
        id =
            Array.length world.entityTypes
    in
    ( EntityId id
    , World
        { entityTypes = Array.push AbType world.entityTypes
        , abEntities = Dict.insert id entity world.abEntities
        , abcEntities = world.abcEntities
        }
    )


addAbcEntity : AbcEntity -> World -> ( EntityId, World )
addAbcEntity entity (World world) =
    let
        entityId =
            Array.length world.entityTypes
    in
    ( EntityId entityId
    , World
        { entityTypes = Array.push AbcType world.entityTypes
        , abEntities = world.abEntities
        , abcEntities = Dict.insert entityId entity world.abcEntities
        }
    )


getComponentA : EntityId -> World -> Maybe ComponentA
getComponentA (EntityId entityId) (World world) =
    case Array.get entityId world.entityTypes of
        Just entityType ->
            case entityType of
                AbType ->
                    case Dict.get entityId world.abEntities of
                        Just entity ->
                            Just entity.a

                        Nothing ->
                            Nothing

                AbcType ->
                    case Dict.get entityId world.abcEntities of
                        Just entity ->
                            Just entity.a

                        Nothing ->
                            Nothing

        Nothing ->
            Nothing


foldlA : (Int -> { a : ComponentA } -> acc -> acc) -> acc -> World -> acc
foldlA fn acc0 (World world) =
    let
        wrappedFn entityId entity =
            fn entityId { a = entity.a }

        acc1 =
            Dict.foldl wrappedFn acc0 world.abEntities

        acc2 =
            Dict.foldl wrappedFn acc1 world.abcEntities
    in
    acc2


foldlAb : (Int -> { a : ComponentA, b : ComponentB } -> acc -> acc) -> acc -> World -> acc
foldlAb fn acc0 (World world) =
    let
        wrappedFn entityId entity =
            fn entityId { a = entity.a, b = entity.b }

        acc1 =
            Dict.foldl wrappedFn acc0 world.abEntities

        acc2 =
            Dict.foldl wrappedFn acc1 world.abcEntities
    in
    acc2


foldlAbc : (Int -> { a : ComponentA, b : ComponentB, c : ComponentC } -> acc -> acc) -> acc -> World -> acc
foldlAbc fn acc0 (World world) =
    let
        wrappedFn entityId entity =
            fn entityId { a = entity.a, b = entity.b, c = entity.c }

        acc1 =
            Dict.foldl wrappedFn acc0 world.abcEntities
    in
    acc1


updateA : (Int -> { a : ComponentA } -> { a : ComponentA }) -> World -> World
updateA fn (World world) =
    let
        wrappedFn entityId entity0 =
            let
                entity1 =
                    fn entityId { a = entity0.a }
            in
            { entity0 | a = entity1.a }
    in
    World
        { entityTypes = world.entityTypes
        , abEntities = Dict.map wrappedFn world.abEntities
        , abcEntities = Dict.map wrappedFn world.abcEntities
        }


updateAb : (Int -> { a : ComponentA, b : ComponentB } -> { a : ComponentA, b : ComponentB }) -> World -> World
updateAb fn (World world) =
    let
        wrappedFn entityId entity0 =
            let
                entity1 =
                    fn entityId { a = entity0.a, b = entity0.b }
            in
            { entity0 | a = entity1.a, b = entity1.b }
    in
    World
        { entityTypes = world.entityTypes
        , abEntities = Dict.map wrappedFn world.abEntities
        , abcEntities = Dict.map wrappedFn world.abcEntities
        }


updateAbc :
    (Int
     -> { a : ComponentA, b : ComponentB, c : ComponentC }
     -> { a : ComponentA, b : ComponentB, c : ComponentC }
    )
    -> World
    -> World
updateAbc fn (World world) =
    let
        wrappedFn entityId entity0 =
            let
                entity1 =
                    fn entityId { a = entity0.a, b = entity0.b, c = entity0.c }
            in
            { entity0 | a = entity1.a, b = entity1.b, c = entity1.c }
    in
    World
        { entityTypes = world.entityTypes
        , abEntities = world.abEntities
        , abcEntities = Dict.map wrappedFn world.abcEntities
        }
