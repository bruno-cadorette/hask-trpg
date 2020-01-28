module GameApi exposing(..)

import Json.Decode
import Json.Encode exposing (Value)
-- The following module comes from bartavelle/json-helpers
import Json.Helpers exposing (..)
import Dict exposing (Dict)
import Set
import Http
import String
import Url.Builder

maybeBoolToIntStr : Maybe Bool -> String
maybeBoolToIntStr mx =
  case mx of
    Nothing -> ""
    Just True -> "1"
    Just False -> "0"

type alias GameId  = Int

jsonDecGameId : Json.Decode.Decoder ( GameId )
jsonDecGameId =
    Json.Decode.int

jsonEncGameId : GameId -> Value
jsonEncGameId  val = Json.Encode.int val



type alias Position  = (Int, Int)

jsonDecPosition : Json.Decode.Decoder ( Position )
jsonDecPosition =
    Json.Decode.map2 tuple2 (Json.Decode.index 0 (Json.Decode.int)) (Json.Decode.index 1 (Json.Decode.int))

jsonEncPosition : Position -> Value
jsonEncPosition  val = (\(t1,t2) -> Json.Encode.list identity [(Json.Encode.int) t1,(Json.Encode.int) t2]) val



type alias SoldierUnit  =
   { hp: Int
   , movement: Int
   , attack: Int
   , range: Int
   , faction: PlayerId
   }

jsonDecSoldierUnit : Json.Decode.Decoder ( SoldierUnit )
jsonDecSoldierUnit =
   Json.Decode.succeed (\p_hp p_movement p_attack p_range p_faction -> {hp = p_hp, movement = p_movement, attack = p_attack, range = p_range, faction = p_faction})
   |> required "_hp" (Json.Decode.int)
   |> required "_movement" (Json.Decode.int)
   |> required "_attack" (Json.Decode.int)
   |> required "_range" (Json.Decode.int)
   |> required "_faction" (jsonDecPlayerId)

jsonEncSoldierUnit : SoldierUnit -> Value
jsonEncSoldierUnit  val =
   Json.Encode.object
   [ ("_hp", Json.Encode.int val.hp)
   , ("_movement", Json.Encode.int val.movement)
   , ("_attack", Json.Encode.int val.attack)
   , ("_range", Json.Encode.int val.range)
   , ("_faction", jsonEncPlayerId val.faction)
   ]



type alias KeyNotFoundError k = k


jsonEncKeyNotFoundError : (k -> Value) -> KeyNotFoundError k -> Value
jsonEncKeyNotFoundError localEncoder_k val = localEncoder_k val



type alias PlayerId  = Int

jsonDecPlayerId : Json.Decode.Decoder ( PlayerId )
jsonDecPlayerId =
    Json.Decode.int

jsonEncPlayerId : PlayerId -> Value
jsonEncPlayerId  val = Json.Encode.int val



type alias PlayerInput  =
   { inputType: PlayerInputType
   , origin: Position
   , destination: Position
   }

jsonDecPlayerInput : Json.Decode.Decoder ( PlayerInput )
jsonDecPlayerInput =
   Json.Decode.succeed (\p_inputType p_origin p_destination -> {inputType = p_inputType, origin = p_origin, destination = p_destination})
   |> required "_inputType" (jsonDecPlayerInputType)
   |> required "_origin" (jsonDecPosition)
   |> required "_destination" (jsonDecPosition)

jsonEncPlayerInput : PlayerInput -> Value
jsonEncPlayerInput  val =
   Json.Encode.object
   [ ("_inputType", jsonEncPlayerInputType val.inputType)
   , ("_origin", jsonEncPosition val.origin)
   , ("_destination", jsonEncPosition val.destination)
   ]



type PlayerInputType  =
    Movement 
    | Attack 

jsonDecPlayerInputType : Json.Decode.Decoder ( PlayerInputType )
jsonDecPlayerInputType = 
    let jsonDecDictPlayerInputType = Dict.fromList [("Movement", Movement), ("Attack", Attack)]
    in  decodeSumUnaries "PlayerInputType" jsonDecDictPlayerInputType

jsonEncPlayerInputType : PlayerInputType -> Value
jsonEncPlayerInputType  val =
    case val of
        Movement -> Json.Encode.string "Movement"
        Attack -> Json.Encode.string "Attack"



type PlayerMoveInputError  =
    NotPlayerOwned Position
    | RegionOccupied Position
    | RegionNotOccupied Position
    | AttackAllies Position Position
    | AttackTooFar Position Position
    | MoveTooMuch Position

jsonDecPlayerMoveInputError : Json.Decode.Decoder ( PlayerMoveInputError )
jsonDecPlayerMoveInputError =
    let jsonDecDictPlayerMoveInputError = Dict.fromList
            [ ("NotPlayerOwned", Json.Decode.lazy (\_ -> Json.Decode.map NotPlayerOwned (jsonDecPosition)))
            , ("RegionOccupied", Json.Decode.lazy (\_ -> Json.Decode.map RegionOccupied (jsonDecPosition)))
            , ("RegionNotOccupied", Json.Decode.lazy (\_ -> Json.Decode.map RegionNotOccupied (jsonDecPosition)))
            , ("AttackAllies", Json.Decode.lazy (\_ -> Json.Decode.map2 AttackAllies (Json.Decode.index 0 (jsonDecPosition)) (Json.Decode.index 1 (jsonDecPosition))))
            , ("AttackTooFar", Json.Decode.lazy (\_ -> Json.Decode.map2 AttackTooFar (Json.Decode.index 0 (jsonDecPosition)) (Json.Decode.index 1 (jsonDecPosition))))
            , ("MoveTooMuch", Json.Decode.lazy (\_ -> Json.Decode.map MoveTooMuch (jsonDecPosition)))
            ]
    in  decodeSumObjectWithSingleField  "PlayerMoveInputError" jsonDecDictPlayerMoveInputError

jsonEncPlayerMoveInputError : PlayerMoveInputError -> Value
jsonEncPlayerMoveInputError  val =
    let keyval v = case v of
                    NotPlayerOwned v1 -> ("NotPlayerOwned", encodeValue (jsonEncPosition v1))
                    RegionOccupied v1 -> ("RegionOccupied", encodeValue (jsonEncPosition v1))
                    RegionNotOccupied v1 -> ("RegionNotOccupied", encodeValue (jsonEncPosition v1))
                    AttackAllies v1 v2 -> ("AttackAllies", encodeValue (Json.Encode.list identity [jsonEncPosition v1, jsonEncPosition v2]))
                    AttackTooFar v1 v2 -> ("AttackTooFar", encodeValue (Json.Encode.list identity [jsonEncPosition v1, jsonEncPosition v2]))
                    MoveTooMuch v1 -> ("MoveTooMuch", encodeValue (jsonEncPosition v1))
    in encodeSumObjectWithSingleField keyval val



type alias UnitPositions  = (List (Position, SoldierUnit))

jsonDecUnitPositions : Json.Decode.Decoder ( UnitPositions )
jsonDecUnitPositions =
    Json.Decode.list (Json.Decode.map2 tuple2 (Json.Decode.index 0 (jsonDecPosition)) (Json.Decode.index 1 (jsonDecSoldierUnit)))

jsonEncUnitPositions : UnitPositions -> Value
jsonEncUnitPositions  val = (Json.Encode.list (\(t1,t2) -> Json.Encode.list identity [(jsonEncPosition) t1,(jsonEncSoldierUnit) t2])) val



type TerrainType  =
    Grass 
    | Water 
    | Wall 

jsonDecTerrainType : Json.Decode.Decoder ( TerrainType )
jsonDecTerrainType = 
    let jsonDecDictTerrainType = Dict.fromList [("Grass", Grass), ("Water", Water), ("Wall", Wall)]
    in  decodeSumUnaries "TerrainType" jsonDecDictTerrainType

jsonEncTerrainType : TerrainType -> Value
jsonEncTerrainType  val =
    case val of
        Grass -> Json.Encode.string "Grass"
        Water -> Json.Encode.string "Water"
        Wall -> Json.Encode.string "Wall"



type alias Borders  = (List (Position, TerrainType))

jsonDecBorders : Json.Decode.Decoder ( Borders )
jsonDecBorders =
    Json.Decode.list (Json.Decode.map2 tuple2 (Json.Decode.index 0 (jsonDecPosition)) (Json.Decode.index 1 (jsonDecTerrainType)))

jsonEncBorders : Borders -> Value
jsonEncBorders  val = (Json.Encode.list (\(t1,t2) -> Json.Encode.list identity [(jsonEncPosition) t1,(jsonEncTerrainType) t2])) val


getGame : (Result Http.Error  ((List GameId))  -> msg) -> Cmd msg
getGame toMsg =
    let
        params =
            List.filterMap identity
            (List.concat
                [])
    in
        Http.request
            { method =
                "GET"
            , headers =
                []
            , url =
                Url.Builder.crossOrigin ""
                    [ "game"
                    ]
                    params
            , body =
                Http.emptyBody
            , expect =
                Http.expectJson toMsg (Json.Decode.list (jsonDecGameId))
            , timeout =
                Nothing
            , tracker =
                Nothing
            }

getGameByGameIdGameState : GameId -> (Result Http.Error  (UnitPositions)  -> msg) -> Cmd msg
getGameByGameIdGameState capture_gameId toMsg =
    let
        params =
            List.filterMap identity
            (List.concat
                [])
    in
        Http.request
            { method =
                "GET"
            , headers =
                []
            , url =
                Url.Builder.crossOrigin ""
                    [ "game"
                    , capture_gameId |> String.fromInt
                    , "gameState"
                    ]
                    params
            , body =
                Http.emptyBody
            , expect =
                Http.expectJson toMsg jsonDecUnitPositions
            , timeout =
                Nothing
            , tracker =
                Nothing
            }

postGameByGameIdGameStateByPlayerId : GameId -> PlayerId -> PlayerInput -> (Result Http.Error  (())  -> msg) -> Cmd msg
postGameByGameIdGameStateByPlayerId capture_gameId capture_playerId body toMsg =
    let
        params =
            List.filterMap identity
            (List.concat
                [])
    in
        Http.request
            { method =
                "POST"
            , headers =
                []
            , url =
                Url.Builder.crossOrigin ""
                    [ "game"
                    , capture_gameId |> String.fromInt
                    , "gameState"
                    , capture_playerId |> String.fromInt
                    ]
                    params
            , body =
                Http.jsonBody (jsonEncPlayerInput body)
            , expect =
                Http.expectString 
                     (\x -> case x of
                     Err e -> toMsg (Err e)
                     Ok _ -> toMsg (Ok ()))
            , timeout =
                Nothing
            , tracker =
                Nothing
            }

getGameByGameIdBorders : GameId -> (Result Http.Error  (Borders)  -> msg) -> Cmd msg
getGameByGameIdBorders capture_gameId toMsg =
    let
        params =
            List.filterMap identity
            (List.concat
                [])
    in
        Http.request
            { method =
                "GET"
            , headers =
                []
            , url =
                Url.Builder.crossOrigin ""
                    [ "game"
                    , capture_gameId |> String.fromInt
                    , "borders"
                    ]
                    params
            , body =
                Http.emptyBody
            , expect =
                Http.expectJson toMsg jsonDecBorders
            , timeout =
                Nothing
            , tracker =
                Nothing
            }
