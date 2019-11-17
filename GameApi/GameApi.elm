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



type alias RegionId  = (Int, Int)

jsonDecRegionId : Json.Decode.Decoder ( RegionId )
jsonDecRegionId =
    Json.Decode.map2 tuple2 (Json.Decode.index 0 (Json.Decode.int)) (Json.Decode.index 1 (Json.Decode.int))

jsonEncRegionId : RegionId -> Value
jsonEncRegionId  val = (\(t1,t2) -> Json.Encode.list identity [(Json.Encode.int) t1,(Json.Encode.int) t2]) val



type alias SoldierUnit  =
   { _hp: Int
   , _movement: Int
   , _attack: Int
   , _range: Int
   , _faction: PlayerId
   }

jsonDecSoldierUnit : Json.Decode.Decoder ( SoldierUnit )
jsonDecSoldierUnit =
   Json.Decode.succeed (\p_hp p_movement p_attack p_range p_faction -> {_hp = p_hp, _movement = p_movement, _attack = p_attack, _range = p_range, _faction = p_faction})
   |> required "_hp" (Json.Decode.int)
   |> required "_movement" (Json.Decode.int)
   |> required "_attack" (Json.Decode.int)
   |> required "_range" (Json.Decode.int)
   |> required "_faction" (jsonDecPlayerId)

jsonEncSoldierUnit : SoldierUnit -> Value
jsonEncSoldierUnit  val =
   Json.Encode.object
   [ ("_hp", Json.Encode.int val._hp)
   , ("_movement", Json.Encode.int val._movement)
   , ("_attack", Json.Encode.int val._attack)
   , ("_range", Json.Encode.int val._range)
   , ("_faction", jsonEncPlayerId val._faction)
   ]



type alias KeyNotFoundError k = k

jsonDecKeyNotFoundError : Json.Decode.Decoder k -> Json.Decode.Decoder ( KeyNotFoundError k )
jsonDecKeyNotFoundError localDecoder_k =
    localDecoder_k

jsonEncKeyNotFoundError : (k -> Value) -> KeyNotFoundError k -> Value
jsonEncKeyNotFoundError localEncoder_k val = localEncoder_k val



type alias PlayerId  = Int

jsonDecPlayerId : Json.Decode.Decoder ( PlayerId )
jsonDecPlayerId =
    Json.Decode.int

jsonEncPlayerId : PlayerId -> Value
jsonEncPlayerId  val = Json.Encode.int val



type alias PlayerInput  =
   { _inputType: PlayerInputType
   , _origin: RegionId
   , _destination: RegionId
   }

jsonDecPlayerInput : Json.Decode.Decoder ( PlayerInput )
jsonDecPlayerInput =
   Json.Decode.succeed (\p_inputType p_origin p_destination -> {_inputType = p_inputType, _origin = p_origin, _destination = p_destination})
   |> required "_inputType" (jsonDecPlayerInputType)
   |> required "_origin" (jsonDecRegionId)
   |> required "_destination" (jsonDecRegionId)

jsonEncPlayerInput : PlayerInput -> Value
jsonEncPlayerInput  val =
   Json.Encode.object
   [ ("_inputType", jsonEncPlayerInputType val._inputType)
   , ("_origin", jsonEncRegionId val._origin)
   , ("_destination", jsonEncRegionId val._destination)
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
    NotPlayerOwned RegionId
    | RegionOccupied RegionId
    | RegionNotOccupied RegionId
    | AttackAllies RegionId RegionId
    | AttackTooFar RegionId RegionId
    | MoveTooMuch RegionId

jsonDecPlayerMoveInputError : Json.Decode.Decoder ( PlayerMoveInputError )
jsonDecPlayerMoveInputError =
    let jsonDecDictPlayerMoveInputError = Dict.fromList
            [ ("NotPlayerOwned", Json.Decode.lazy (\_ -> Json.Decode.map NotPlayerOwned (jsonDecRegionId)))
            , ("RegionOccupied", Json.Decode.lazy (\_ -> Json.Decode.map RegionOccupied (jsonDecRegionId)))
            , ("RegionNotOccupied", Json.Decode.lazy (\_ -> Json.Decode.map RegionNotOccupied (jsonDecRegionId)))
            , ("AttackAllies", Json.Decode.lazy (\_ -> Json.Decode.map2 AttackAllies (Json.Decode.index 0 (jsonDecRegionId)) (Json.Decode.index 1 (jsonDecRegionId))))
            , ("AttackTooFar", Json.Decode.lazy (\_ -> Json.Decode.map2 AttackTooFar (Json.Decode.index 0 (jsonDecRegionId)) (Json.Decode.index 1 (jsonDecRegionId))))
            , ("MoveTooMuch", Json.Decode.lazy (\_ -> Json.Decode.map MoveTooMuch (jsonDecRegionId)))
            ]
    in  decodeSumObjectWithSingleField  "PlayerMoveInputError" jsonDecDictPlayerMoveInputError

jsonEncPlayerMoveInputError : PlayerMoveInputError -> Value
jsonEncPlayerMoveInputError  val =
    let keyval v = case v of
                    NotPlayerOwned v1 -> ("NotPlayerOwned", encodeValue (jsonEncRegionId v1))
                    RegionOccupied v1 -> ("RegionOccupied", encodeValue (jsonEncRegionId v1))
                    RegionNotOccupied v1 -> ("RegionNotOccupied", encodeValue (jsonEncRegionId v1))
                    AttackAllies v1 v2 -> ("AttackAllies", encodeValue (Json.Encode.list identity [jsonEncRegionId v1, jsonEncRegionId v2]))
                    AttackTooFar v1 v2 -> ("AttackTooFar", encodeValue (Json.Encode.list identity [jsonEncRegionId v1, jsonEncRegionId v2]))
                    MoveTooMuch v1 -> ("MoveTooMuch", encodeValue (jsonEncRegionId v1))
    in encodeSumObjectWithSingleField keyval val



type alias UnitPositions  = (List (RegionId, SoldierUnit))

jsonDecUnitPositions : Json.Decode.Decoder ( UnitPositions )
jsonDecUnitPositions =
    Json.Decode.list (Json.Decode.map2 tuple2 (Json.Decode.index 0 (jsonDecRegionId)) (Json.Decode.index 1 (jsonDecSoldierUnit)))

jsonEncUnitPositions : UnitPositions -> Value
jsonEncUnitPositions  val = (Json.Encode.list (\(t1,t2) -> Json.Encode.list identity [(jsonEncRegionId) t1,(jsonEncSoldierUnit) t2])) val



type alias Borders  = (List (RegionId, (List RegionId)))

jsonDecBorders : Json.Decode.Decoder ( Borders )
jsonDecBorders =
    Json.Decode.list (Json.Decode.map2 tuple2 (Json.Decode.index 0 (jsonDecRegionId)) (Json.Decode.index 1 (Json.Decode.list (jsonDecRegionId))))

jsonEncBorders : Borders -> Value
jsonEncBorders  val = (Json.Encode.list (\(t1,t2) -> Json.Encode.list identity [(jsonEncRegionId) t1,((Json.Encode.list jsonEncRegionId)) t2])) val


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
