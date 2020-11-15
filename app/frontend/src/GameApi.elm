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
   { soldierId: Int
   , hp: Int
   , movement: Int
   , attack: Int
   , range: Int
   , faction: PlayerId
   }

jsonDecSoldierUnit : Json.Decode.Decoder ( SoldierUnit )
jsonDecSoldierUnit =
   Json.Decode.succeed (\psoldierId php pmovement pattack prange pfaction -> {soldierId = psoldierId, hp = php, movement = pmovement, attack = pattack, range = prange, faction = pfaction})
   |> required "soldierId" (Json.Decode.int)
   |> required "hp" (Json.Decode.int)
   |> required "movement" (Json.Decode.int)
   |> required "attack" (Json.Decode.int)
   |> required "range" (Json.Decode.int)
   |> required "faction" (jsonDecPlayerId)

jsonEncSoldierUnit : SoldierUnit -> Value
jsonEncSoldierUnit  val =
   Json.Encode.object
   [ ("soldierId", Json.Encode.int val.soldierId)
   , ("hp", Json.Encode.int val.hp)
   , ("movement", Json.Encode.int val.movement)
   , ("attack", Json.Encode.int val.attack)
   , ("range", Json.Encode.int val.range)
   , ("faction", jsonEncPlayerId val.faction)
   ]



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
   Json.Decode.succeed (\pinputType porigin pdestination -> {inputType = pinputType, origin = porigin, destination = pdestination})
   |> required "inputType" (jsonDecPlayerInputType)
   |> required "origin" (jsonDecPosition)
   |> required "destination" (jsonDecPosition)

jsonEncPlayerInput : PlayerInput -> Value
jsonEncPlayerInput  val =
   Json.Encode.object
   [ ("inputType", jsonEncPlayerInputType val.inputType)
   , ("origin", jsonEncPosition val.origin)
   , ("destination", jsonEncPosition val.destination)
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



type alias Borders  = (List (Position, (List Position)))

jsonDecBorders : Json.Decode.Decoder ( Borders )
jsonDecBorders =
    Json.Decode.list (Json.Decode.map2 tuple2 (Json.Decode.index 0 (jsonDecPosition)) (Json.Decode.index 1 (Json.Decode.list (jsonDecPosition))))

jsonEncBorders : Borders -> Value
jsonEncBorders  val = (Json.Encode.list (\(t1,t2) -> Json.Encode.list identity [(jsonEncPosition) t1,((Json.Encode.list jsonEncPosition)) t2])) val



type PossibleInput  =
    PossibleInput PlayerInputType (List Position)

jsonDecPossibleInput : Json.Decode.Decoder ( PossibleInput )
jsonDecPossibleInput =
    Json.Decode.lazy (\_ -> Json.Decode.map2 PossibleInput (Json.Decode.index 0 (jsonDecPlayerInputType)) (Json.Decode.index 1 (Json.Decode.list (jsonDecPosition))))


jsonEncPossibleInput : PossibleInput -> Value
jsonEncPossibleInput (PossibleInput v1 v2) =
    Json.Encode.list identity [jsonEncPlayerInputType v1, (Json.Encode.list jsonEncPosition) v2]


getUnitpositions : (Result Http.Error  (UnitPositions)  -> msg) -> Cmd msg
getUnitpositions toMsg =
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
                    [ "unitpositions"
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

postPlayeractionByPlayerId : PlayerId -> PlayerInput -> (Result Http.Error  (())  -> msg) -> Cmd msg
postPlayeractionByPlayerId capture_playerId body toMsg =
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
                    [ "playeraction"
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

getPossibleactionsByPlayerIdByXByY : PlayerId -> Int -> Int -> (Result Http.Error  ((List PossibleInput))  -> msg) -> Cmd msg
getPossibleactionsByPlayerIdByXByY capture_playerId capture_x capture_y toMsg =
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
                    [ "possibleactions"
                    , capture_playerId |> String.fromInt
                    , capture_x |> String.fromInt
                    , capture_y |> String.fromInt
                    ]
                    params
            , body =
                Http.emptyBody
            , expect =
                Http.expectJson toMsg (Json.Decode.list (jsonDecPossibleInput))
            , timeout =
                Nothing
            , tracker =
                Nothing
            }

getBorders : (Result Http.Error  (Borders)  -> msg) -> Cmd msg
getBorders toMsg =
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
                    [ "borders"
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
