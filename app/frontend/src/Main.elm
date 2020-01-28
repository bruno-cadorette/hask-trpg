module Main exposing(..)
import GameApi exposing(..)
import Browser
import Dict
import Maybe
import Tuple exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Platform.Cmd exposing (..)
import List exposing (..)
import Time exposing (..)

--type Action = Move | Attack

--type SelectionState  =
--  NoSelection
--  | SelectUnit Position
--  | SelectAction Position Action

gameId = 1

initalGameState = 
  {selection = SelectNone, units = Dict.empty, playerId = 1, error = "", mapInfo = Dict.empty}

main =
  Browser.element 
    { 
      init = \() -> (initalGameState, Cmd.batch [getMapInfo, getGameState]), 
      update = update, 
      view = view, 
      subscriptions = always (Time.every 500 (\x -> SendReceiveUnitsCommand))
    }

getMapInfo = GameApi.getGameByGameIdBorders gameId (handleError (Dict.fromList >> ReceiveMap))
getGameState = GameApi.getGameByGameIdGameState gameId (handleError (Dict.fromList >> ReceiveUnits))
type Selection a =
  SelectNone | SelectOne a | SelectTwo a a

regionIdToString (a, b) = "(" ++ String.fromInt a ++ ", " ++ String.fromInt b ++ ")"

showSelection selection = 
  case selection of
    SelectNone -> "()"
    SelectOne a -> regionIdToString a
    SelectTwo a b -> (regionIdToString a) ++ " -> " ++ (regionIdToString b)

isSelected x selection = 
  case selection of
    SelectNone -> False
    SelectOne a -> a == x
    SelectTwo a b -> a == x || b == x

addSelection new selection = 
  case selection of
    SelectNone -> SelectOne new
    SelectOne existing -> SelectTwo existing new
    SelectTwo _ _ -> SelectNone

type alias Model = {
    selection : Selection GameApi.Position,
    units : Dict.Dict GameApi.Position GameApi.SoldierUnit,
    error : String
  }

type Msg = 
    Select GameApi.Position
  | ReceiveUnits (Dict.Dict GameApi.Position GameApi.SoldierUnit)
  | ReceiveMap (Dict.Dict GameApi.Position GameApi.TerrainType)
  | SendReceiveUnitsCommand
  | SendCommand
  | ChangePlayerId String
  | Error String
  | Success

handleError f result =
  case result of
    Err err -> Error "invalid move!"
    Ok x -> f x

update msg model =
  case msg of
    Select r ->
      ({model | selection = addSelection r model.selection }, Cmd.none)
    ReceiveUnits r ->
      ({model | units = r}, Cmd.none)
    ReceiveMap b ->
      ( {model | mapInfo = b}, Cmd.none)
    SendReceiveUnitsCommand ->
      (model, getGameState)
    SendCommand ->
      let 
        cmd =
          case model.selection of
            SelectTwo origin destination -> 
              GameApi.postGameByGameIdGameStateByPlayerId 
                gameId 
                model.playerId
                {origin = origin, destination = destination, inputType = GameApi.Movement}
                (handleError (always Success))
            _ -> Cmd.none
      in ({ model | selection = SelectNone }, cmd)
    ChangePlayerId str -> 
      case String.toInt str of
        Just id -> ({model | playerId = id}, Cmd.none)
        Nothing -> (model, Cmd.none)
    Success -> ({ model | error = "" }, Cmd.none)
    Error err -> ({ model | error = err }, Cmd.none)

createTable len f = 
  let 
    lst = List.range 0 len
    row i = List.map (\j -> Html.td [] [f i j]) lst
  in Html.table [] <| List.map (\i -> Html.tr [] <| row i) lst
  
buttonClasses model i j = 
  if isSelected (i, j) model.selection then
    [class "selected"]
  else 
    case Dict.get (i, j) model.units of
      Just soldier -> 
        if soldier.faction == 1 then
          [class "player"]
        else 
          [class "enemy"]
      Nothing ->
        []
terrainClass model i j = 
  case Dict.get (i, j) model.mapInfo of
    Just terrain ->
      case terrain of
        Grass -> class "grass"
        Water -> class "water"
        Wall -> class "wall"
    Nothing -> class ""

individualButton model i j = 
  let hp = Dict.get (i, j) model.units |> Maybe.map (\x -> [text <| String.fromInt x.hp]) |> Maybe.withDefault []
  in button ([onClick <| Select (i, j), class "county"] ++ (buttonClasses model i j) ++ [terrainClass model i j]) hp

getMaxLength model = 
  model.mapInfo 
  |> Dict.keys
  |> List.map first
  |> List.maximum
  |> Maybe.withDefault 0

showSoldier ((x,y), soldier) = 
  ul [] [
    li [] [text <| "position: " ++ "(" ++ String.fromInt x ++ ", " ++ String.fromInt y ++ ")"],
    li [] [text <| "hp: " ++ String.fromInt soldier.hp],
    li [] [text <| "movement: " ++ String.fromInt soldier.movement],
    li [] [text <| "attack: " ++ String.fromInt soldier.attack],
    li [] [text <| "range: " ++ String.fromInt soldier.range],
    li [] [text <| "faction: " ++ String.fromInt soldier.faction]
  ]

view model = div [] [
  createTable (getMaxLength model) (individualButton model),
  text (showSelection model.selection),
  br [] [],
  button [onClick SendCommand] [text "Send command"],
  text (model.error),
  br [] [],
  model.units |> Dict.toList |> List.map showSoldier |> div [],
  input [style "position" "fixed", style "bottom" "0", onInput ChangePlayerId] []
  ]
 