module Main exposing(..)
import GameApi
import Browser
import Dict
import Maybe
import Task
import Process
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Platform.Cmd exposing (..)
import List exposing (..)
import Time exposing (..)
import Debug

playerId = 1
gameId = 1

main =
  Browser.element 
    { 
      init = \() -> ({selection = SelectNone, units = Dict.empty, playerId = 1, error = "", isMoving = False}, getGameState), 
      update = update, 
      view = view, 
      subscriptions = always (Time.every 500 (\x -> SendReceiveUnitsCommand))
    }

getGameState = GameApi.getGameByGameIdGameState gameId (handleError (Dict.fromList >> ReceiveUnits))
type Selection a =
  SelectNone | SelectOne a | SelectTwo a a

regionIdToString (a, b) = "(" ++ String.fromInt a ++ ", " ++ String.fromInt b ++ ")"

showSelection : Selection (Int, Int) -> String
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
    selection : Selection GameApi.RegionId,
    units : Dict.Dict GameApi.RegionId SoldierModel,
    error : String,
    playerId : Int,
    isMoving : Bool
  }
type Direction =
    Up
  | Down
  | Left
  | Right
type MovementState = 
    Idle
  | Move Direction GameApi.RegionId

type alias SoldierModel = {
    movementState : MovementState,
    soldierData : GameApi.SoldierUnit
  }
type Msg = 
    Select GameApi.RegionId
  | ReceiveUnits (Dict.Dict GameApi.RegionId GameApi.SoldierUnit)
  | MoveSoldier (Maybe GameApi.RegionId)
  | SendReceiveUnitsCommand
  | SendCommand
  | ChangePlayerId String
  | Error String
  | Success
{-
nouveaux objets mais avec les anciennes positions


-}
receiveUnits : 
  Dict.Dict GameApi.RegionId GameApi.SoldierUnit -> 
  Dict.Dict GameApi.RegionId SoldierModel -> 
  Dict.Dict GameApi.RegionId SoldierModel
receiveUnits serverDict localDict =
  let 
    newPositions = 
      serverDict
      |> Dict.toList
      |> List.map(\(k,v) -> (v.soldierId, (k, v)))
    oldPositions = 
      localDict 
      |> Dict.toList
      |> List.map(\(k,v) -> (v.soldierData.soldierId, k))
      |> Dict.fromList

  in 
    newPositions 
    |> List.map (\(soldierId, (newPos, newData)) -> 
      case Dict.get soldierId oldPositions of 
              Just oldPos -> 
                if oldPos /= newPos then
                  (oldPos, {movementState = Move (getDirection oldPos newPos) newPos, soldierData = newData} )
                else
                  (newPos, {movementState = Idle, soldierData = newData})
              Nothing -> (newPos, {movementState = Idle, soldierData = newData}))
    |> Dict.fromList
    
getDirection (oldY, oldX) (newY, newX) = 
  if oldX < newX then
    Right
  else if oldX > newX then
    Left
  else if oldY < newY then
    Down
  else
    Up

immediateNextMove (curY, curX) goal = 
  case getDirection (curY, curX) goal of
     Right -> (curY, curX + 1)
     Left -> (curY, curX - 1)
     Down -> (curY + 1, curX)
     Up -> (curY - 1, curX)

nextMove current goal dict =
  let next = immediateNextMove current goal
  in
    if current == goal then 
      current
    else if Dict.member next dict then
      nextMove next goal dict
    else
      next

find : (a -> Bool) -> List a -> Maybe a
find f list =
  case list of
    (x::xs) -> if f x then Just x else find f xs
    [] -> Nothing

findNextToMove : Dict.Dict GameApi.RegionId SoldierModel -> Maybe.Maybe GameApi.RegionId
findNextToMove dict =
  dict 
  |> Dict.toList 
  |> find (\(currentPos, v) -> 
      case v.movementState of
          Move _ futurePos -> currentPos /= futurePos
          _ -> False)
  |> Maybe.map Tuple.first

moveSoldier currentPos dict =
  case Dict.get currentPos dict of
    Just unit -> 
      case unit.movementState of
        Move _ goal -> 
          let nextPos = nextMove currentPos goal dict
          in
            Dict.remove currentPos dict 
            |> Dict.insert nextPos ({unit | movementState = if nextPos == goal then Idle else Move (getDirection nextPos goal) goal})
        Idle -> dict
    Nothing -> dict

handleError f result =
  case result of
    Err err -> Error "invalid move!"
    Ok x -> f x

moveSoldiersCmd : Dict.Dict GameApi.RegionId SoldierModel -> Cmd Msg
moveSoldiersCmd newUnits = 
  Process.sleep (1000) |> Task.perform (\_ -> MoveSoldier (findNextToMove newUnits))

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Select r ->
      ({model | selection = addSelection r model.selection }, Cmd.none)
    ReceiveUnits r ->
      let 
        newUnits = receiveUnits r model.units
        canMove = 
          case findNextToMove newUnits of
            Just _ -> True
            Nothing -> False
      in 
        if model.isMoving && canMove then (model, Cmd.none) else ({model | units = newUnits}, moveSoldiersCmd newUnits)
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
    MoveSoldier nextToMove ->
      case nextToMove of
        Just currentPos -> 
          let newUnits = Debug.log ("Currently moving soldier") (moveSoldier currentPos model.units)
          in ({model | units = newUnits, isMoving = True}, moveSoldiersCmd newUnits)
        Nothing -> 
          Debug.log ("Done moving soldier") ({model | isMoving = False}, Cmd.none)

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
  in Html.table [id "game"] <| List.map (\i -> Html.tr [] <| row i) lst
  
movementClass : MovementState -> String
movementClass movementState =
  case movementState of
    Idle -> "hero-idle"
    Move direction _ ->
      case direction of
         Up -> "hero-walk-up"
         Down -> "hero-walk-down"
         Left -> "hero-walk-left"
         Right -> "hero-walk-right"

unitAnimation model i j = 
    let 
      soldierClass = 
        case Dict.get (i, j) model.units of
          Just soldier -> 
            if soldier.soldierData.faction == playerId then
              [class ("hero " ++ movementClass soldier.movementState)]
            else 
              [class "enemy"]
          Nothing ->
            []
      selected = 
        if isSelected (i, j) model.selection then
          [class "selected"]
        else []
    in List.append soldierClass selected 
    
isSelectedClass model i j  =  
  if isSelected (i, j) model.selection then
    class "selected"
  else class ""

individualButton : Model -> Int -> Int -> Html Msg
individualButton model i j = 
  let hp = Dict.get (i, j) model.units |> Maybe.map (\x -> [text <| String.fromInt x.soldierData.hp]) |> Maybe.withDefault []
  in div [onClick <| Select (i, j), class "county", isSelectedClass model i j] [div (unitAnimation model i j) []]

positionToString (x,y) = 
  "(" ++ String.fromInt x ++ ", " ++ String.fromInt y ++ ")"

showSoldier (x,y) soldier = 
  ul [] [
    li [] [text <| "position: " ++ (positionToString (x,y))],
    li [] [text <| "hp: " ++ String.fromInt soldier.hp],
    li [] [text <| "movement: " ++ String.fromInt soldier.movement],
    li [] [text <| "attack: " ++ String.fromInt soldier.attack],
    li [] [text <| "range: " ++ String.fromInt soldier.range],
    li [] [text <| "faction: " ++ String.fromInt soldier.faction]
  ]

view : Model -> Html Msg
view model = div [] [
  createTable 10 (individualButton model),
  div [class "info"] [ 
    text (showSelection model.selection),
    br [] [],
    button [onClick SendCommand] [text "Send command"],
    text (model.error),
    br [] [],
    model.units |> Dict.toList |> List.map (\(k, v) -> showSoldier k v.soldierData) |> div [],
    text "Player ID: ", input [onInput ChangePlayerId, value (String.fromInt model.playerId)] []
  ]
  ]
 