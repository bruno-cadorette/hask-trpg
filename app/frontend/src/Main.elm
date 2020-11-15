module Main exposing(..)
import GameApi exposing (..)
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
import Move exposing (..)
import Url exposing (toString)
import Http

playerId = 1

main =
  Browser.element 
    { 
      init = \() -> ({selection = SelectNone, units = Dict.empty, playerId = 1, error = "", isMoving = False, possibleActions = []}, getGameState), 
      update = update, 
      view = view, 
      subscriptions = always (Time.every 500 (\x -> SendReceiveUnitsCommand))
    }

getGameState = GameApi.getUnitpositions (handleError (Dict.fromList >> ReceiveUnits))
type Selection a =
  SelectNone | SelectOne a | SelectOneWithAction a PlayerInputType | SelectTwo a a PlayerInputType

regionIdToString (a, b) = "(" ++ String.fromInt a ++ ", " ++ String.fromInt b ++ ")"
actionToString act =
  case act of 
    Movement -> "Movement"
    Attack -> "Attack"
showSelection : Selection (Int, Int) -> String
showSelection selection = 
  case selection of
    SelectNone -> "()"
    SelectOne a -> regionIdToString a
    SelectOneWithAction a act -> actionToString act ++ ": " ++ (regionIdToString a)
    SelectTwo a b act -> actionToString act ++ ": " ++ (regionIdToString a) ++ " -> " ++ (regionIdToString b)

isSelected x selection = 
  case selection of
    SelectNone -> False
    SelectOne a  -> a == x
    SelectOneWithAction a _ -> a == x
    SelectTwo a b _ -> a == x || b == x

type alias Model = {
    selection : Selection GameApi.RegionId,
    units : Dict.Dict GameApi.RegionId SoldierModel,
    error : String,
    playerId : Int,
    isMoving : Bool,
    possibleActions : List PossibleInput
  }

type alias SoldierModel = {
    movementState : MovementState,
    soldierData : GameApi.SoldierUnit
  }
type Msg = 
    Select RegionId
  | SelectAction PlayerInputType
  | ReceivePossibleActions (List PossibleInput)
  | ReceiveUnits (Dict.Dict RegionId SoldierUnit)
  | MoveSoldier (Maybe RegionId)
  | SendReceiveUnitsCommand
  | SendCommand
  | ChangePlayerId String
  | Error String
  | Success

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

findNextToMove dict =
  dict 
  |> Dict.toList 
  |> find (\(currentPos, v) -> 
      case v.movementState of
          Move _ futurePos -> currentPos /= futurePos
          _ -> False)
  |> Maybe.map Tuple.first


errorToString : Http.Error -> String
errorToString error =
    case error of
        Http.BadUrl url ->
            "The URL " ++ url ++ " was invalid"
        Http.Timeout ->
            "Timeout"
        Http.NetworkError ->
            "NetworkError"
        Http.BadStatus n ->
            "BadStatus " ++ (String.fromInt n)
        Http.BadBody errorMessage ->
            "BadBody " ++ errorMessage

handleError f result =
  case result of
    Err err -> Error (errorToString err)
    Ok x -> f x

moveSoldiersCmd : Dict.Dict GameApi.RegionId SoldierModel -> Cmd Msg
moveSoldiersCmd newUnits = 
  Process.sleep (1000) |> Task.perform (\_ -> MoveSoldier (findNextToMove newUnits))

setSelection selection model = 
  let 
    possibleActions =
      case selection of
        SelectNone ->
          []
        _ -> model.possibleActions
  in {model | possibleActions = possibleActions, selection = selection}

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Select (x,y) ->
      case model.selection of
        SelectNone ->
          let 
            cmd = 
              GameApi.getPossibleactionsByPlayerIdByXByY 
                model.playerId x y (handleError ReceivePossibleActions)

          in (setSelection (SelectOne (x,y)) model, cmd)
        SelectOneWithAction a act ->
          (setSelection (SelectTwo a (x,y) act) model, Cmd.none)
        _ -> 
          (setSelection SelectNone model, Cmd.none)
    SelectAction act ->
      let 
        newSelection =
          case model.selection of
            SelectOne x -> SelectOneWithAction x act
            _ -> SelectNone
      in (setSelection newSelection model, Cmd.none)
    ReceivePossibleActions inputs ->
      ({model | possibleActions = inputs}, Cmd.none )
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
            SelectTwo origin destination act -> 
              GameApi.postPlayeractionByPlayerId 
                model.playerId
                {origin = origin, destination = destination, inputType = act}
                (handleError (always Success))
            _ -> Cmd.none
      in (setSelection SelectNone model, cmd)
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
  
isSelectable position model =
  case model.selection of
    SelectOneWithAction _ act ->
      model.possibleActions 
      |> any (\(PossibleInput action list) -> action == act && member position list)
    SelectNone -> 
      case Dict.get position model.units of
          Just soldier -> soldier.soldierData.faction == playerId
          Nothing -> False
    _ -> False

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
    in List.concat [soldierClass, selected] 
    
isSelectedClass model i j  =  
  if isSelected (i, j) model.selection then
    class "selected"
  else class ""

individualButton : Model -> Int -> Int -> Html Msg
individualButton model i j = 
  let 
    selectable =
      if isSelectable (i,j) model then
        "selectable"
      else
        "inacessible"
  in div [onClick <| Select (i, j), class "county", isSelectedClass model i j, class selectable] [div (unitAnimation model i j) []]

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

uiButtons model =
  let 
    isSendDisabled = 
      case model.selection of
        SelectTwo _ _ _ -> False
        _ -> True
  in 
    (model.possibleActions |> List.map (\(PossibleInput action _) -> button [onClick (SelectAction action)] [text (actionToString action)])) ++ 
    [button [onClick SendCommand, disabled isSendDisabled] [text "Send command"]]


view : Model -> Html Msg
view model = div [] [
  createTable 10 (individualButton model),
  div [class "info"] [ 
    text (showSelection model.selection),
    br [] [],
    div [] (uiButtons model),
    text (model.error),
    br [] [],
    model.units |> Dict.toList |> List.map (\(k, v) -> showSoldier k v.soldierData) |> div [],
    text "Player ID: ", input [onInput ChangePlayerId, value (String.fromInt model.playerId)] []
  ]
  ]
 