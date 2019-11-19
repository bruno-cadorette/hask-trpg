module Main exposing(..)
import GameApi
import Browser
import Dict
import Maybe
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Platform.Cmd exposing (..)
import List exposing (..)


playerId = 1
gameId = 1

main =
  Browser.element 
    { 
      init = \() -> ({selection = SelectNone, units = Dict.empty}, getGameState), 
      update = update, 
      view = view, 
      subscriptions = always (Sub.none) 
    }

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
    selection : Selection GameApi.RegionId,
    units : Dict.Dict GameApi.RegionId GameApi.SoldierUnit
  }

type Msg = 
    Select GameApi.RegionId
  | ReceiveUnits (Dict.Dict GameApi.RegionId GameApi.SoldierUnit)
  | SendCommand
  | Error
  | Success

handleError f result =
  case result of
    Err _ -> Error
    Ok x -> f x

update msg model =
  case msg of
    Select r ->
      ({model | selection = addSelection r model.selection }, Cmd.none)
    ReceiveUnits r ->
      ({model | units = r}, Cmd.none)
    SendCommand ->
      let 
        cmd =
          case model.selection of
            SelectTwo origin destination -> 
              GameApi.postGameByGameIdGameStateByPlayerId 
                playerId 
                gameId 
                {origin = origin, destination = destination, inputType = GameApi.Movement}
                (handleError (always Success))
            _ -> Cmd.none
      in ({ model | selection = SelectNone }, cmd)
    Success -> (model, getGameState)
    Error -> (model, Cmd.none)

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
        if soldier.faction == playerId then
          [class "player"]
        else 
          [class "enemy"]
      Nothing ->
        []

individualButton model i j = 
  let hp = Dict.get (i, j) model.units |> Maybe.map (\x -> [text <| String.fromInt x.hp]) |> Maybe.withDefault []
  in button ([onClick <| Select (i, j), class "county"] ++ (buttonClasses model i j)) hp

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
  createTable 10 (individualButton model),
  text (showSelection model.selection),
  br [] [],
  button [onClick SendCommand] [text "Send command"],
  br [] [],
  model.units |> Dict.toList |> List.map showSoldier |> div []
  ]
 