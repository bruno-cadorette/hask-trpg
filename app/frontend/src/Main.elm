module Main exposing(..)
import GameApi
import Browser
import Dict
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Platform.Cmd exposing (..)


playerId = 1
gameId = 1

main =
  Browser.element 
    { 
      init = \() -> ({selection = SelectNone, units = Dict.empty}, Cmd.none), 
      update = update, 
      view = view, 
      subscriptions = always (Sub.none) 
    }

type Selection a =
  SelectNone | SelectOne a | SelectTwo a a

regionIdToString (a, b) = "(" ++ String.fromInt a ++ ", " ++ String.fromInt b ++ ")"

showSelection selection = 
  case selection of
    SelectNone -> "()"
    SelectOne r -> regionIdToString r
    SelectTwo a b -> (regionIdToString a) ++ " -> " ++ (regionIdToString b)

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
    Success -> (model, GameApi.getGameByGameIdGameState gameId (handleError (Dict.fromList >> ReceiveUnits)))
    Error -> (model, Cmd.none)

createTable len f = 
  let 
    lst = List.range 0 len
    row i = List.map (\j -> Html.td [] [f i j]) lst
  in Html.table [] <| List.map (\i -> Html.tr [] <| row i) lst
  

individualButton i j = button [onClick <| Select (i, j)] []

view model = div [] [
  createTable 10 individualButton,
  text (showSelection model.selection),
  button [onClick SendCommand] [text "Send command"]] 
 