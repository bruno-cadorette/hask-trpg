module Move exposing (..)
import GameApi
import Dict


type Direction =
    Up
  | Down
  | Left
  | Right
type MovementState = 
    Idle
  | Move Direction GameApi.Position


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


