{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Page.Game where

import Lucid
import Game.Effects
import Data.Map 
import Control.Monad.Reader


headTemplate title = head_ (title_ title)
pageTemplate title pageBody = doctypehtml_ (headTemplate title <> body_ pageBody)

game :: Html()
game = doctypehtml_ $ do
    head_ $ do
        title_ "Game"
        gameImports
    body_ $ do 
        div_ [id_ "elm-node"] mempty
        script_ "Elm.Main.init({node: document.getElementById('elm-node')})"

        
    

gameImports :: Html()
gameImports = do
    script_ [src_ "static/main.js"] ""
    link_ [rel_ "stylesheet", href_ "static/game.css"]


gameBody :: Html()
gameBody = do 
    table_ [id_ "game"] mempty
    div_ (do
        "Actions: " 
        (ul_ [id_ "actions"] mempty)
        button_ [onclick_ "commit()"] "Commit")
    div_ (do
        "Money: "
        label_ [id_ "money"] "0")
    script_ ("window.onload = init")
 