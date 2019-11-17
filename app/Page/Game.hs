{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Page.Game where

import Lucid
import Game.Effects
import Data.Map 
import Control.Monad.Reader


headTemplate title = head_ (title_ title)
pageTemplate title pageBody = doctypehtml_ (headTemplate title <> body_ pageBody)

css = style_ (".county {\
        \    border: solid;\
        \    height: 20px;\
        \    width: 20px;\
        \}\
        \\
        \div[data-faction='0'] {\
        \    background-color: lightyellow\
        \}\
        \\
        \div[data-faction='1'] {\
        \    background-color: greenyellow\
        \}\
        \\
        \div[data-faction='2'] {\
        \    background-color: cornflowerblue\
        \}\
        \\
        \.selected {\
        \    background-color: red\
        \}\
        \\
        \.inacessible {\
        \    filter: brightness(40%);\
        \}")

game :: Html()
game = doctypehtml_ $ do
    head_ $ do
        title_ "Game"
        css
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
 