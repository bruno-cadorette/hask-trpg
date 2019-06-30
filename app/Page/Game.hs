{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Page.Game where

import Lucid
import Game.Effects
import Data.Map 
import Control.Monad.Reader


headTemplate title = head_ (title_ title)
pageTemplate title pageBody = doctypehtml_ (headTemplate title <> body_ pageBody)

getGameHub = pure mempty

gameList :: [Html ()]
gameList = do
    gameIds <- fmap keys getGameHub
    return $ pageTemplate "Game List" $ foldMap (\(GameId id) -> li_ $ toHtml $ show id) gameIds


game :: Html()
game = doctypehtml_ $ do
    head_ (do
        title_ "Game"
        gameImports)
    body_ gameBody



gameImports :: Html()
gameImports = do
    script_ [src_ "static/api.js"] ""
    script_ [src_ "static/gameMap.js"] ""
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
 