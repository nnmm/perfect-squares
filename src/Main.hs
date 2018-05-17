{-# LANGUAGE OverloadedStrings #-}
module Main where

import SDL
import SDL.Vect (Point (..))
import Linear (V4 (..))
import Control.Monad (unless)

main :: IO ()
main = do
  initializeAll
  window <- createWindow "My SDL Application" defaultWindow
  renderer <- createRenderer window (-1) defaultRenderer
  appLoop renderer

appLoop :: Renderer -> IO ()
appLoop renderer = do
  events <- pollEvents
  let eventIsQPress event =
        case eventPayload event of
          KeyboardEvent keyboardEvent ->
            keyboardEventKeyMotion keyboardEvent == Pressed &&
            keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeQ
          _ -> False
      qPressed = any eventIsQPress events
  rendererDrawColor renderer $= V4 255 255 255 255
  clear renderer
  rendererDrawColor renderer $= V4 0 0 255 255
  drawRect renderer (Just $ Rectangle (P $ V2 10 10) (V2 10 10))
  present renderer
  unless qPressed (appLoop renderer)