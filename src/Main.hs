module Main(main) where

import View
import Model
import Controller

import Graphics.Gloss.Interface.IO.Game

main :: IO()
main = playIO   (InWindow "Astroids without a twist" (800, 500) (0, 0))
                black
                10
                initialState
                view
                input
                step