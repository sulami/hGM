module Main where

import Game.DnD.GM.Entry (newEntry)

main = do let e = newEntry "Hello, World!"
          print e

