module Main where

import OpticUI (animate)
import Model (state)
import View (view)

main = animate state view
