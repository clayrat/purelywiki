module AsciiDoc where

import DOM.HTML.Types (HTMLElement ())
import Control.Monad.Eff (Eff ())
import Prelude


foreign import convert :: String -> String
foreign import mkViewer :: forall eff. String -> HTMLElement -> Eff eff Unit
foreign import mkEditor :: forall eff. String -> HTMLElement -> Eff eff Unit
