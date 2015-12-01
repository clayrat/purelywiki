module AsciiDoc where

import DOM.HTML.Types (HTMLElement ())
import Control.Monad.Eff (Eff ())
import Prelude


foreign import convert :: String -> String
foreign import unsafeConvertToNode :: forall eff. String -> HTMLElement -> Eff eff Unit
