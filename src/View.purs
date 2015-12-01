module View where


import           AsciiDoc (mkEditor, mkViewer)
import           Control.Monad.Aff
import           DOM
import           Data.Array as AR
import           Data.Foldable (mconcat)
import           Data.Lens (Lens (..), lens, (^.))
import qualified Data.Map as M
import           Data.Maybe (maybe, maybe', fromMaybe)
import           Data.Monoid (mempty)
import           Model
import           OpticUI
import           OpticUI.Markup (attr)
import qualified OpticUI.Markup.HTML as H
import           Prelude


view = mconcat [header, main']


header = with $ \s h ->
  ui $ H.header [H.classA "home-menu pure-menu pure-menu-horizontal"] $ mconcat
  [ H.a [H.classA "pure-menu-heading", attr "href" "/"] $
    mconcat [text s.title, H.span [] $ text s.subtitle]
  , H.ul [H.classA "pure-menu-list"] $
    H.li [H.classA "pure-menu-item"] $
    H.a [H.classA "pure-menu-link", attr "href" "#"] $ text $ s ^. username
  ]


mainMenu = lens ((fromMaybe emptyArticle) <<< (M.lookup (Title "MainMenu")))
                (flip $ M.insert (Title "MainMenu"))


main' :: forall eff. UI (dom :: DOM | eff) Markup App App
main' = with $ \s h ->
  withView (H.main [H.classA "pure-g"]) $
  mconcat [ withView (pureU 1 6) $ (articles <<< mainMenu) $ article
          , withView (pureU 2 3) $ articles $ foreach $ const article
          , ui $ pureU 1 6 $ text "SideMenu"
          ]


article = with $ \s h ->
  let toggleMode = const $ runHandler h $ s {mode = if (s ^. mode == View) then Edit else View}
  in ui $ mconcat
  [ H.h3 [] $ text $ s ^. title
  , if (s ^. mode == View)
    then H.div [H.onInitialized "viewer" (mkViewer $ s ^. content)] mempty
    else H.pre [H.onInitialized "editor" (mkEditor $ s ^. content), attr "style" "height:10em;font-size:14px;"] $ mempty
  , H.button [H.classA "pure-button", H.onClick toggleMode] $ H.i [H.classA "fa fa-pencil"] mempty
  ]


pureG = H.div [H.classA "pure-g"]


pureU :: Int -> Int -> Markup -> Markup
pureU n m = H.div [H.classA $ "pure-u-" ++ show n ++ "-" ++ show m]


pureImg a = H.img (mergeClass a "pure-img")


-- HELPERS


type Class = String


mergeClass :: Array Prop -> Class -> Array Prop
mergeClass p c =
  maybe' (\_ -> AR.snoc p $ H.classA c)
         (\i -> fromMaybe p $ AR.modifyAt i (concatClass c) p)
         (AR.findIndex isClassAttr p)
  where
    isClassAttr (AttrP n _) = n == "class"
    isClassAttr _ = false
    concatClass c (AttrP n v) = AttrP n $ v ++ " " ++ c
    concatClass _ a = a


addClass :: Class -> Markup -> Markup
addClass c (Markup [(Element ns t p m)]) = Markup [(Element ns t (mergeClass p c) m)]
addClass c m = H.div [H.classA c] m

