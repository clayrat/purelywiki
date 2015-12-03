module Model where


import Prelude
import Data.Date (Date (..))
import Data.Generic
import Data.Lens (lens, Lens (..))
import Data.Maybe (Maybe (..))
import Data.Foldable (foldMap, Foldable)
import Data.Monoid (mempty)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.List as L


newtype User = User String


newtype WikiText = WikiText String


newtype Title = Title String

derive instance genericTitle :: Generic Title
instance eqTitle :: Eq Title where
  eq = gEq
instance ordTitle :: Ord Title where
  compare = gCompare


type Tag = Title


data Mode = View | Edit

derive instance genericMode :: Generic Mode
instance eqMode :: Eq Mode where
  eq = gEq

type Article =
  { created :: Maybe Date
  , updated :: Maybe Date
  , title :: Title
  , content :: WikiText
  , tags :: S.Set Tag
  , author :: User
  , mode :: Mode
  , revision :: Int
  }

type Articles = M.Map Title Article

type App =
  { articles :: Articles
  , user :: User
  , title :: String
  , subtitle :: String
  }


mkArticlesMap :: forall f. (Foldable f) => f Article -> Articles
mkArticlesMap = foldMap $ \a -> M.singleton a.title a


emptyArticle :: Article
emptyArticle =
  { created: Nothing
  , updated: Nothing
  , title: Title ""
  , content: WikiText ""
  , tags: S.empty
  , author: User "d(^_^)b"
  , mode: View
  , revision: 0
  }


state :: App
state =
  { articles: mkArticlesMap
      [ { created: Nothing
        , updated: Nothing
        , title: Title "Tiddly The Only One"
        , content: WikiText "We need a *markup* language."
        , tags: S.empty
        , author: User "d(^_^)b"
        , mode: View
        , revision: 0
        }
      ]
  , user: User "d(^_^)b"
  , title: "PurelyWiki"
  , subtitle: "typechecked notebook"
  }


user = lens _.user (_ { user = _ })

username :: Lens _ _ _ _
username = user <<< lens (\(User u) -> u) (\(User u) v -> User v)

articles = lens _.articles (_ { articles = _ })
title = lens (\{ title: (Title x) } -> x) (\a x -> a { title = (Title x) })
content = lens (\{ content: (WikiText x) } -> x) (\a x -> a { content = (WikiText x) })
mode = lens _.mode (_ { mode = _ })
