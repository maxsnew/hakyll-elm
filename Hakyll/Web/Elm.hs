{-# LANGUAGE OverloadedStrings #-}
module Hakyll.Web.Elm (elmStandaloneCompiler, elmRuntime)
       where

import Data.Monoid         ((<>), mempty)
import Data.String         (fromString)
import Data.Traversable    (traverse)
import Control.Applicative ((<$>))
import Control.Monad.Error (throwError)

import Hakyll
import Text.Blaze                      (preEscapedToMarkup)
import Text.Blaze.Html5                ((!))
import Text.Blaze.Html.Renderer.String (renderHtml)

import qualified Elm.Internal.Paths          as Path
import qualified Elm.Internal.Utils          as Elm
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as Attr

{-| Compiles an elm file to a div and inline Javascript.
    
    Expects elm-runtime.js to have already been loaded on the page.

    Works for files that only import from modules in the elm runtime.
-}
elmStandaloneCompiler :: Compiler (Item String)
elmStandaloneCompiler = cached cacheName $ do
  it <- getResourceBody
  case traverse compileModule it of
    Left  err -> throwError [err]
    Right out -> return out

  where cacheName = "Hakyll.Web.Elm.elmStandaloneCompiler"

elmRuntime :: String
elmRuntime = Path.runtime


compileModule :: String -> Either String String
compileModule bod = html modul <$> js
  where modul = maybe "Main" id . Elm.moduleName $ bod
        js    = Elm.compile bod

html :: String -- ^ Module Name
        -> String -- ^ Generated Javascript
        -> String -- ^ HTML & JS
html modul genJS = renderHtml $ node <> instantiate
  where node = H.div ! Attr.id (fromString modul) $ mempty
        instantiate = (H.script ! Attr.type_ "text/javascript")
                      . preEscapedToMarkup . unlines $
                      [ genJS
                      , "var div = document.getElementById('" <> modul <> "', div);"
                      , "Elm.embed(Elm." <> modul <> ", div);"
                      ]
