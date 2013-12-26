{-# LANGUAGE OverloadedStrings #-}
module Hakyll.Web.Elm
       where

import Data.Monoid         ((<>), mempty)
import Data.String         (fromString)
import Data.Traversable    (traverse)
import Control.Applicative ((<$>))
import Control.Monad.Error (throwError)

--import Debug.Trace

import qualified Language.Elm      as Elm
import Hakyll
import Text.Blaze                  (preEscapedToMarkup)
import Text.Blaze.Html5            ((!))
import Text.Blaze.Html.Renderer.String (renderHtml)
import qualified Text.Blaze.Html5  as H
import qualified Text.Blaze.Html5.Attributes as Attr

elmCompiler :: Compiler (Item String)
elmCompiler = do
  it <- getResourceBody
  case traverse compileModule it of
    Left  err -> throwError [err]
    Right out -> return out

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
