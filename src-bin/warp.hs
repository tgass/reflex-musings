{-# LANGUAGE OverloadedStrings #-}

import qualified App as App
import           Control.Monad
import           Data.Text
import           Language.Javascript.JSaddle
import qualified GHCJS.DOM.Document as Doc
import qualified GHCJS.DOM as DOM
import qualified GHCJS.DOM.Node as DOM
import qualified GHCJS.DOM.Element as DOM
import qualified GHCJS.DOM.History as DOM
import qualified GHCJS.DOM.Window as DOM
import           Language.Javascript.JSaddle (jsNull) -- remove for wkwebview
import qualified Network.URI as URI
import           Reflex.Dom

main :: IO ()
main = run $ do
  jquery
  setInitialRoute
  App.main

setInitialRoute :: JSM ()
setInitialRoute = do
  window <- DOM.currentWindowUnchecked
  initialLocation <- DOM.getLocation window
  initialUri <- getLocationUri initialLocation
  history <- DOM.getHistory window
  DOM.replaceState history jsNull ("" :: Text) $ Just $ 
    show $ setAdaptedUriPath "/app" initialUri

setAdaptedUriPath :: String -> URI.URI -> URI.URI
setAdaptedUriPath s u = u { URI.uriPath = s }

jquery :: JSM ()
jquery = do
  Just doc <- DOM.currentDocument
  Just body <- Doc.getBody doc
  jq_el <- Doc.createElement doc ("script" :: Text)
  DOM.setAttribute jq_el ("src" :: String) ("http://localhost:8080/app/js/jquery-3.2.1.min.js" :: String)
  void $ DOM.appendChild body jq_el

