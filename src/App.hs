{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module App where

import           Ipanema.Widget
import           GHCJS.DOM.Types
import           Reflex.Dom.Core
import           Reflex.Dom.Main (mainWidgetWithHead)

main :: JSM ()
main = mainWidgetWithHead headElement changePasswordWidget

headElement :: MonadWidget t m => m ()
headElement = do
  meta $ "charset" =: "utf-8"
  meta $ "http-equiv" =: "X-UA-Compatible" <> "content" =: "IE=edge"
  meta $ "name" =: "viewport" <> "content" =: "width=device-width, initial-scale=1, maximum-scale=1, user-scalable=no"
  
  link $ "rel" =: "stylesheet" <>
         "href" =: "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css" <>
         "integrity" =: "sha384-BVYiiSIFeK1dGmJRAkycuHAHRg32OmUcww7on3RYdg4Va+PmSTsz/K68vbdEjh4u" <>
         "crossorigin" =: "anonymous"
  script $ "src" =: "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/js/bootstrap.min.js" <>
           "integrity" =: "sha384-Tc5IQib027qvyjSMfHjOMaLkfuWVxZxUPnCJA7l2mCWNIpG9mGCD8wGNIcPD7Txa" <>
           "crossorigin" =: "anonymous"
  where
    meta attrs = elAttr "meta" attrs blank
    script attrs = elAttr "meta" attrs blank
    link attrs = elAttr "meta" attrs blank
