{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Ipanema.Form where

import           Control.Lens
import           Control.Monad
import           Data.Either.Validation
import           Data.Map.Strict (Map)
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import           Reflex
import           Reflex.Dom hiding (mainWidgetWithHead)

formGroup :: (Show reason, MonadWidget t m, EventWriter t (Endo form) m) => 
       Text
    -> Lens' form (Maybe Text)
    -> (form -> Maybe (Validation [reason] a))
    -> Text
    -> Dynamic t form 
    -> Event t ()
    -> m (Dynamic t (Maybe (Validation [reason] a)))
formGroup label lens validator fieldType dynFormRaw resetEvt = do
  let dynValidated = validator <$> dynFormRaw
  elDynAttr "div" (mergeWithClass "form-group has-feedback" . mkClass <$> dynValidated) $ do
    elAttr "label" ("class" =: "control-label") $ text label
    ti <- textInput $ def
            & textInputConfig_attributes .~ pure ("class" =: "form-control" <> "placeholder" =: label)
            & textInputConfig_setValue .~ ("" <$ resetEvt)
            & textInputConfig_inputType .~ fieldType
    tellEvent $ fmap (\val -> Endo $ lens ?~ val) $ tag (current $ ti ^. textInput_value) $ ffilter not $ updated $ ti ^. textInput_hasFocus
    elAttr "span" ("class" =: "form-control-feedback") $ elDynAttr "span" (mkFeedbackClass <$> dynValidated) blank
    void $ dyn $ reasonWidget <$> dynValidated
  return dynValidated

mergeWithClass :: Text -> Map Text Text -> Map Text Text
mergeWithClass clazz = Map.unionWith (\t1 t2 -> t1 `T.append` " " `T.append` t2) ("class" =: clazz) 

mkClass :: Maybe (Validation e a) -> Map Text Text
mkClass Nothing = Map.empty
mkClass (Just (Success _)) = "class" =: "has-success"
mkClass (Just (Failure _)) = "class" =: "has-error"

mkFeedbackClass :: Maybe (Validation e a) -> Map Text Text
mkFeedbackClass (Just (Success _)) = "class" =: "glyphicon glyphicon-ok"
mkFeedbackClass _ = Map.empty

reasonWidget :: (Show reason, MonadWidget t m) => Maybe (Validation [reason] a) -> m ()
reasonWidget (Just (Failure reasons)) = forM_ reasons $ \reason ->
  divClass "margin-left-s" $ 
    el "small" $ text $ T.pack $ show reason
reasonWidget _ = blank

