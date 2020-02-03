{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}

module Musings.FormGroup where

import           Control.Arrow
import           Control.Lens
import           Control.Monad
import           Control.Monad.Reader
import           Data.Either.Validation
import           Data.Map.Strict (Map)
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import           Reflex
import           Reflex.Dom hiding (mainWidgetWithHead)

data Config form reason a = Config {
    label :: Text
  , lens :: Lens' form (Maybe Text)
  , validator :: form -> Maybe (Validation [reason] a)
  , fieldType :: Text
  }

formGroup :: (Show reason, MonadWidget t m, EventWriter t (Endo form) m, MonadReader (Event t (), Dynamic t form) m) => Config form reason a -> m (Dynamic t (Maybe (Validation [reason] a)))
formGroup Config{..} = do
  (resetEvt, dynValidated) <- asks (second (fmap validator))
  elDynAttr "div" (mergeWithClass "form-group has-feedback" . mkStateAttrs <$> dynValidated) $ do
    elAttr "label" ("class" =: "control-label") $ text label
    ti <- textInput $ def
            & textInputConfig_attributes .~ pure ("class" =: "form-control" <> "placeholder" =: label)
            & textInputConfig_setValue .~ ("" <$ resetEvt)
            & textInputConfig_inputType .~ fieldType
    tellEvent $ fmap (\val -> Endo $ lens ?~ val) $ tag (current $ ti ^. textInput_value) $ ffilter not $ updated $ ti ^. textInput_hasFocus
    elAttr "span" ("class" =: "form-control-feedback") $ elDynAttr "span" (mkFeedbackAttrs <$> dynValidated) blank
    void $ dyn $ reasonWidget <$> dynValidated
  return dynValidated

mergeWithClass :: Text -> Map Text Text -> Map Text Text
mergeWithClass clazz = Map.adjust (\c -> clazz <> " " <> c) "class"

mkStateAttrs :: Maybe (Validation e a) -> Map Text Text
mkStateAttrs Nothing = Map.empty
mkStateAttrs (Just (Success _)) = "class" =: "has-success"
mkStateAttrs (Just (Failure _)) = "class" =: "has-error"

mkFeedbackAttrs :: Maybe (Validation e a) -> Map Text Text
mkFeedbackAttrs (Just (Success _)) = "class" =: "glyphicon glyphicon-ok"
mkFeedbackAttrs _ = Map.empty

reasonWidget :: (Show reason, MonadWidget t m) => Maybe (Validation [reason] a) -> m ()
reasonWidget (Just (Failure reasons)) = forM_ reasons $ \reason ->
  divClass "margin-left-s" $ 
    el "small" $ text $ T.pack $ show reason
reasonWidget _ = blank

