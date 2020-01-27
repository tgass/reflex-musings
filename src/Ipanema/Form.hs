{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module Ipanema.Form where

import           Control.Applicative
import           Control.Lens
import           Control.Monad
import           Data.Char
import           Data.Either.Validation
import           Data.Map.Strict (Map)
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import           Reflex
import           Reflex.Dom hiding (mainWidgetWithHead)
import           Reflex.Dom.Main (mainWidgetWithHead)

data Form a b = Form {
    _myCurrentPw :: a
  , _myNewPw :: b
  , _myNewPwRepeat :: b
  }

makeLenses ''Form

type FormValidated = Form Text Password
type FormRaw = Form (Maybe Text) (Maybe Text)

newtype Password = Password Text deriving (Show, Eq)

emptyForm :: FormRaw
emptyForm = Form Nothing Nothing Nothing

data Reason = PasswordMismatch | CurrentPasswordEmpty | PasswordTooShort | PasswordTooLong | PasswordRequiresCapitalLetter deriving Show

changePasswordWidget :: MonadWidget t m => m ()
changePasswordWidget = do
  rec dynFormRaw <- foldDyn appEndo emptyForm $ leftmost [updateEvt, (Endo $ const emptyForm) <$ saveEvt]
      (dynFormValidated, updateEvt) <- el "form" $ runEventWriterT $ do

        dynCurrentPassword <- formGroup 
           "Current Password" 
           myCurrentPw 
           validateNonEmpty 
           "password" 
           dynFormRaw 
           saveEvt

        dynNewPassword <- formGroup "New Password" myNewPw validatePassword "password" dynFormRaw saveEvt
        dynNewPasswordRepeat <- formGroup "New Password (repeat)" myNewPwRepeat validatePasswordRepeat "password" dynFormRaw saveEvt
        return $ (liftA3 . liftA3) Form <$> dynCurrentPassword <*> dynNewPassword <*> dynNewPasswordRepeat
      saveEvt <- el "div" $ boolButton ("class" =: "btn btn-info") (isValid <$> dynFormValidated) $ text "Save"
  return ()
  where
    isValid :: Maybe (Validation [Reason] FormValidated) -> Bool
    isValid (Just (Success _)) = True
    isValid _ = False

formGroup :: forall t m a. (MonadWidget t m, EventWriter t (Endo FormRaw) m) => 
       Text
    -> Lens' FormRaw (Maybe Text)
    -> (FormRaw -> Maybe (Validation [Reason] a))
    -> Text
    -> Dynamic t FormRaw 
    -> Event t ()
    -> m (Dynamic t (Maybe (Validation [Reason] a)))
formGroup label lens validator fieldType dynFormRaw resetEvt = do
  let dynValidated = validator <$> dynFormRaw
  elDynAttr "div" (mergeWithClass "form-group has-feedback" . mkClass <$> dynValidated) $ do
    elAttr "label" ("class" =: "control-label") $ text label
    ti <- textInput $ def
            & textInputConfig_attributes .~ pure ("class" =: "form-control" <> "placeholder" =: label)
            & textInputConfig_setValue .~ ("" <$ resetEvt)
            & textInputConfig_inputType .~ fieldType
    tellEvent $ fmap (\val -> Endo $ lens .~ Just val) $ tag (current $ ti ^. textInput_value) $ ffilter not $ updated $ ti ^. textInput_hasFocus
    elAttr "span" ("class" =: "form-control-feedback") $ elDynAttr "span" (mkFeedbackClass <$> dynValidated) blank
    void $ dyn $ reasonWidget <$> dynValidated
  return dynValidated
  where
    mergeWithClass :: Text -> Map Text Text -> Map Text Text
    mergeWithClass clazz attrMap = Map.unionWith (\t1 t2 -> t1 `T.append` " " `T.append` t2) ("class" =: clazz) attrMap

    mkClass :: Maybe (Validation e a) -> Map Text Text
    mkClass Nothing = Map.empty
    mkClass (Just (Success _)) = "class" =: "has-success"
    mkClass (Just (Failure _)) = "class" =: "has-error"

    mkFeedbackClass :: Maybe (Validation e a) -> Map Text Text
    mkFeedbackClass (Just (Success _)) = "class" =: "glyphicon glyphicon-ok"
    mkFeedbackClass _ = Map.empty

    reasonWidget :: MonadWidget t m => Maybe (Validation [Reason] a) -> m ()
    reasonWidget (Just (Failure reasons)) = forM_ reasons $ \reason ->
      divClass "margin-left-s" $ 
        el "small" $ text $ T.pack $ show reason
    reasonWidget _ = blank

validateNonEmpty :: FormRaw -> Maybe (Validation [Reason] Text)
validateNonEmpty (Form Nothing _ _) = Nothing
validateNonEmpty (Form (Just myCurrentPw) _ _)
  | not $ T.null myCurrentPw = Just $ Success myCurrentPw
  | otherwise = Just $ Failure [CurrentPasswordEmpty]

validatePasswordRepeat :: FormRaw -> Maybe (Validation [Reason] Password)
validatePasswordRepeat (Form _ _ Nothing) = Nothing
validatePasswordRepeat form@Form{..} = case validatePassword form of
  Just (Success (Password password)) -> if (Just password) == _myNewPwRepeat
                   then Just $ Success $ Password password 
                   else Just $ Failure [PasswordMismatch]
  _ -> Nothing

validatePassword :: FormRaw -> Maybe (Validation [Reason] Password)
validatePassword (Form _ (Just pw) _) = Just $
  valPasswordSize pw <*
  valPasswordRequiresCapitalLetter pw 
validatePassword _ = Nothing

valPasswordSize :: Text -> Validation [Reason] Password
valPasswordSize text
  | T.length text < 6 = Failure [PasswordTooShort]
  | T.length text > 20 = Failure [PasswordTooLong]
  | otherwise = Success $ Password text

valPasswordRequiresCapitalLetter :: Text -> Validation [Reason] Password
valPasswordRequiresCapitalLetter text
  | T.any isUpper text = Success $ Password text
  | otherwise = Failure [PasswordRequiresCapitalLetter]

boolButton :: MonadWidget t m => Map Text Text -> Dynamic t Bool -> m a -> m (Event t ())
boolButton attrs dynIsEnabled child = do
  (e, _) <- elDynAttr' "button" (mkAttr <$> dynIsEnabled) child
  return $ domEvent Click e
  where
    mkAttr True = attrs
    mkAttr False = attrs <> "disabled" =: "disabled"
