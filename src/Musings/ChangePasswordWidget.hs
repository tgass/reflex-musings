{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module Musings.ChangePasswordWidget where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Reader
import           Control.Lens
import           Data.Char
import           Data.Either.Validation
import           Data.Map.Strict (Map)
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import           Reflex
import           Reflex.Dom hiding (mainWidgetWithHead)
import           Musings.FormGroup

newtype Password = Password Text deriving (Show, Eq)

data Form a b = Form {
    _fCurrentPassword :: a
  , _fNewPassword :: b
  , _fNewPasswordRepeat :: b
  } deriving Show

makeLenses ''Form

type FormValidated = Form Text Password
type FormRaw = Form (Maybe Text) (Maybe Text)

emptyForm :: FormRaw
emptyForm = Form Nothing Nothing Nothing

data Reason = PasswordMismatch | FieldRequired | PasswordTooShort | PasswordTooLong | PasswordRequiresCapitalLetter deriving Show

changePasswordWidget :: MonadWidget t m => m ()
changePasswordWidget = do
  rec dynFormRaw <- foldDyn appEndo emptyForm updateEvt
      (dynFormValidated, updateEvt) <- el "form" $ flip runReaderT (saveEvt, dynFormRaw) $ runEventWriterT $ do

        dynCurrentPassword <- formGroup Config {
             label = "Current Password" 
           , selector = fCurrentPassword 
           , validator = fmap validateNonEmpty . view fCurrentPassword
           , fieldType = "password" 
           }

        dynNewPassword <- formGroup Config {
             label = "New Password" 
           , selector = fNewPassword 
           , validator = fmap validatePassword . view fNewPassword
           , fieldType = "password" 
           }

        dynNewPasswordRepeat <- formGroup Config {
             label = "New Password (repeat)" 
           , selector = fNewPasswordRepeat 
           , validator = \form -> join $ validatePasswordRepeat <$> (form ^. fNewPassword) <*> (form ^. fNewPasswordRepeat)
           , fieldType = "password" 
           }

        return $ (liftA3 . liftA3) Form <$> dynCurrentPassword <*> dynNewPassword <*> dynNewPasswordRepeat
      saveEvt <- el "div" $ boolButton ("class" =: "btn btn-info") (isValid <$> dynFormValidated) $ text "Save"
  return ()

validateNonEmpty :: Text -> Validation [Reason] Text
validateNonEmpty text 
  | not $ T.null text = Success text
  | otherwise = Failure [FieldRequired]

validatePasswordRepeat :: Text -> Text -> Maybe (Validation [Reason] Password)
validatePasswordRepeat passwordNew passwordRepeat = case validatePassword passwordNew of
  (Success (Password password)) 
    | password == passwordRepeat -> Just $ Success $ Password password 
    | otherwise -> Just $ Failure [PasswordMismatch]
  _ -> Nothing

validatePassword :: Text -> Validation [Reason] Password
validatePassword password = 
  valLengthRange 6 20 password <*
  valRequiresCapitalLetter password 

valLengthRange :: Int -> Int -> Text -> Validation [Reason] Password
valLengthRange min max text
  | T.length text < min = Failure [PasswordTooShort]
  | T.length text > max = Failure [PasswordTooLong]
  | otherwise = Success $ Password text

valRequiresCapitalLetter :: Text -> Validation [Reason] Password
valRequiresCapitalLetter text
  | T.any isUpper text = Success $ Password text
  | otherwise = Failure [PasswordRequiresCapitalLetter]

boolButton :: MonadWidget t m => Map Text Text -> Dynamic t Bool -> m a -> m (Event t ())
boolButton attrs dynIsEnabled child = do
  (e, _) <- elDynAttr' "button" (mkAttr <$> dynIsEnabled) child
  return $ domEvent Click e
  where
    mkAttr True = attrs
    mkAttr False = attrs <> "disabled" =: "disabled"

isValid :: Maybe (Validation [Reason] FormValidated) -> Bool
isValid (Just (Success _)) = True
isValid _ = False

