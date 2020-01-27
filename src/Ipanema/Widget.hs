{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module Ipanema.Widget where

import           Control.Applicative
import           Control.Lens
import           Data.Char
import           Data.Either.Validation
import           Data.Map.Strict (Map)
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import           Reflex
import           Reflex.Dom hiding (mainWidgetWithHead)
import           Ipanema.Form

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
