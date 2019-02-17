{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE MonoLocalBinds #-}

module Main where

import Protolude
import Reflex.Dom

import Data.FileEmbed
import qualified Data.Map.Strict as M


main :: IO ()
main = do
  mw $(embedFile "/home/czechow/work/spike/rfl-first/materialize.css")

mw :: ByteString -> IO ()
mw css = mainWidgetWithCss css $ do
  elAttr "script"  (M.fromList [("type", "text/javascript"), ("src", "/home/czechow/work/spike/rfl-first/materialize.js")]) $ text ""
  void $ workflow w1


buttonClass :: MonadWidget t m => Text -> Text -> m (Event t ())
buttonClass s c = do
  (e, _) <- elAttr' "button" (M.fromList [("type", "button"), ("class", c)]) (text s)
  return $ domEvent Click e

buttonClass3 :: MonadWidget t m => Dynamic t Text -> Text -> Dynamic t Bool -> m (Event t ())
buttonClass3 s c ds = do
  let ds' = fmap (\v -> if v then stdp else (M.insert "disabled" "" stdp)) ds
  (e, _) <- elDynAttr' "button" ds' (dynText s)
  return $ domEvent Click e
  where
    stdp = M.fromList [("type", "button"), ("class", c)]


myTextInput :: MonadWidget t m => Text -> Text -> m (Event t ())
myTextInput = myInput "text"

myPassInput :: MonadWidget t m => Text -> Text -> m (Event t ())
myPassInput = myInput "password"

myInput :: MonadWidget t m => Text -> Text -> Text -> m (Event t ())
myInput type' id' label' = do
    (e, _) <- elClass' "div" "input-field" $ do
      r <- elAttr' "input" [("id", id'), ("type", type'), ("class", "validate")] $ blank
      elAttr "label" [("for", id')] $ text label'
      pure r
    return $ fmap (const ()) $ domEvent Keypress e



-- Login page...
w1 :: (MonadWidget t m) => Workflow t m (Event t ())
w1 = Workflow $ do
  elClass "div" "login-page" $ do
    elClass "div" "form" $ mdo
      el "h5" $ text "Login page"
      void $ myTextInput "username" "User"
      void $ myPassInput "password" "Password"

      dt2 <- holdDyn "Check me" $ leftmost ["Checking..." <$ e', "Check again" <$ doneLogin]
      dt3 <- holdDyn True $ leftmost [False <$ e', True <$ doneLogin]
      e' <- el "div" $ buttonClass3 dt2 "btn" dt3
      e <- el "div" $ buttonClass "Login" "btn"
      doneLogin <- performEvent $ ffor e' $ \_ -> liftIO $ do
        -- querying service...
        threadDelay $ 1000 * 1000 * 3

      dt <- holdDyn "" $ leftmost ["Testing..." <$ e', "Connection ok" <$ doneLogin]
      _ <- el "div" $ dynText dt
      pure (e, w2 <$ e)


-- Main page
w2 :: MonadWidget t m => Workflow t m (Event t ())
w2 = Workflow $ do
  e' <- el "nav" $
    elClass "div" "nav-wrapper" $ do
      elClass "a" "brand-logo" $ text "Logo"
      elAttr "ul" [("id", "nav-mobile"), ("class", "right")] $ do
        el "li" $ el "a" $ text "Info"
        el "li" $ buttonClass "Logout" "btn"

  e'' <- elClass "div" "container" $ do
    elClass "div" "row" $ do
      elClass "div" "col s12" $ el "h5" $ elClass "div" "center-align" $ text "Main page"
      elClass "div" "col s12" $ elClass "div" "center-align" $ buttonClass "Logout" "btn"

  let e = leftmost [e', e'']
  pure (e, w1 <$ e)

