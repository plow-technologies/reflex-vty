{-|
Module: Reflex.Vty.Widget.Input
Description: User input widgets for reflex-vty
-}
module Reflex.Vty.Widget.Input
  ( module Export
  , module Reflex.Vty.Widget.Input
  ) where


import Reflex.Vty.Widget.Input.Mouse as Export
import Reflex.Vty.Widget.Input.Text as Export

import Control.Monad (join)
import Control.Monad.Fix (MonadFix)
import Data.Default (Default(..))
import Data.List (foldl')
import Data.Text (Text)
import qualified Graphics.Vty as V
import Reflex
import Reflex.Vty.Widget
import Reflex.Vty.Widget.Box
import Reflex.Vty.Widget.Text

-- * Buttons

-- | Configuration options for the 'button' widget
data ButtonConfig t = ButtonConfig
  { _buttonConfig_boxStyle :: Dynamic t BoxStyle
  , _buttonConfig_focusStyle :: Dynamic t BoxStyle
  }

instance Reflex t => Default (ButtonConfig t) where
  def = ButtonConfig (pure singleBoxStyle) (pure thickBoxStyle)

-- | A button widget that contains a sub-widget
button
  :: (MonadFix m, MonadHold t m, HasFocusReader t m, HasTheme t m, HasDisplayRegion t m, HasImageWriter t m, HasInput t m)
  => ButtonConfig t
  -> m ()
  -> m (Event t ())
button cfg child = do
  f <- focus
  let style = do
        isFocused <- f
        if isFocused
          then _buttonConfig_focusStyle cfg
          else _buttonConfig_boxStyle cfg
  box style child
  m <- mouseUp
  k <- key V.KEnter
  return $ leftmost [() <$ k, () <$ m]

-- | A button widget that displays text that can change
textButton
  :: (MonadFix m, MonadHold t m, HasDisplayRegion t m, HasFocusReader t m, HasTheme t m, HasImageWriter t m, HasInput t m)
  => ButtonConfig t
  -> Dynamic t Text
  -> m (Event t ())
textButton cfg = button cfg . text -- TODO Centering etc.

-- | A button widget that displays a static bit of text
textButtonStatic
  :: (MonadFix m, MonadHold t m, HasDisplayRegion t m, HasFocusReader t m, HasTheme t m, HasImageWriter t m, HasInput t m)
  => ButtonConfig t
  -> Text
  -> m (Event t ())
textButtonStatic cfg = textButton cfg . pure

-- * Links

-- | A clickable link widget
link
  :: (Reflex t, Monad m, HasDisplayRegion t m, HasImageWriter t m, HasInput t m, HasTheme t m)
  => Dynamic t Text
  -> m (Event t MouseUp)
link t = do
  bt <- theme
  let cfg = RichTextConfig
        { _richTextConfig_attributes = fmap (\attr -> V.withStyle attr V.underline) bt
        }
  richText cfg t
  mouseUp

-- | A clickable link widget with a static label
linkStatic
  :: (Reflex t, Monad m, HasImageWriter t m, HasDisplayRegion t m, HasInput t m, HasTheme t m)
  => Text
  -> m (Event t MouseUp)
linkStatic = link . pure

-- * Checkboxes

-- | Characters used to render checked and unchecked textboxes
data CheckboxStyle = CheckboxStyle
  { _checkboxStyle_unchecked :: Text
  , _checkboxStyle_checked :: Text
  }

instance Default CheckboxStyle where
  def = checkboxStyleTick

-- | This checkbox style uses an "x" to indicate the checked state
checkboxStyleX :: CheckboxStyle
checkboxStyleX = CheckboxStyle
  { _checkboxStyle_unchecked = "[ ]"
  , _checkboxStyle_checked = "[x]"
  }

-- | This checkbox style uses a unicode tick mark to indicate the checked state
checkboxStyleTick :: CheckboxStyle
checkboxStyleTick = CheckboxStyle
  { _checkboxStyle_unchecked = "[ ]"
  , _checkboxStyle_checked = "[✓]"
  }

-- | Configuration options for a checkbox
data CheckboxConfig t = CheckboxConfig
  { _checkboxConfig_checkboxStyle :: Dynamic t CheckboxStyle
  -- TODO DELETE and use HasTheme instead
  , _checkboxConfig_attributes :: Dynamic t V.Attr
  , _checkboxConfig_setValue :: Event t Bool
  }

instance (Reflex t) => Default (CheckboxConfig t) where
  def = CheckboxConfig
    { _checkboxConfig_checkboxStyle = pure def
    , _checkboxConfig_attributes = pure V.defAttr
    , _checkboxConfig_setValue = never
    }

-- | A checkbox widget
checkbox
  :: (MonadHold t m, MonadFix m, Reflex t, HasInput t m, HasDisplayRegion t m, HasImageWriter t m, HasFocusReader t m, HasTheme t m)
  => CheckboxConfig t
  -> Bool
  -> m (Dynamic t Bool)
checkbox cfg v0 = do
  md <- mouseDown V.BLeft
  mu <- mouseUp
  space <- key (V.KChar ' ')
  f <- focus
  v <- foldDyn ($) v0 $ leftmost
    [ not <$ mu
    , not <$ space
    , const <$> _checkboxConfig_setValue cfg
    ]
  depressed <- holdDyn V.defaultStyleMask $ leftmost
    [ V.bold <$ md
    , V.defaultStyleMask <$ mu
    ]
  let focused = ffor f $ \x -> if x then V.bold else V.defaultStyleMask
  let attrs = combineStyles
        <$> _checkboxConfig_attributes cfg
        <*> sequence [depressed, focused]
  richText (RichTextConfig attrs) $ join $ ffor v $ \checked ->
    if checked
      then _checkboxStyle_checked <$> _checkboxConfig_checkboxStyle cfg
      else _checkboxStyle_unchecked <$> _checkboxConfig_checkboxStyle cfg
  return v
  where
    combineStyles :: V.Attr -> [V.Style] -> V.Attr
    combineStyles x xs = foldl' V.withStyle x xs
