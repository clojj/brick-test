{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import           Control.Applicative            ( (<$>) )
import           Lens.Micro                     ( (^.)
                                                , (&)
                                                , (.~)
                                                , (%~)
                                                )
import           Lens.Micro.TH                  ( makeLenses )
import           Control.Monad                  ( void )
import           Data.Monoid                    ( (<>) )
import qualified Graphics.Vty                  as V

import qualified Brick.Types                   as T
import           Brick.AttrMap
import           Brick.Util
import           Brick.Types                    ( Widget
                                                , ViewportType(Vertical)
                                                )
import qualified Brick.Main                    as M
import qualified Brick.Widgets.Edit            as E
import qualified Brick.Widgets.Center          as C
import qualified Brick.Widgets.Border          as B
import           Brick.Widgets.Core
import           Data.Text.Zipper               ( moveCursor )
import           Data.Tuple                     ( swap )

data Name = Prose | TextBox
          deriving (Show, Ord, Eq)

data St =
    St { _clicked :: [T.Extent Name]
       , _lastReportedClick :: Maybe (Name, T.Location)
       , _prose :: String
       , _edit :: E.Editor String Name
       }

makeLenses ''St

drawUi :: St -> [Widget Name]
drawUi st = [editorLayer st <+> proseLayer st]

editorLayer :: St -> Widget Name
editorLayer st = C.hCenterLayer
  (vLimit 20 $ hLimit 80 $ E.renderEditor (str . unlines) True (st ^. edit))

proseLayer :: St -> Widget Name
proseLayer st =
  B.border
    $ vLimit 8
    $
  -- n.b. if clickable and viewport are inverted here, click event
  -- coordinates will only identify the viewable range, not the actual
  -- editor widget coordinates.
      viewport Prose Vertical
    $ clickable Prose
    $ vBox
    $ map str
    $ lines (st ^. prose)

appEvent :: St -> T.BrickEvent Name e -> T.EventM Name (T.Next St)
appEvent st (T.MouseDown n _ _ loc) = do
  let T.Location pos = loc
  M.continue $ st & lastReportedClick .~ Just (n, loc) & edit %~ E.applyEdit
    (if n == TextBox then moveCursor (swap pos) else id)
appEvent st T.MouseUp{} = M.continue $ st & lastReportedClick .~ Nothing
appEvent st (T.VtyEvent V.EvMouseUp{}) =
  M.continue $ st & lastReportedClick .~ Nothing
appEvent st (T.VtyEvent (V.EvKey V.KUp [V.MCtrl])) =
  M.vScrollBy (M.viewportScroll Prose) (-1) >> M.continue st
appEvent st (T.VtyEvent (V.EvKey V.KDown [V.MCtrl])) =
  M.vScrollBy (M.viewportScroll Prose) 1 >> M.continue st
appEvent st (T.VtyEvent (V.EvKey V.KEsc [])) = M.halt st
appEvent st (T.VtyEvent ev) =
  M.continue =<< T.handleEventLensed st edit E.handleEditorEvent ev
appEvent st _ = M.continue st

aMap :: AttrMap
aMap = attrMap V.defAttr [(E.editFocusedAttr, V.black `on` V.yellow)]

app :: M.App St e Name
app = M.App
  { M.appDraw         = drawUi
  , M.appStartEvent   = return
  , M.appHandleEvent  = appEvent
  , M.appAttrMap      = const aMap
  , M.appChooseCursor = M.showFirstCursor
  }

main :: IO ()
main = do
  let buildVty = do
        v <- V.mkVty =<< V.standardIOConfig
        V.setMode (V.outputIface v) V.Mouse True
        return v

  void $ M.customMain buildVty Nothing app $ St
    []
    Nothing
    "Press Ctrl-up and Ctrl-down arrow keys to scroll, ESC to quit.\n\
           \Observe that the click coordinates identify the\n\
           \underlying widget coordinates.\n\
           \\n\
           \Lorem ipsum dolor sit amet,\n\
           \consectetur adipiscing elit,\n\
           \sed do eiusmod tempor incididunt ut labore\n\
           \et dolore magna aliqua.\n\
           \ \n\
           \Ut enim ad minim veniam\n\
           \quis nostrud exercitation ullamco laboris\n\
           \nisi ut aliquip ex ea commodo consequat.\n\
           \\n\
           \Duis aute irure dolor in reprehenderit\n\
           \in voluptate velit esse cillum dolore eu fugiat nulla pariatur.\n\
           \\n\
           \Excepteur sint occaecat cupidatat not proident,\n\
           \sunt in culpa qui officia deserunt mollit\n\
           \anim id est laborum.\n"
    (E.editor TextBox Nothing "")

