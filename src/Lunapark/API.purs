module Lunapark.API where

import Prelude

import Control.Monad.Rec.Class (class MonadRec)
import Data.Argonaut.Core (Json) as J
import Data.Argonaut.Decode.Class (decodeJson) as J
import Data.Argonaut.Encode.Class (encodeJson) as J
import Data.Array as A
import Data.Bifunctor (lmap)
import Data.Either (Either(..), isRight)
import Data.FoldableWithIndex as FI
import Data.List (List(..), (:))
import Data.List as L
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits as Str
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable as T
import Data.Variant as V
import Effect.Aff as Aff
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Foreign.Object as FO
import Lunapark.ActionF (_lunaparkActions, LUNAPARK_ACTIONS, ActionF(..), TouchF(..))
import Lunapark.Endpoint as LP
import Lunapark.Error as LE
import Lunapark.LunaparkF (_lunapark, LUNAPARK, ElementF(..), LunaparkF(..), performActions, findElement)
import Lunapark.Types as LT
import Lunapark.Utils (liftAndRethrow, throwLeft, catch)
import Node.Buffer as B
import Node.FS.Aff as FS
import Run as R
import Run.Except (EXCEPT)
import Type.Row (type (+))

type LUNAPARK' r = (lunapark ∷ LUNAPARK | r)
type LUNAPARK_ACTIONS' r = (lunaparkActions ∷ LUNAPARK_ACTIONS | r)
type AFF' r = (aff ∷ R.AFF | r)
type EFFECT' r = (effect ∷ R.EFFECT | r)
type EXCEPT' r = (except ∷ EXCEPT LE.Error | r)

type Lunapark r = R.Run (AFF' + EFFECT' + EXCEPT' + LUNAPARK' + LUNAPARK_ACTIONS' + r)

init
  ∷ ∀ m r a
  . MonadAff m
  ⇒ MonadRec m
  ⇒ String
  → LT.CapabilitiesRequest
  → m (Either LE.Error (Lunapark r a → R.Run (AFF' + EFFECT' + EXCEPT' + r) a))
init uri caps = do
  res ←
    liftAff
      $ LP.post uri (LP.Session : Nil)
      $ LT.encodeCapabilitiesRequest caps

  let
    sessionResponse = do
      sessObj ← res
      lmap LE.unknownError $ LT.decodeCreateSessionResponse sessObj

  T.for sessionResponse \{ session, capabilities } → do
    timeoutsRef ←
      liftEffect $ Ref.new
        { implicit: Milliseconds 0.0
        , pageLoad: Milliseconds 300000.0
        , script: Milliseconds 30000.0
        }

    requestMapRef ← liftEffect $ Ref.new Map.empty

    actionsEnabled ←
      map isRight
        $ liftAff
        $ LP.post uri  (LP.InSession session : LP.Actions : Nil)
        $ LT.encodeActionRequest $ FO.singleton "action-test"
        $ LT.NoSource [ LT.pause $ Milliseconds 0.0 ]

    let
      input =
        { timeoutsRef
        , requestMapRef
        , uri
        , session
        , capabilities
        , actionsEnabled
        }

    pure $ interpret input

interpret
  ∷ ∀ r
  . HandleLunaparkInput
  → Lunapark r
  ~> R.Run (AFF' + EFFECT' + EXCEPT' + r)
interpret input = runLunapark input <<< runLunaparkActions input

runLunapark
  ∷ ∀ r
  . HandleLunaparkInput
  → R.Run (AFF' + EFFECT' + EXCEPT' + LUNAPARK' + r)
  ~> R.Run (AFF' + EFFECT' + EXCEPT' + r)
runLunapark input = do
  R.interpretRec (R.on _lunapark (handleLunapark input) R.send)

runLunaparkActions
  ∷ ∀ r
  . HandleLunaparkInput
  → R.Run (AFF' + EXCEPT' + LUNAPARK' + LUNAPARK_ACTIONS' + r)
  ~> R.Run (AFF' + EXCEPT' + LUNAPARK' + r)
runLunaparkActions input
  | input.actionsEnabled = interpretW3CActions Nil
  | otherwise = R.interpretRec (R.on _lunaparkActions (jsonWireActions input) R.send)

interpretW3CActions
  ∷ ∀ r
  . List LT.ActionSequence
  → R.Run (LUNAPARK' + LUNAPARK_ACTIONS' + r)
  ~> R.Run (LUNAPARK' + r)
interpretW3CActions acc as = case R.peel as of
  Left la → case tag la of
    Left a → w3cActions acc interpretW3CActions a
    Right others → do
      T.for_ (L.reverse acc) \s → performActions $ FO.singleton "dummy" s
      cont ← R.send others
      interpretW3CActions Nil cont
  Right a → pure a
  where
  tag = R.on _lunaparkActions Left Right

w3cActions
  ∷ ∀ f g a
  . List LT.ActionSequence
  → (List LT.ActionSequence → f ~> g)
  → ActionF (f a)
  → g a
w3cActions acc loop = case _ of
  Click btn next →
    let seq = [ LT.pointerDown btn, LT.pointerUp btn ]
    in loop (inMouse seq) next
  ButtonDown btn next →
    let seq = [ LT.pointerDown btn ]
    in loop (inMouse seq) next
  ButtonUp btn next →
    let seq = [ LT.pointerUp btn ]
    in loop (inMouse seq) next
  DoubleClick btn next →
    let seq = [ LT.pointerDown btn, LT.pointerUp btn, LT.pointerDown btn, LT.pointerUp btn ]
    in loop (inMouse seq) next
  MoveTo move next →
    let seq = [ LT.pointerMove move ]
    in loop (inPointer seq) next
  SendKeys txt next →
    let seq = T.foldMap (\ch → [ LT.keyDown ch, LT.keyUp ch ]) $ Str.toCharArray txt
    in loop (inKeys seq) next
  Pause ms next →
    let seq = [ LT.pause ms ]
    in loop (anywhere seq) next
  InTouch tch → case tch of
    Tap next →
      let seq = [ LT.pointerDown LT.LeftBtn, LT.pointerUp LT.LeftBtn ]
      in loop (inTouch seq) next
    TouchDown next →
      let seq = [ LT.pointerDown LT.LeftBtn ]
      in loop (inTouch seq) next
    TouchUp next →
      let seq = [ LT.pointerUp LT.LeftBtn ]
      in loop (inTouch seq) next
    LongClick next →
      let seq = [ LT.pointerDown LT.LeftBtn, LT.pause (Milliseconds 3000.0), LT.pointerUp LT.LeftBtn ]
      in loop (inTouch seq) next
    Flick move next →
      let seq = [ LT.pointerMove move ]
      in loop (inTouch seq) next
    Scroll move next →
      let seq = [ LT.pointerMove move ]
      in loop (inTouch seq) next
    DoubleTap next →
      let seq = [ LT.pointerDown LT.LeftBtn, LT.pointerUp LT.LeftBtn, LT.pointerDown LT.LeftBtn, LT.pointerUp LT.LeftBtn ]
      in loop (inTouch seq) next
  where
  unconsed = L.uncons acc

  inMouse seq = case unconsed of
    Just { head: (LT.Pointer LT.Mouse as), tail } →
      LT.Pointer LT.Mouse (as <> seq) : tail
    _ → LT.Pointer LT.Mouse seq : acc

  inPointer seq = case unconsed of
    Just { head: (LT.Pointer ptr as), tail } →
      LT.Pointer ptr (as <> seq) : tail
    _ → LT.Pointer LT.Mouse seq : acc

  inKeys seq = case unconsed of
    Just { head: LT.Key as, tail } →
      LT.Key (as <> seq) : tail
    _ → LT.Key seq : acc

  inTouch seq = case unconsed of
    Just { head: (LT.Pointer LT.Touch as), tail } →
      LT.Pointer LT.Touch (as <> seq) : tail
    _ → LT.Pointer LT.Mouse seq : acc

  anywhere ∷ Array (V.Variant (pause ∷ Milliseconds)) → L.List LT.ActionSequence
  anywhere seq = case unconsed of
    Nothing → L.singleton $ LT.NoSource seq
    Just { head, tail } → case head of
      LT.Pointer ptr as → LT.Pointer ptr (as <> map V.expand seq) : tail
      LT.Key as → LT.Key (as <> map V.expand seq) : tail
      LT.NoSource as → LT.NoSource (as <> seq) : tail

type HandleLunaparkInput =
  { session ∷ LT.SessionId
  , timeoutsRef ∷ Ref.Ref LT.Timeouts
  , requestMapRef ∷ Ref.Ref (Map.Map String Int)
  , uri ∷ String
  , capabilities ∷ Array LT.Capability
  , actionsEnabled ∷ Boolean
  }

jsonWireActions
  ∷ ∀ r
  . HandleLunaparkInput
  → ActionF
  ~> R.Run (AFF' + EXCEPT' + LUNAPARK' + r)
jsonWireActions inp = case _ of
  Click btn next → do
    _ ← post inp (LP.Click : Nil) (LT.encodeButton btn)
    pure next
  ButtonDown btn next → do
    _ ← post inp (LP.ButtonDown : Nil) (LT.encodeButton btn)
    pure next
  ButtonUp btn next → do
    _ ← post inp (LP.ButtonUp : Nil) (LT.encodeButton btn)
    pure next
  DoubleClick btn next → do
    _ ← case btn of
      LT.LeftBtn → post_ inp (LP.DoubleClick : Nil)
      other → do
        _ ← post inp (LP.Click : Nil) (LT.encodeButton btn)
        post inp (LP.Click : Nil) (LT.encodeButton btn)
    pure next
  SendKeys txt next → do
    _ ← post inp (LP.Keys : Nil) (LT.encodeSendKeysRequest txt)
    pure next
  MoveTo move next → do
    element ← case move.origin of
      LT.FromViewport → map Just $ findElement $ LT.ByTagName "body"
      LT.FromPointer → pure Nothing
      LT.FromElement el → pure $ Just el
    let req = { xoffset: move.x, yoffset: move.y, element }
    _ ← post inp (LP.MoveTo : Nil) (LT.encodeMoveToRequest req)
    pure next
  Pause ms next → do
    R.liftAff $ Aff.delay ms
    pure next
  InTouch tch → case tch of
    Tap next → do
      _ ← post_ inp (LP.Touch : LP.Click : Nil)
      pure next
    TouchDown next → do
      _ ← post_ inp (LP.Touch : LP.Down : Nil)
      pure next
    TouchUp next → do
      _ ← post_ inp (LP.Touch : LP.Up : Nil)
      pure next
    LongClick next → do
      _ ← post_ inp (LP.Touch : LP.LongClick : Nil)
      pure next
    Flick move next → do
      element ← case move.origin of
        LT.FromViewport → map Just $ findElement $ LT.ByTagName "body"
        LT.FromPointer → pure Nothing
        LT.FromElement el → pure $ Just el
      let req = { xoffset: move.x, yoffset: move.y, element }
      _ ← post inp (LP.Touch : LP.Flick : Nil) (LT.encodeMoveToRequest req)
      pure next
    Scroll move next → do
      element ← case move.origin of
        LT.FromViewport → map Just $ findElement $ LT.ByTagName "body"
        LT.FromPointer → pure Nothing
        LT.FromElement el → pure $ Just el
      let req = { xoffset: move.x, yoffset: move.y, element }
      _ ← post inp (LP.Touch : LP.Scroll : Nil) (LT.encodeMoveToRequest req)
      pure next
    DoubleTap next → do
      _ ← post_ inp (LP.Touch : LP.DoubleClick : Nil)
      pure next

handleLunapark
  ∷ ∀ r
  . HandleLunaparkInput
  → LunaparkF
  ~> R.Run (AFF' + EFFECT' + EXCEPT' + r)
handleLunapark inp = case _ of
  Quit next → do
    _ ← delete inp Nil
    pure next
  Status cont → do
    res ← liftAndRethrow $ LP.get inp.uri $ LP.Status : Nil
    ss ← throwLeft $ LT.decodeServerStatus res
    pure $ cont ss
  GetTimeouts cont → do
    res ← R.liftEffect $ Ref.read inp.timeoutsRef
    pure $ cont res
  SetTimeouts ts next → do
    R.liftEffect $ Ref.write ts inp.timeoutsRef
    tryAndCache inp "set timeouts"
      [ void $ post inp (LP.Timeouts : Nil) (LT.encodeTimeouts ts)
      , do T.for_ (LT.encodeLegacyTimeouts ts) \j →
           void $ post inp (LP.Timeouts : Nil) j
      ]
    pure next
  GoTo uri next → do
    _ ← post inp (LP.Url : Nil) $ LT.encodeGoRequest uri
    pure next
  GetUrl cont → do
    res ← get inp (LP.Url : Nil)
    map cont $ throwLeft $ J.decodeJson res
  Back next → do
    _ ← post_ inp (LP.Back : Nil)
    pure next
  Forward next → do
    _ ← post_ inp (LP.Forward : Nil)
    pure next
  Refresh next → do
    _ ← post_ inp (LP.Refresh : Nil)
    pure next
  GetTitle cont → do
    res ← get inp (LP.Title : Nil)
    map cont $ throwLeft $ J.decodeJson res
  GetWindowHandle cont → do
    res ← tryAndCache inp "get window handle"
      [ get inp (LP.Window : Nil)
      , get inp (LP.WindowHandle : Nil)
      ]
    map cont $ throwLeft $ LT.decodeWindowHandle res
  GetWindowHandles cont → do
    res ← tryAndCache inp "get window handles"
      [ get inp (LP.Window : LP.Handles : Nil)
      , get inp (LP.WindowHandles : Nil)
      ]
    map cont $ throwLeft $ T.traverse LT.decodeWindowHandle =<< J.decodeJson res
  CloseWindow next → do
    _ ← delete inp (LP.Window : Nil)
    pure next
  SwitchToWindow w next → do
    _ ← post inp (LP.Window : Nil) (LT.encodeSwitchToWindowRequest w)
    pure next
  SwitchToFrame fid next → do
    _ ← post inp (LP.Frame : Nil) (LT.encodeFrameId fid)
    pure next
  SwitchToParentFrame next → do
    _ ← post_ inp (LP.Frame : LP.Parent : Nil)
    pure next
  GetWindowRectangle cont → do
    res ← tryAndCache inp "get window rectangle"
      [ do res ← get inp (LP.Window : LP.Rect : Nil)
           throwLeft $ LT.decodeRectangle res
      , do position ← get inp (LP.Window : LP.Position : Nil)
           size ← get inp (LP.Window : LP.Size : Nil)
           throwLeft $ LT.decodeRectangleLegacy { position, size }
      ]
    pure $ cont res
  SetWindowRectangle r next → do
    tryAndCache inp "set window rectangle"
      [ void $ post inp (LP.Window : LP.Rect : Nil) (LT.encodeRectangle r)
      , do let js = LT.encodeRectangleLegacy r
           _ ← post inp (LP.Window : LP.Size : Nil) js.size
           void $ post inp (LP.Window : LP.Position : Nil) js.position
      ]
    pure next
  MaximizeWindow next → do
    _ ← post_ inp (LP.Window : LP.Maximize : Nil)
    pure next
  MinimizeWindow next → do
    _ ← post_ inp (LP.Window : LP.Minimize : Nil)
    pure next
  FullscreenWindow next → do
    _ ← post_ inp (LP.Window : LP.Fullscreen : Nil)
    pure next
  ExecuteScript script cont → do
    map cont $ tryAndCache inp "execute script"
      [ post inp (LP.Execute : LP.Sync : Nil) (LT.encodeScript script)
      , post inp (LP.Execute : Nil) (LT.encodeScript script)
      ]
  ExecuteScriptAsync script cont → do
    map cont $ tryAndCache inp "execute script async"
      [ post inp (LP.Execute : LP.Async : Nil) (LT.encodeScript script)
      , post inp (LP.ExecuteAsync : Nil) (LT.encodeScript script)
      ]
  GetAllCookies cont → do
    res ← get inp (LP.Cookies : Nil)
    map cont $ throwLeft $ T.traverse LT.decodeCookie =<< J.decodeJson res
  GetCookie name cont → do
    res ← get inp (LP.Cookie name : Nil)
    map cont $ throwLeft $ LT.decodeCookie res
  DeleteAllCookies next → do
    _ ← delete inp (LP.Cookies : Nil)
    pure next
  DeleteCookie name next → do
    _ ← delete inp (LP.Cookie name : Nil)
    pure next
  AddCookie cookie next → do
    _ ← post inp (LP.Cookies : Nil) (LT.encodeCookie cookie)
    pure next
  DismissAlert next → do
    _ ← tryAndCache inp "dismiss alert"
      [ post_ inp (LP.Alert : LP.Dismiss : Nil)
      , post_ inp (LP.DismissAlert : Nil)
      ]
    pure next
  AcceptAlert next → do
    _ ← tryAndCache inp "accept alert"
      [ post_ inp (LP.Alert : LP.Accept : Nil)
      , post_ inp (LP.AcceptAlert : Nil)
      ]
    pure next
  GetAlertText cont → do
    res ← tryAndCache inp "get alert text"
      [ get inp (LP.Alert : LP.Text : Nil)
      , get inp (LP.AlertText : Nil)
      ]
    map cont $ throwLeft $ J.decodeJson res
  SendAlertText str next → do
    _ ← tryAndCache inp "send alert text"
      [ post inp (LP.Alert : LP.Text : Nil) (LT.encodeSendKeysRequest str)
      , post inp (LP.AlertText : Nil) (LT.encodeSendKeysRequest str)
      ]
    pure next
  Screenshot fp next → do
    res ← get inp (LP.Screenshot : Nil)
    screenshotPack ← throwLeft $ LT.decodeScreenshot res
    buffer ← R.liftEffect $ B.fromString screenshotPack.content screenshotPack.encoding
    R.liftAff $ FS.writeFile fp buffer
    pure next
  FindElement loc cont → do
    res ← post inp (LP.Element : Nil) (LT.encodeLocator loc)
    map cont $ throwLeft $ LT.decodeElement res
  FindElements loc cont → do
    res ← post inp (LP.Elements : Nil) (LT.encodeLocator loc)
    map cont $ throwLeft $ T.traverse LT.decodeElement =<< J.decodeJson res
  GetActiveElement cont → do
    res ← get inp (LP.Element : LP.Active : Nil)
    map cont $ throwLeft $ LT.decodeElement res
  PerformActions req next → do
    when inp.actionsEnabled do
      void $ post inp (LP.Actions : Nil) (LT.encodeActionRequest req)
    pure next
  ReleaseActions next → do
    when inp.actionsEnabled $ void $ delete inp (LP.Actions : Nil)
    pure next
  OnElement el elF →
    let inElement = LP.InElement el
    in case elF of
      ChildElement loc cont → do
        res ← post inp (inElement : LP.Element : Nil) (LT.encodeLocator loc)
        map cont $ throwLeft $ LT.decodeElement res
      ChildElements loc cont → do
        res ← post inp (inElement : LP.Elements : Nil) (LT.encodeLocator loc)
        map cont $ throwLeft $ T.traverse LT.decodeElement =<< J.decodeJson res
      ScreenshotEl fp next → do
        res ← get inp (inElement : LP.Screenshot : Nil)
        screenshotPack ← throwLeft $ LT.decodeScreenshot res
        buffer ← R.liftEffect $ B.fromString screenshotPack.content screenshotPack.encoding
        R.liftAff $ FS.writeFile fp buffer
        pure next
      IsSelected cont → do
        res ← get inp (inElement : LP.Selected : Nil)
        map cont $ throwLeft $ J.decodeJson res
      GetAttribute attr cont → do
        res ← get inp (inElement : LP.Attribute attr : Nil)
        map cont $ throwLeft $ J.decodeJson res
      GetProperty prop cont → do
        map cont $ get inp (inElement : LP.Property prop : Nil)
      GetCss css cont → do
        res ← get inp (inElement : LP.CssValue css : Nil)
        map cont $ throwLeft $ J.decodeJson res
      GetText cont → do
        res ← get inp (inElement : LP.Text : Nil)
        map cont $ throwLeft $ J.decodeJson res
      GetTagName cont → do
        res ← get inp (inElement : LP.Name : Nil)
        map cont $ throwLeft $ J.decodeJson res
      GetRectangle cont →
        map cont $ tryAndCache inp "get element rectangle"
          [ do res ← get inp (inElement : LP.Rect : Nil)
               throwLeft $ LT.decodeRectangle res
          , do position ← get inp (inElement : LP.Position : Nil)
               size ← get inp (inElement : LP.Size : Nil)
               throwLeft $ LT.decodeRectangleLegacy { position, size }
          ]
      IsEnabled cont → do
        res ← get inp (inElement : LP.Enabled : Nil)
        map cont $ throwLeft $ J.decodeJson res
      ClickEl next → do
        _ ← post_ inp (inElement : LP.Click : Nil)
        pure next
      ClearEl next → do
        _ ← post_ inp (inElement : LP.Clear : Nil)
        pure next
      SendKeysEl txt next → do
        _ ← tryAndCache inp "send keys chromedriver hack"
          [ post inp (inElement : LP.Value : Nil) (LT.encodeSendKeysRequest txt)
          , post inp (inElement : LP.Value : Nil)
              $ J.encodeJson $ FO.singleton "value" $ Str.toCharArray txt
          ]
        pure next
      IsDisplayed cont → do
        res ← tryAndCache inp "is element displayed"
          [ get inp (inElement : LP.Displayed : Nil)
          , do let script =
                     { script: """var el = arguments[0]; return el.offsetHeight > 0 && el.offsetWidth > 0"""
                     , args: [ LT.encodeElement el ]
                     }
               handleLunapark inp $ ExecuteScript script identity
          ]
        map cont $ throwLeft $ J.decodeJson res
      Submit next → do
        _ ← post_ inp (inElement : LP.Submit : Nil)
        pure next

tryAndCache
  ∷ ∀ r a
  . HandleLunaparkInput
  → String
  → Array (R.Run (EFFECT' + EXCEPT' + r) a)
  → R.Run (EFFECT' + EXCEPT' + r) a
tryAndCache inp key actions = do
  let emptyCases = throwLeft $ Left $ "No valid cases for " <> key <> " caching"
  let incorrectCache = throwLeft $ Left $ "Fallback for " <> key <> " error"

  mp ← R.liftEffect $ Ref.read inp.requestMapRef
  case Map.lookup key mp of
    Just ix → case A.index actions ix of
      Just action → action
      Nothing → incorrectCache
    Nothing →
      let
        go ix acc act =
          let try' = do
                a ← act
                R.liftEffect $ Ref.modify_ (Map.insert key ix) inp.requestMapRef
                pure a
          in catch try' \_ → acc
      in
       FI.foldlWithIndex go emptyCases actions

get
  ∷ ∀ r
  . HandleLunaparkInput
  → List LP.EndpointPart
  → R.Run (AFF' + EXCEPT' + r) J.Json
get inp ep = liftAndRethrow $ LP.get inp.uri (LP.InSession inp.session : ep)

post
  ∷ ∀ r
  . HandleLunaparkInput
  → List LP.EndpointPart
  → J.Json
  → R.Run (AFF' + EXCEPT' + r) J.Json
post inp ep json = liftAndRethrow $ LP.post inp.uri (LP.InSession inp.session : ep) json

post_
  ∷ ∀ r
  . HandleLunaparkInput
  → List LP.EndpointPart
  → R.Run (AFF' + EXCEPT' + r) J.Json
post_ inp ep = liftAndRethrow $ LP.post_ inp.uri (LP.InSession inp.session : ep)

delete
  ∷ ∀ r
  . HandleLunaparkInput
  → List LP.EndpointPart
  → R.Run (AFF' + EXCEPT' + r) J.Json
delete inp ep = liftAndRethrow $ LP.delete inp.uri (LP.InSession inp.session : ep)
