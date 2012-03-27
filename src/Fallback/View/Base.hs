{- ============================================================================
| Copyright 2011 Matthew D. Steele <mdsteele@alum.mit.edu>                    |
|                                                                             |
| This file is part of Fallback.                                              |
|                                                                             |
| Fallback is free software: you can redistribute it and/or modify it under   |
| the terms of the GNU General Public License as published by the Free        |
| Software Foundation, either version 3 of the License, or (at your option)   |
| any later version.                                                          |
|                                                                             |
| Fallback is distributed in the hope that it will be useful, but WITHOUT     |
| ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or       |
| FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for   |
| more details.                                                               |
|                                                                             |
| You should have received a copy of the GNU General Public License along     |
| with Fallback.  If not, see <http://www.gnu.org/licenses/>.                 |
============================================================================ -}

module Fallback.View.Base
  (-- * The @View@ type
   View(..), nullView, inertView, wallView, Action(..), fromAction,
   -- * View combinators
   f2map, vmap, vmapM, viewMap, viewMapM,
   compoundView, compoundViewM, subView, subView_,
   newMaybeView, newEitherView, newHoverOnlyView,
   -- * Utility functions
   whenWithinCanvas)
where

import Control.Applicative ((<$>))
import Control.Monad (liftM, unless, when)

import Fallback.Data.Point
import Fallback.Draw
import Fallback.Event (Event(..))
import Fallback.Utility (flip3)

-------------------------------------------------------------------------------
-- The View type:

data View a b = View
  { viewPaint :: a -> Paint (),
    viewHandler :: a -> Event -> Handler (Action b) }

instance Functor (View a) where
  fmap fn (View paint handler) = View paint handler' where
    handler' input event = (fn <$>) <$> handler input event

-- | A view that never paints anything and ignores all events.
nullView :: View a b
nullView = inertView $ const $ return ()

-- | Given a paint function, create a view which ignores all inputs to its
-- handler.
inertView :: (a -> Paint ()) -> View a b
inertView paintFn = View paintFn $ const $ const $ return Ignore

-- | Given a paint function, create a view which ignores all inputs to its
-- handler, except that it suppresses mouse clicks within its rect (thus
-- preventing them from reaching views \"behind\" this one).
wallView :: (a -> Paint ()) -> View a b
wallView paintFn = View paintFn handler where
  handler _ (EvMouseDown pt) = do
    rect <- canvasRect
    return $ if rectContains rect pt then Suppress else Ignore
  handler _ _ = return Ignore

-------------------------------------------------------------------------------
-- The Action type:

data Action a = Ignore | Suppress | Action a

instance Functor Action where
  fmap _ Ignore = Ignore
  fmap _ Suppress = Suppress
  fmap fn (Action a) = Action (fn a)

fromAction :: Action a -> Maybe a
fromAction (Action a) = Just a
fromAction _ = Nothing

-------------------------------------------------------------------------------
-- View combinators:

-- | Like 'fmap' for views, but allows the output value of the resulting view
-- to depend on the input value, as well as the output value, of the original
-- view.
f2map :: (a -> b -> c) -> View a b -> View a c
f2map fn (View paint handler) = View paint handler' where
  handler' input event = (fn input <$>) <$> handler input event

-- | Like 'fmap' for views, but rather than transforming the output value of
-- the original view to that of the resulting view, it transforms the input
-- value of the resulting view to that of the original view.
vmap :: (a -> c) -> View c b -> View a b
vmap fn (View paint handler) = View paint' handler' where
  paint' = paint . fn
  handler' input event = handler (fn input) event

-- | A variant of 'vmap' that allows a monadic action.
vmapM :: (a -> Draw c) -> View c b -> View a b
vmapM fn (View paint handler) = View paint' handler' where
  paint' input = runDraw (fn input) >>= paint
  handler' input event = do
    input' <- runDraw (fn input)
    handler input' event

-- | A combination of 'vmap' and 'fmap'.
viewMap :: (a -> c) -> (d -> b) -> View c d -> View a b
viewMap f1 f2 (View paint handler) = View paint' handler' where
  paint' = paint . f1
  handler' input event = (f2 <$>) <$> handler (f1 input) event

-- | A variant of 'viewMap' that allows monadic actions.
viewMapM :: (a -> Draw c) -> (d -> Draw (Action b)) -> View c d
         -> View a b
viewMapM f1 f2 (View paint handler) = View paint' handler' where
  paint' input = runDraw (f1 input) >>= paint
  handler' input event = do
    input' <- runDraw (f1 input)
    action <- handler input' event
    case action of
      Ignore -> return Ignore
      Suppress -> return Suppress
      Action value -> runDraw (f2 value)

-- | Layer multiple views on top of one another.  The first view in the list
-- will be the rearmost view; the last view in the list will be the frontmost.
-- Events will be passed to all views, and the resulting action will be that of
-- the first view, from front to back, to return a non-'Ignore' action.
compoundView :: [View a b] -> View a b
compoundView views = View paintFn handlerFn where
  paintFn input = mapM_ (flip viewPaint input) views
  handlerFn input event = do
    let firstAction [] = Ignore
        firstAction (Ignore : xs) = firstAction xs
        firstAction (x : _) = x
    firstAction <$> mapM (flip3 viewHandler input event) (reverse views)

compoundViewM :: (Monad m) => [m (View a b)] -> m (View a b)
compoundViewM = liftM compoundView . sequence

subView :: (a -> (Int, Int) -> IRect) -> View a b -> View a b
subView rectFn view = View paintFn handlerFn where
  paintFn input = do size <- canvasSize
                     withSubCanvas (rectFn input size) (viewPaint view input)
  handlerFn input event = do
    size <- canvasSize
    let subRect = rectFn input size
    withSubCanvas subRect $ do
      viewHandler view input $ translateEvent subRect event

subView_ :: IRect -> View a b -> View a b
subView_ = subView . const . const

-- sizedView :: (a -> (Int, Int) -> c) -> View c b -> View a b
-- sizedView fn (View paint handler) = View paint' handler' where
--   paint' input = paint . fn input =<< canvasSize
--   handler' input rect event = handler (fn input (rectSize rect)) rect event

-- maybeView :: (a -> Maybe c) -> View c b -> View a b
-- maybeView fn (View paint handler) = View paint' handler' where
--   paint' input = maybe (return ()) paint (fn input)
--   handler' input rect event =
--     maybe (return Ignore) (flip3 handler rect event) (fn input)

newMaybeView :: (MonadDraw m) => (a -> Maybe c) -> View c b -> m (View a b)
newMaybeView fn (View paint handler) = do
  visibleRef <- newDrawRef False
  let
    paint' input =
      maybe (return ()) paint =<< runHandler (transform input)
    handler' input event =
      maybe (return Ignore) (flip handler event) =<< transform input
    transform input = do
      case fn input of
        Nothing -> do
          writeDrawRef visibleRef False
          return Nothing
        Just input' -> do
          visible <- readDrawRef visibleRef
          unless visible $ do
            _ <- handler input' EvBlur
            writeDrawRef visibleRef True
          return (Just input')
  return (View paint' handler')

newEitherView :: (MonadDraw m) => (a -> Either c d) -> View c b -> View d b
              -> m (View a b)
newEitherView fn v1 v2 = do
  v1' <- newMaybeView (\a -> case fn a of Left c -> Just c
                                          Right _ -> Nothing) v1
  v2' <- newMaybeView (\a -> case fn a of Left _ -> Nothing
                                          Right d -> Just d) v2
  return $ compoundView [v1', v2']

newHoverOnlyView :: (MonadDraw m) => View a b -> m (View a b)
newHoverOnlyView (View paint handler) = do
  visibleRef <- newDrawRef False
  let
    paint' input = do
      hover <- isMouseWithinCanvas
      when hover $ paint input
    handler' input event = do
      hover <- isMouseWithinCanvas
      if hover then do
        writeDrawRef visibleRef True
        handler input event
       else do
        visible <- readDrawRef visibleRef
        when visible $ do
          _ <- handler input EvBlur
          writeDrawRef visibleRef False
        return Ignore
  return (View paint' handler')

-------------------------------------------------------------------------------

whenWithinCanvas :: IPoint -> Handler (Action a) -> Handler (Action a)
whenWithinCanvas pt action = do
  rect <- canvasRect
  if rectContains rect pt then action else return Ignore

-------------------------------------------------------------------------------
-- Private utility functions:

translateEvent :: IRect -> Event -> Event
translateEvent rect event =
  case event of
    EvMouseMotion pt rel -> EvMouseMotion (translate pt) rel
    EvMouseUp pt -> EvMouseUp (translate pt)
    EvMouseDown pt -> EvMouseDown (translate pt)
    EvScrollDownwards pt -> EvScrollDownwards (translate pt)
    EvScrollUpwards pt -> EvScrollUpwards (translate pt)
    _ -> event
  where translate pt = pt `pSub` rectTopleft rect

-------------------------------------------------------------------------------
