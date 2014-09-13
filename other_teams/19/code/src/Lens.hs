module Lens where

import Util

import Control.Category
import Control.Monad
import Control.Monad.State
import Prelude hiding (id, (.))

data Cursor a b = Cursor { focus :: a, construct :: a -> b }

data Lens a b = Lens { runLens :: a -> Cursor b a }

lens :: (a -> b) -> (a -> b -> a) -> Lens a b
lens getter setter = Lens $ \ s -> Cursor (getter s) (setter s)

isoLens :: (a -> b) -> (b -> a) -> Lens a b
isoLens to from = lens to $ const from

instance Category Lens where
  id = Lens $ \ a -> Cursor a id
  g . f = Lens $ \ a -> 
    let Cursor b ba = runLens f a
        Cursor c cb = runLens g b
    in Cursor c $ ba . cb

access :: Lens a b -> a -> b
access = focus .: runLens

update :: Lens a b -> (b -> b) -> a -> a
update l f a =
  let Cursor b ba = runLens l a
  in ba $ f b
(~:) :: Lens a b -> (b -> b) -> a -> a
(~:) = update

udpateM :: (Monad m) => Lens a b -> (b -> m b) -> a -> m a
udpateM l f a =
  let Cursor b ba = runLens l a
  in liftM ba $ f b

set :: Lens a b -> b -> a -> a
set l = update l . const
(=:) :: Lens a b -> b -> a -> a
(=:) = set

(|:) :: a -> (a -> a) -> a
(|:) = applyTo

-- state stuff

getL :: (MonadState s m) => Lens s a -> m a
getL l = liftM (access l) get

putL :: (MonadState s m) => Lens s a -> a -> m ()
putL l a = modify $ set l a

modifyL :: (MonadState s m) => Lens s a -> (a -> a) -> m ()
modifyL l f = modify $ update l f

nextL :: (MonadState s m, Num a) => Lens s a -> m a
nextL l = do
  i <- getL l
  putL l $ i + 1
  return i

