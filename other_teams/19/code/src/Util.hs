module Util where

import Control.Monad
import Data.Set (Set)

import qualified Data.Set as Set

infixr 0 *$
infixr 0 ^$
infixr 9 *.

(*$) :: (Monad m) => (a -> m b) -> m a -> m b
(*$) = (=<<)

(^$) :: (Monad m) => (a -> b) -> m a -> m b
(^$) = liftM

(*.) :: (Monad m) => (b -> m c) -> (a -> m b) -> (a -> m c)
(*.) = (<=<)

data P a = P

maybeToSet :: (Ord a) => Maybe a -> Set a
maybeToSet (Just a) = Set.singleton a
maybeToSet Nothing = Set.empty

setExtend :: (Ord b) => (a -> Set b) -> Set a -> Set b
setExtend f aM = Set.foldl' (\ r a -> Set.union (f a) r) Set.empty aM

(.:) :: (c -> d) -> (a -> b -> c) -> (a -> b -> d)
(.:) = (.) . (.)

applyTo :: a -> (a -> b) -> b
applyTo = flip ($)

zipLongest :: [a] -> [a] -> [(Maybe a, Maybe a)]
zipLongest [] [] = []
zipLongest [] (y:ys) = (Nothing, Just y) : zipLongest [] ys
zipLongest (x:xs) [] = (Just x, Nothing) : zipLongest xs []
zipLongest (x:xs) (y:ys) = (Just x, Just y) : zipLongest xs ys

maybeM :: (Monad m) => m b -> (a -> m b) -> Maybe a -> m b
maybeM bM _ Nothing = bM
maybeM _ f (Just a) = f a

maybeMOn :: (Monad m) => Maybe a -> m b -> (a -> m b) -> m b
maybeMOn aM bM f = maybeM bM f aM

whenNothing :: (Monad m) => m a -> Maybe a -> m a
whenNothing aM = maybe aM return

maybeZero :: (MonadPlus m) => Maybe a -> m a
maybeZero = whenNothing mzero

whenJust :: (Monad m) => Maybe a -> (a -> m ()) -> m ()
whenJust aM f = maybeM (return ()) f aM

whenLeft :: (Monad m) => (a -> m b) -> Either a b -> m b
whenLeft f = either f return
