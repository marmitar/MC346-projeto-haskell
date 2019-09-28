-----------------------------------------------------------------------------
-- |
-- Module      :  Point
-- Copyright   :  (c) 2019 Tiago de Paula & João Pedro de Amorim
-- License     :  MIT
--
-- Definição de um ponto multidimensional.
--
-----------------------------------------------------------------------------

module Point where

import Data.Foldable     (toList)
import Control.Monad.Zip (MonadZip, mzipWith)



-- | Um ponto, que é basicamente uma lista de valores do mesmo tipo.
newtype Point a = Point [a]
    deriving (Eq, Show)


-- | Cálculo da distância Euclidiana entre dois pontos.
distance :: Floating a => Point a -> Point a -> a
distance x y = sqrt $! sum $ square <$> mzipWith (-) x y
    where square x = x * x

instance Foldable Point where
    foldMap f (Point vals) = foldMap f vals

instance Functor Point where
    fmap f = Point . fmap f . toList

instance Traversable Point where
    traverse f p = Point <$> traverse f (toList p)

instance Applicative Point where
    pure = Point . pure
    pf <*> pv = Point $ toList pf <*> toList pv

instance Monad Point where
    ps >>= f = Point $ toList ps >>= toList . f

instance MonadZip Point where
    mzipWith f x y = Point $ zipWith f (toList x) (toList y)
