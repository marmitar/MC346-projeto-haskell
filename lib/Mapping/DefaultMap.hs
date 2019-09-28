{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FunctionalDependencies #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Mapping.DefaultMap
-- Copyright   :  (c) 2019 Tiago de Paula & João Pedro de Amorim
-- License     :  MIT
--
-- Definição de um mapeamento com valores padrões e
-- definição de mapeamento inversível.
--
-----------------------------------------------------------------------------


module Mapping.DefaultMap (
    -- * Mapeamento Padrão
    DefaultMap,
    -- * Mapeamento Invertível
    InvertibleMapping(..)
) where

import Mapping
import Data.Default
import Data.Maybe
import Data.Foldable (foldr')
import Control.Applicative ((<|>))

import Prelude hiding (toList, foldMap, null, lookup)


-- | Envolve um mapeamento com valor padrão.
--
-- A ideia é que toda chave desassociada neste mapeamento
-- está associada a um valor padrão, definido pela classe
-- @'Default'@.
--
-- Remover uma chave e associar ela ao valor padrão são
-- operações indistinguíveis nesse mapeamento.
--
-- Com essa definição, o mapeamento passa a ser sempre
-- completo em relação ao domínio.
--
-- ==== __Exemplos__
--
-- >>> fromList [(0, ""), (1, "x")] == fromList [(1, "x")]
-- True
-- >>> size $ fromList [(2, "")]
-- 0
-- >>> lookup 10 empty
-- Just ""
-- >>> adjust (++ "hey") 3 empty
-- fromList [(3, "hey")]
data DefaultMap m k v = DefaultMap (m k v)
    deriving (Eq, Read, Show)

-- | Extrai a definição interna do mapeamento.
getMap :: DefaultMap m k v -> m k v
getMap (DefaultMap map) = map


instance (Mapping m k v, Default v, Eq v) => Mapping (DefaultMap m) k v where

    empty = DefaultMap empty
    fromList = DefaultMap . fromList . filter ((/= def) . snd)

    toList = toList . getMap
    fold f ini = fold f ini . getMap
    foldMap f = foldMap f . getMap

    keys = keys . getMap
    elems = elems . getMap
    member key = member key . getMap

    size = size . getMap
    null = null . getMap

    lookup key = (<|> Just def) . lookup key . getMap

    alter f key = DefaultMap . alter ((filterDef =<<) . f . (<|> Just def)) key . getMap
        where filterDef x
                | x == def  = Nothing
                | otherwise = Just x

    insert key val
        | val == def = delete key
        | otherwise  = DefaultMap . insert key val . getMap

    adjust f = alter (Just . f . fromMaybe def)
    update f = alter (f . fromMaybe def)
    delete key = DefaultMap . delete key . getMap


-- | Mapeamentos invertíveis são mapeamentos de @k -> v@ que
-- podem ser invertidos para @v -> f k@.
class (Mapping m k v, Mapping n v (f k), Foldable f) => InvertibleMapping m n f k v | m f -> n where
    -- | Inverte um mapeamento @m@ de @k -> v@ para um outro
    -- mapeamento @n@ de @v -> f k@.
    invert :: m k v -> n v (f k)

    -- | Reverte o mapeamento iverso.
    --
    -- Deve valer que @'id' = 'invert' . 'revert'@
    revert :: n v (f k) -> m k v
    revert = fold (flip . foldr' . flip insert) empty


instance (Mapping m k v, Mapping m v [k]) => InvertibleMapping m (DefaultMap m) [] k v where
    invert = fold (adjust . (:)) empty
