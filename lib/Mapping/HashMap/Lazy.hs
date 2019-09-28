{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Mapping.HashMap.Lazy
-- Copyright   :  (c) 2019 Tiago de Paula & João Pedro de Amorim
-- License     :  MIT
--
-- Implementação de 'Mapping' para 'Data.HashMap.Lazy.HashMap'
--
-----------------------------------------------------------------------------

module Mapping.HashMap.Lazy (HashMap) where

import Mapping
import Data.Ord
import Data.Hashable
import Data.HashMap.Lazy as M


instance (Hashable k, Ord k) => Mapping HashMap k v where

    empty    = M.empty
    fromList = M.fromList

    fold   = M.foldrWithKey
    toList = M.toList

    lookup = M.lookup
    keys   = M.keys
    elems  = M.elems
    member = M.member
    size   = M.size
    null   = M.null

    alter  = M.alter
    insert = M.insert
    adjust = M.adjust
    update = M.update
    delete = M.delete
