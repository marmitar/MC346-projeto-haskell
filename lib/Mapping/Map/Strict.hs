{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Mapping.Map.Strict
-- Copyright   :  (c) 2019 Tiago de Paula & João Pedro de Amorim
-- License     :  MIT
--
-- Implementação de 'Mapping' para 'Data.Map.Strict.Map'
--
-----------------------------------------------------------------------------

module Mapping.Map.Strict (Map) where

import Mapping
import Data.Ord
import Data.Map.Strict as M


instance Ord k => Mapping Map k v where

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
