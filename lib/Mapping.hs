{-# LANGUAGE MultiParamTypeClasses #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Mapping
-- Copyright   :  (c) 2019 Tiago de Paula, João Pedro de Amorim
-- License     :  MIT
--
-- Definição e operações de mapeamento.
--
-----------------------------------------------------------------------------

module Mapping (Mapping(..), (!)) where


import Prelude hiding (foldMap, lookup, toList)

import Data.Maybe  (fromMaybe, isJust)
import Data.Monoid (getAlt)
import Control.Applicative ((<|>))


-- | Composição de uma função com um operador binário,
-- aplicando a função no resultado da operação.
(.:) :: (a -> b) -> (c -> d -> a) -> c -> d -> b
f .: g = (f .) . g


-- | Estruturas de mapeamentos.
--
-- Servem como hom-sets generalizados para categorias finitas
-- e podem ser usados como tabelas de dados.
--
-- A ordem dos dados não é significativa.
class Eq k => Mapping m k v where
    {-# MINIMAL alter, (empty | fromList), (fold | toList) #-}

    -- | Constrói um mapeamento vazio.
    --
    -- Equivalente a @'fromList' []@
    empty :: m k v
    empty = fromList []

    -- | Constrói um mapeamento a partir de uma lista
    -- de chaves e valores.
    --
    -- Desse modo @'fromList' . 'toList'@ funciona como
    -- um morfismo de um tipo de mapeamento para seu
    -- equivalente em outro tipo de mapeamento.
    fromList :: [(k, v)] -> m k v
    fromList = foldl (flip $ uncurry insert) empty

    -- | Reduz as chaves e os valores do mapeamento em
    -- um resultado.
    --
    -- A ordem não é importante.
    fold :: (k -> v -> b -> b) -> b -> m k v -> b
    fold f ini = foldr (uncurry f) ini . toList

    -- | Gera uma lista com as chaves e os valores.
    --
    -- Note que, por que a ordem não é importante, NÃO
    -- é necessário que @'toList' = 'zip' . 'keys' '<*>' 'elems'@
    toList :: m k v -> [(k, v)]
    toList = fold ((:) .: curry id) []

    -- | @'foldMap'@ é como o @'fold'@, reduzindo o mapeamento em uma valor,
    -- mas a lógica de associação dos valores é pelo operador binário do monóide,
    -- iniciando com seu elemento vazio.
    foldMap :: Monoid n => (k -> v -> n) -> m k v -> n
    foldMap f = fold (mappend .: f) mempty

    -- | Busca de um valor por uma chave.
    --
    -- ==== __Exemplos__
    --
    -- >>> lookup "Tistão" $ fromList [("Tistão", 7.3), ("Marmita", 5.6)] == Just 7.3
    -- True
    -- >>> lookup "Tunico" $ fromList [("Tistão", 7.3), ("Marmita", 5.6)] == Nothing
    -- True
    lookup :: k -> m k v -> Maybe v
    lookup key = getAlt . foldMap (\x val -> if x == key then pure val else mempty)

    -- | Lista com todas as chaves presentes no mapeamento.
    --
    -- ==== __Exemplos__
    --
    -- >>> keys $ fromList [("Tistão", 7.3), ("Marmita", 5.6)]
    -- ["Marmita", "Tistão"]
    keys :: m k v -> [k]
    keys = map fst . toList

    -- | Lista com todos os valores presentes no mapeamento.
    --
    -- ==== __Exemplos__
    --
    -- >>> keys $ fromList [("Tistão", 7.3), ("Marmita", 5.6)]
    -- [7.3, 5.6]
    elems :: m k v -> [v]
    elems = map snd . toList

    -- | Testa se a chave está presente no mapeamento.
    member :: k -> m k v -> Bool
    member = isJust .: lookup

    -- | Tamanho do mapeamento, isto é, quantidade de chaves
    -- e valores presentes nele.
    size :: m k v -> Int
    size = fold (\_ _ x -> x + 1) 0

    -- | Testa se o mapeamento está vazio.
    null :: m k v -> Bool
    null = (0 ==) . size

    -- | Essa é a principal função de alteração do mapeamento.
    --
    -- Se a chave estiver presente no mapeamento, o valor
    -- relacionado a ela será passado dentro de um @'Just'@
    -- para a função, passando um @'Nothing'@, caso contrário.
    --
    -- Independente do que ela receber, o resultado dela define
    -- a alteração do mapeamento. Se o resultado for @'Nothing'@,
    -- a chave é desassociada de um valor, independentemente se
    -- estava associada ou não. Se retornar algum valor, então
    -- este valor é associado com a chave, mesmo se nenhum valor
    -- estava associado anteriormente.
    alter :: (Maybe v -> Maybe v) -> k -> m k v -> m k v

    -- | Associa um valor a uma chave. Se a chave já estava
    -- associada a um valor, então o valor é alterado.
    insert :: k -> v -> m k v -> m k v
    insert = flip (alter . const . Just)

    -- | Altera o valor associado a chave com um morfismo,
    -- se a chave estiver presente no mapeamento.
    adjust :: (v -> v) -> k -> m k v -> m k v
    adjust = alter . fmap

    -- | Altera o valor como a função @'adjust'@, mas com
    -- possibilidade de remoção, caso retorne @'Nothing'@
    update :: (v -> Maybe v) -> k -> m k v -> m k v
    update = alter . (=<<)

    -- | Desassocia a chave no mapeamento, se ela estiver associada.
    delete :: k -> m k v -> m k v
    delete = alter (const Nothing)


-- | Acesso no mapeamento pela chave. Causa erro se a
-- chave não existir.
--
-- ==== __Exemplos__
--
-- >>> fromList [('a', 1), ('b', 2)] ! 'a'
-- 1
-- >>> empty ! 'a'
-- *** Exception: key not associated to any value
(!) :: (Mapping m k v) => m k v -> k -> v
map ! key = fromMaybe (error "key not associated to any value") $ lookup key map
