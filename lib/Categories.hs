{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE FlexibleContexts      #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Point
-- Copyright   :  (c) 2019 Tiago de Paula & João Pedro de Amorim
-- License     :  MIT
--
-- Definição de um ponto multidimensional.
--
-----------------------------------------------------------------------------

module Categories (
    StringMapping,

    -- * Mapa dos Pontos
    Points,
    parsePoints,

    -- * Pontos Categorizados
    Categorized,
    parseCategorized,

    -- * Mapa das Categorias
    Categories,
    fromCategorized,

    -- * Entrada e Saída
    parseInput,
    parseInputIO,
    parseOutput,
    parseOutputIO,

    -- * Interação Trivial com IO
    Solvable,
    solveWith
) where

import Point
import Mapping hiding (null)
import Mapping.DefaultMap
import Data.List (sortOn, sort, unwords, dropWhileEnd)



-- | Mapeamento por 'String'.
type StringMapping m a = Mapping m String a


-- | Mapa de pontos por nome.
type Points m a = m String (Point a)


-- | Lê um ponto e seu nome de uma `String`.
readNamedPoint :: Read a => String -> (String, Point a)
readNamedPoint line = (name, Point $ map read nums)
    where (name:nums) = words line

-- | Monta o mapeamento de pontos, com um ponto por linha.
parsePoints :: (Read a, StringMapping m (Point a)) => [String] -> Points m a
parsePoints s = fromList $ map readNamedPoint s


-- | Mapeamento dos pontos categorizados.
type Categorized m = m String String

-- | Lê o nome de um ponto e sua categoria a patir de uma `String`.
readCategorizedPoint :: String -> (String, String)
readCategorizedPoint line = (name, label)
    where [name, label] = words line

-- | Monta o mapeamento dos pontos categorizados, com um ponto por linha.
parseCategorized :: StringMapping m String => [String] -> Categorized m
parseCategorized s = fromList $ map readCategorizedPoint s


-- | Mapeamento nome dos pontos pertencentes a cada categoria.
type Categories m = DefaultMap m String [String]

-- | Monta o mapeamento de categorias com base nos pontos categorizados,
-- que é apenas um mapeamento inverso.
fromCategorized :: (Mapping m String String, Mapping m String [String]) => Categorized m -> Categories m
fromCategorized = invert


-- | Remove os elementos do começo e do final de uma lista
-- a partir de um predicado.
trim :: (a -> Bool) -> [a] -> [a]
trim p = dropWhile p . dropWhileEnd p

-- | Monta a referência dos pontos e dos pontos já categorizados
-- a partir do texto.
parseInput :: (Read a, StringMapping m (Point a), StringMapping n String) => String -> (Points m a, Categorized n)
parseInput text = let (p, c) = break null $ lines text
                  in (parsePoints p, parseCategorized $ trim null c)

-- | Monta a referência dos pontos e dos pontos já categorizados
-- a partir da entrada padrão.
parseInputIO :: (Read a, StringMapping m (Point a), StringMapping n String) => IO (Points m a, Categorized n)
parseInputIO = parseInput <$> getContents


-- | Monta o resultado das categorias em uma lista de linhas.
parseOutput :: StringMapping m [String] => Categories m -> [String]
parseOutput cats = map parseLine $ sortOn fst $ toList cats
    where parseLine (label, points) = unwords $ label : (sort points)

-- | Monta o resultado das categorias na saída padrão.
parseOutputIO :: StringMapping m [String] => Categories m -> IO ()
parseOutputIO = mapM_ putStrLn . parseOutput

-- | Tipos para qual a entrada e saída padrão são
-- trivialmente resolvíveis.
type Solvable a m n p = (Read a, StringMapping m (Point a), StringMapping n String, StringMapping p [String])

-- | Interação trivial com a saída e entrada padrão, com uma
-- solução pra problema.
solveWith :: Solvable a m n p => (Points m a -> Categorized n -> Categories p) -> IO ()
solveWith f = uncurry f <$> parseInputIO >>= parseOutputIO
