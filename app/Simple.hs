module Main (main) where

import Point
import Categories
import Mapping hiding (null)
import Mapping.Map.Strict



-- wrapper de distancia, para facilitar achar o mínimo
data Distance a = Dist a String String
    deriving (Eq, Show)

instance Ord a => Ord (Distance a) where
    (Dist n1 _ _) `compare` (Dist n2 _ _) = n1 `compare` n2


-- construtor da distância a partir do mapa dos pontos
dist :: Floating a => Points Map a -> String -> String -> Distance a
dist pts p1 p2 = Dist (distance (pts!p1) (pts!p2)) p1 p2

-- lista de nomes dos pontos não categorizados
uncategorized :: Points Map a -> Categorized Map -> [String]
uncategorized pts cats = filter (not . flip member cats) $ keys pts

-- lista de distâncias de todos os pontos não categorizados com
-- todos os pontos categorizados
dists :: Floating a => Points Map a -> Categorized Map -> [Distance a]
dists pts cats = map (dist pts) uncats <*> keys cats
    where uncats = uncategorized pts cats

-- categoriza um ponto com base no wrapper de distância
categorizePoint :: Floating a => Categorized Map -> Distance a -> Categorized Map
categorizePoint cats (Dist _ uncat cat) = insert uncat (cats!cat) cats

-- categoriza todos os pontos não categorizados
categorize :: (Floating a, Ord a) => Points Map a -> Categorized Map -> Categorized Map
categorize pts cats
    | null d    = cats
    | otherwise = categorize pts $ categorizePoint cats $ minimum d
        where d = dists pts cats

-- categoriza os pontos restantes e monta o mapeamento inverso
categories :: Points Map Double -> Categorized Map -> Categories Map
categories pts cat = fromCategorized $ categorize pts cat


main :: IO ()
main = solveWith categories
