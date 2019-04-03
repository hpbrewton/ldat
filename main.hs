module Main (
    main
) where

import Text.Parsec 
import Text.Parsec.Token as PT 
import Text.Parsec.Combinator
import Text.Parsec.String

data Tau = Value Int 
    | Range Int Int 
    | ExplicitList [Tau]
    | List Tau 
    | NonemptyList Tau 
    | BadList
    | EmptyList 
    | Top 
    deriving (Show)

meet :: Tau -> Tau -> Tau 
meet (Value a) (Value b) = Range a b 
meet (Value a) (Range p q) 
    | a >= p && a < q = (Range p q)
    | a < p = (Range a q)
    | a >= q  = (Range p a)
meet r@(Range p q) v@(Value a) = meet v r 
meet (Range p q) (Range r s) = (Range (p `min` r) (q `max` s))
meet (ExplicitList []) (ExplicitList []) = EmptyList
meet (ExplicitList []) (ExplicitList us) = List $ foldr1 meet us 
meet (ExplicitList ts) (ExplicitList []) = List $ foldr1 meet ts 
meet (ExplicitList ts) (ExplicitList us) = List $ meet (foldr1 meet ts) (foldr1 meet us)
meet (List t) (List u) = List $ meet t u
meet (NonemptyList t) (NonemptyList u) = NonemptyList $ meet t u 
meet EmptyList (List u) = (List u)
meet EmptyList (NonemptyList u) = List u 
meet EmptyList (ExplicitList us) = (ExplicitList us)
meet (List u) EmptyList = (List u)
meet (NonemptyList u) EmptyList = List u 
meet (ExplicitList us) EmptyList = (ExplicitList us)
meet a b = Top

generalize :: (Tau -> Bool) -> Tau -> Tau 
generalize o (Value n) = Range ((narrow (o . (\(Value v) -> (Value (-v)))) n (minBound `quot` 2))) (narrow o n (maxBound `quot` 2))
    where 
        narrow o a b 
            | (m == a) || (m == b) = if o (Value b) then b else a
            | o (Value m) = narrow o m b 
            | otherwise = narrow o a m 
            where
                m = (a+b) `quot` 2
generalize _ r@(Range a b) = r 
generalize o (ExplicitList l) 
    | 0 == length l = EmptyList
    | otherwise = case foldr1 meet $ map (generalize (\t -> o $ ExplicitList (t : l))) l of 
        Top -> BadList 
        t -> case o (ExplicitList []) of 
            False -> NonemptyList t 
            True -> List t 
generalize o l@(NonemptyList t) = l 
generalize o BadList = BadList
generalize o EmptyList = EmptyList 
generalize o Top = Top 

value :: Parser Tau 
value = do 
    numstr <- many1 digit
    return $ Value $ read numstr

list :: Parser Tau 
list = do 
    string "["
    elems <- expr `sepBy` (string "," >> spaces)
    string "]"
    if (0 == length elems)
        then return $ EmptyList 
        else return $ ExplicitList elems

expr :: Parser Tau 
expr = (try list) <|> (try value)

o :: Tau -> Bool 
o (ExplicitList []) = False 
o (ExplicitList l) = and $ map nonemp l 
    where 
        nonemp :: Tau -> Bool 
        nonemp (ExplicitList []) = False 
        nonemp (ExplicitList l) = and $ map (\(Value v) -> (v <= 90) && (v >= -90)) l

main :: IO () 
main = case (parse expr "" "[[1, 2,3]]") of 
    Left err -> putStrLn $ show err 
    Right val -> putStrLn $ show $ generalize o val