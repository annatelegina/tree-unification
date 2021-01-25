import Data.Maybe
import Data.List

data Operation = Add | Sub | Mul | Div deriving (Eq)

instance Show Operation where
    show Add = "+"
    show Sub = "-"
    show Mul = "*"
    show Div = "/"

data Cell = Op Operation | Val Double | Var String deriving (Eq, Show)
data Abstract = Empty | Node Cell (Abstract) (Abstract) | Leaf Cell deriving (Show)


-- print utils for console
showAll :: [(Cell, Cell)] -> String
showAll []  = "\n"
showAll ( (a, b): xs) = showCell a ++ " = " ++ showCell b  ++ "\n " ++ showAll xs
-- showAll (Nothing : xs) = "Nothing " ++ showAll xs

showCell :: Cell -> String
showCell (Op a) = show a
showCell (Val a) = show a
showCell (Var a) = a

showTree :: Abstract -> String
showTree (Node a l r) = (showCell a) ++ (showTree l) ++ (showTree r) ++ "\n"
showTree (Leaf a) = (showCell a)
showTree Empty = "empty \n" 


catJust :: [Maybe (Cell, Cell)] -> [(Cell, Cell)]
catJust [] = []
catJust (Just (a, b) : xs) = (a, b) : (catJust xs)


-- checking utils for the list of elements
checkCorrect :: [Maybe (Cell, Cell)] -> Bool
checkCorrect [] = True
checkCorrect (Nothing : xs) = False
checkCorrect ( _ : xs) = checkCorrect xs


findExpr :: (Cell, Cell) -> [(Cell, Cell)] -> [(Cell, Cell)]
findExpr _ [] = []
findExpr (Var p, b) ((Var p2, c) : xs) | p == p2 && b == c = (Var p, b) : (findExpr (Var p, b) xs)
findExpr (Var p, b) ((Var p2, c) : xs) | p == p2 = (b, c) : (findExpr (Var p, b) xs)
findExpr (Var p, b) ((p2, Var c) : xs) | p == c = (p2, b) : (findExpr (Var p, b) xs)
findExpr k (x: xs) = x : findExpr k xs       


checkVal :: [(Cell, Cell)] -> [(Cell, Cell)] -> [(Cell, Cell)]
checkVal [] l = l
checkVal (x : xs) l = checkVal xs $ findExpr x l


checkEquals :: [(Cell, Cell)] -> Bool
checkEquals [] = True
checkEquals ((Val a, Val b) :xs) | a /= b = False
checkEquals (( Val a, Op b) :xs) = False
checkEquals ( (Op a, Val b) :xs) = False
checkEquals (x:xs) = checkEquals xs


checkPair :: Cell -> Cell -> [Maybe (Cell, Cell)]
checkPair (Op a) (Op b) | a == b = []
checkPair (Val a) (Val b) | a == b = []
checkPair (Var a) (Var b) | a == b = []
checkPair (Op a) (Var b) = [Just (Var b, Op a)]
checkPair (Var a) (Op b) = [Just (Var a, Op b)]
checkPair (Var a) (Val b) = [Just (Var a, Val b)]
checkPair (Val a) (Var b) = [Just (Var b, Val a)]
checkPair (Var a) (Var b) = [Just (Var a, Var b)]
checkPair _ _ = [Nothing]


-- unification function for Trees
unification :: Abstract -> Abstract -> [Maybe (Cell, Cell)]
unification Empty Empty = []
unification Empty _ = [Nothing]
unification _ Empty = [Nothing]
unification (Node a l r) (Node b x y) = (checkPair a b) ++ (unification l x) ++ (unification r y)
unification (Leaf a) (Leaf b) = (checkPair a b)
unification (Leaf _) (Node _ _ _) = [Nothing]
unification (Node _ _ _) (Leaf _ ) = [Nothing]


-- process of unification and checking

processTree :: Abstract -> Abstract -> [(Cell, Cell)]
processTree a b = catJust $ unification a b   

analyze :: Abstract -> Abstract -> String
analyze a b | (checkCorrect y) && (checkEquals $ checkVal x x) = "Success! \n " ++ showAll x
              where 
                   y = unification a b
                   x = catJust y
analyze _ _ = "Can't match the expression! \n"

-- b :: Abstract
-- a :: Abstract
-- a = (Node (Var "F") (Node (Op Mul) (Leaf (Val 7)) (Leaf (Var "Y"))) (Leaf (Val 7))) 
-- b = (Node (Op Add) (Node (Var "Y") (Leaf (Var "X")) (Leaf (Var "K"))) (Leaf (Var "P"))) 

main = do  
      putStrLn $ analyze Empty Empty
      putStrLn $ analyze (Leaf (Op Mul)) (Leaf (Val 7))
      putStrLn $ analyze (Leaf (Var "X")) (Leaf (Val 10))
      putStrLn $ analyze (Node (Op Mul) (Leaf (Var "X")) Empty ) (Node (Op Add) (Leaf (Var "P")) Empty )
      putStrLn $ analyze (Node (Op Add) (Node (Op Mul) (Leaf (Val 7)) (Leaf (Var "Y")))  (Leaf (Val 7))) (Node (Var "F") (Node (Var "A") (Leaf (Val 7)) (Leaf (Val 5 )))  (Leaf (Val 7)) )
      putStrLn $ analyze (Node (Op Add) (Node (Op Mul) (Leaf (Val 10)) (Leaf (Var "Y")))  (Leaf (Val 7))) (Node (Var "F") (Node (Var "A") (Leaf (Val 7)) (Leaf (Val 5 )))  (Leaf (Val 7)) )
      putStrLn $ analyze (Node (Op Add) (Node (Op Mul) (Leaf (Val 7)) (Leaf (Var "Y")))  (Leaf (Val 7))) (Node (Var "F") (Node (Var "A") (Leaf (Val 7)) (Leaf (Val 5 ))) Empty )
      putStrLn $ analyze (Node (Op Add) (Node (Var "B") (Leaf (Var "K")) (Leaf (Var "Y")))  (Leaf (Val 7))) (Node (Op Add) (Node (Var "A") (Leaf (Val 7)) (Leaf (Val 5 )))  (Leaf (Val 7)) )
