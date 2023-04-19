-- Generate appendTree<0..4> and addDigits<1..4> for Data.Sequence
module Main where

main :: IO ()
main = putStr (compose [showAppend n | n <- [0..4]] "")

showAppend :: Int -> ShowS
showAppend n =
    showChar '\n' .
    showString "appendTree" . shows n . showString " :: " .
        showFunType
            ([fingertree] ++ replicate n tyarg ++ [fingertree]) fingertree .
            showString "\n" .
    appendTreeClause "EmptyT" "xs" (showCons (args n) (showString "xs")) .
    appendTreeClause "xs" "EmptyT" (showSnoc (showString "xs") (args n)) .
    appendTreeClause "(Single x)" "xs"
        (showCons ('x':args n) (showString "xs")) .
    appendTreeClause "xs" "(Single x)"
        (showSnoc (showString "xs") (args n++"x")) .
    appendTreeClause "(Deep s1 pr1 m1 sf1)" "(Deep s2 pr2 m2 sf2)"
        (showString "Deep (s1" .
         compose [showString " + size " . showChar v | v <- args n] .
         showString " + s2) pr1 (addDigits" . shows n .
         showString " m1 sf1" . showArgList (args n) .
         showString " pr2 m2) sf2") .
    showChar '\n' .
    showString "addDigits" . shows n . showString " :: " .
        showFunType
            ([fingertree_node, digit] ++ replicate n tyarg ++ [digit, fingertree_node])
            fingertree_node .
        showString "\n" .
    compose [addDigitsClause n1 n2 | n1 <- [1..4], n2 <- [1..4]]
  where
    fingertree = tyapp "FingerTree" tyarg
    digit = tyapp "Digit" tyarg
    fingertree_node = tyapp "FingerTree" (tyapp "Node" tyarg)
    showFunType ts tr =
        compose [showString t . showString " -> " | t <- ts] . showString tr
    tyapp tc t = tc ++ " (" ++ t ++ ")"
    tyarg
      | n == 0 = "Elem a"
      | otherwise = "Node a"
    appendTreeClause t1 t2 rhs =
        showString "appendTree" . shows n .
            showChar ' ' . showString t1 . showArgList (args n) .
            showChar ' ' . showString t2 .
            showString " =\n    " . rhs . showChar '\n'
    addDigitsClause n1 n2 =
        showString "addDigits" . shows n .
            showString " m1 (" . showDigit vs1 . showChar ')' .
            showArgList vsm .
            showString " (" . showDigit vs2 . showString ") m2" .
            showString " =\n    " .
            showString "appendTree" . shows (length ns) .
            showString " m1" .
            compose [showString " (" .  showNode node . showChar ')' |
                node <- ns] .
            showString " m2" . showChar '\n'
      where
        vs = args (n1+n+n2)
        vs1 = take n1 vs
        vsm = take n (drop n1 vs)
        vs2 = drop (n1+n) vs
        ns = nodes vs

data Node a = Node2 a a | Node3 a a a

nodes :: [a] -> [Node a]
nodes [a, b] = [Node2 a b]
nodes [a, b, c] = [Node3 a b c]
nodes [a, b, c, d] = [Node2 a b, Node2 c d]
nodes (a:b:c:xs) = Node3 a b c : nodes xs

showNode (Node2 a b) =
    showString "node2 " . showChar a . showChar ' ' . showChar b
showNode (Node3 a b c) =
    showString "node3 " . showChar a . showChar ' ' . showChar b .
        showChar ' ' . showChar c

showDigit vs =
    showString (["One", "Two", "Three", "Four"]!!(length vs-1)) .
    showArgList vs

showArgList :: [Char] -> ShowS
showArgList vs = compose [showChar ' ' . showChar c | c <- vs]

args :: Int -> [Char]
args n = take n ['a'..]

showCons xs sf =
    compose [showChar x . showString " `consTree` " | x <- xs] . sf
showSnoc sf xs =
    sf . compose [showString " `snocTree` " . showChar x | x <- xs]

compose :: [a -> a] -> a -> a
compose = flip (foldr id)
