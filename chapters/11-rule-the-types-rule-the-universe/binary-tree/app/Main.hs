import Data.Tree
import System.Random
import qualified Data.Char as Char

data BinaryTree a =
    Leaf | Branch (BinaryTree a) a (BinaryTree a)
    deriving (Eq, Ord, Show)

insert' :: Ord a
            => a
            -> BinaryTree a
            -> BinaryTree a

insert' b Leaf = Branch Leaf b Leaf
insert' b (Branch left a right) =
    case compare b a of
        LT -> Branch (insert' b left) a right
        GT -> Branch left a (insert' b right)
        EQ -> Branch left a right

toDataTree :: BinaryTree Char -> Tree [Char]
toDataTree Leaf = Node "Leaf" []
toDataTree (Branch left a right) =
    Node [a] [toDataTree left, toDataTree right]

randChars n = take n $ randomRs ('a', 'z') (mkStdGen 0)

-- reduce a list using a state object
reduce :: (a -> a -> a) -> a -> [a] -> a
reduce _ z [] = z
reduce f z (x:xs) = reduce f (f z x) xs

mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f b =
    let
        (Branch left a right) = b
    in Branch (mapTree f left) (f a) (mapTree f right)

foldTree' :: (s -> v -> s -> s -> s) -> s -> BinaryTree v -> s
foldTree' _ s Leaf = s
foldTree' f s (Branch left v right) = f s v (foldTree' f s left) (foldTree' f s right)

main = do
    let chars = randChars 5
    let tree = foldr insert' Leaf chars
    let mappedTree = mapTree Char.toUpper tree
    putStrLn $ drawTree $ toDataTree mappedTree
    let folder = (\s v left right -> s ++ left ++ v:right)
    let folded = foldTree' folder "" mappedTree :: String
    putStrLn $ "Folded: " ++ folded