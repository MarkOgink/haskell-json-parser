import Data.Data
import Data.Tree ( Tree(Node) )
import qualified Data.Tree as Data
replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' n x
    | n <= 0    = []
    | otherwise = x:replicate' (n-1) x

take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _
    | n <= 0    = []
take' _ []      = []
take' n (x:xs) = x : take' (n-1) xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

repeat' :: a -> [a]
repeat' n = n:repeat' n

array' :: (Show a, Typeable a) => [a] -> String
array' [] = "Array"
array' (x:xs) = array' xs ++ " (Value:" ++ show (typeOf x) ++ show x ++ ")"

tree' :: Tree String
tree' = Node "Array" [Node "1" [], Node "2" [], Node "3" []]

--creates abstract syntax tree for an array in json
arrayTree :: (Show a) => [a] -> Tree String
arrayTree [] = Node "Array" []
arrayTree [x] = Node "Array" [Node (show x) []]
arrayTree x = Node "Array" (arrayTreeAux x)

--recursive function returning values inside array
arrayTreeAux :: (Show a) => [a] -> [Tree String]
arrayTreeAux [x] = [Node (show x) []] 
arrayTreeAux (x:xs) = Node (show x) [] : arrayTreeAux xs

tree :: Tree String
tree =
  Node
    "hello"
    [ Node "foo" [],
      Node
        "bars"
        [ Node "oi!" [],
          Node
            "baz"
            [ Node
                "a"
                [ Node "b" [],
                  Node "c" []
                ],
              Node "d" [Node "e" []]
            ]
        ],
      Node "foobar" []
    ]