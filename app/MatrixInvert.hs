module MatrixInvert where

{-
Invert a binary tree.
    a
   / \
  b   c
 / \  /
d   e f
should become:

  a
 / \
 c  b
 \  / \
  f e  d
-}

data Tree a = Node a (Tree a) (Tree a) | EmptyNode deriving (Show, Eq)

invert :: Tree a -> Tree a
invert EmptyNode = EmptyNode
invert (Node v t1 t2) = Node v (invert t2) (invert t1)

exampleTree = Node 'a' (Node 'b'
                          (Node 'd' EmptyNode EmptyNode) (Node 'e' EmptyNode EmptyNode))
                       (Node 'c' (Node 'f' EmptyNode EmptyNode) EmptyNode)