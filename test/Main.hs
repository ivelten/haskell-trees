module Main (main) where

import Test.Tasty
import Test.Tasty.HUnit
import Trees

rtree :: Tree Int
rtree = Node 2 (Node 1 Leaf Leaf) (Node 3 Leaf Leaf)

ltree :: Tree Int
ltree = Node 2 (Node 3 Leaf Leaf) (Node 1 Leaf Leaf)

tests :: TestTree
tests = testGroup "Tree"
  [ testGroup "Functor instance implementation"
    [ testCase "fmap should map every Node value" $ do
        fmap (*2) rtree @?= Node 4 (Node 2 Leaf Leaf) (Node 6 Leaf Leaf)
    ]
    ,
    testGroup "Foldable instance implementation"
    [ testCase "foldr should iterate as expected" $ do
        foldr (\x acc -> show x ++ acc) [] rtree @?= "123"
      ,
      testCase "foldl should iterate as expected" $ do
        foldl (\acc x -> show x ++ acc) [] ltree @?= "123"
    ]
    ,
    testGroup "Helper functions"
    [ testCase "depth should recover depth of tree" $ do
        depth rtree @?= 2
      ,
      testCase "addNewMaxL should increase depth by one" $ do
        depth (addNewMaxL ltree) @?= 3
      ,
      testCase "addNewMaxR should increase depth by one" $ do
        depth (addNewMaxR rtree) @?= 3
      ,
      testCase "addNewMaxL should add maximum value at left side" $ do
        addNewMaxL ltree @?= Node 2 (Node 3 (Node 4 Leaf Leaf) Leaf) (Node 1 Leaf Leaf)
      ,
      testCase "addNewMaxR should add maximum value at right side" $ do
        addNewMaxR rtree @?= Node 2 (Node 1 Leaf Leaf) (Node 3 Leaf (Node 4 Leaf Leaf))
      ,
      testCase "isSortedL should return True in a left based tree that is sorted" $ do
        isSortedL ltree @?= True
        isSortedL rtree @?= False
      ,
      testCase "isSortedR should return True in a right based tree that is sorted" $ do
        isSortedR rtree @?= True
        isSortedR ltree @?= False
      ,
      testCase "toListR shold build a list using right based tree mode" $ do
        toListR rtree @?= [1,2,3]
        toListR ltree @?= [3,2,1]
      ,
      testCase "toListL should build a list using left based tree mode" $ do
        toListL ltree @?= [1,2,3]
        toListL rtree @?= [3,2,1]
      ,
      testCase "insertL should insert correctly in a left based tree" $ do
        insertL 0 ltree @?= Node 2 (Node 3 Leaf Leaf) (Node 1 Leaf (Node 0 Leaf Leaf))
        insertL 4 ltree @?= Node 2 (Node 3 (Node 4 Leaf Leaf) Leaf) (Node 1 Leaf Leaf)
      ,
      testCase "insertR should insert correctly in a right based tree" $ do
        insertR 0 rtree @?= Node 2 (Node 1 (Node 0 Leaf Leaf) Leaf) (Node 3 Leaf Leaf)
        insertR 4 rtree @?= Node 2 (Node 1 Leaf Leaf) (Node 3 Leaf (Node 4 Leaf Leaf))
    ]
  ]

main :: IO ()
main = defaultMain tests
