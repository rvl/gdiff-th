{-# LANGUAGE NoMonomorphismRestriction, GADTs, FlexibleContexts,
    FlexibleInstances, LiberalTypeSynonyms, MultiParamTypeClasses,
    DeriveDataTypeable #-}

module Main where
import Data.Generic.Diff.TH
import Data.Generic.Diff   -- (EditScript(..), diff, Type, compress)
import Data.Typeable
import Utils

-- A simple Expression

data Exp = Exp :+: Exp
         | Exp :*: Exp
         | B Int
         deriving(Show, Eq, Typeable)

makeGDiff ''Exp

-- Two examples using the num hack
testA :: Exp
testA = foldl1 (:+:) . map B $ [0..20]

testB :: Exp
testB = foldl1 (:+:) . map B $ [0..8] ++ [42] ++ [10..20]

-- For some reason I seem to need to do this to help out type inference
diffExp :: (Type ExpFamily Exp)
        => Exp -> Exp -> EditScript ExpFamily Exp Exp
diffExp = diff

diffAandB = showCompressed $ diffExp testA testB

main = diffAandB       

-- Now a more practical example
