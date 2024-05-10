#!/usr/bin/env cabal
{- cabal:
ghc-options: -Wunused-imports
build-depends: base, containers, mtl, lens
other-modules: Utilities
-}

module RowState
( Atom(Null, Number, Words)
, IndexedRow
, ColType
, numberT
, wordsT
, access
, whenM
, whenExists
, bind
, release
) where

import Utilities
import Data.Function
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Functor
import Control.Monad.State
import Data.Either
import Control.Lens
import Data.List

data Atom = Null | Number Int | Words String
  deriving (Eq, Ord)

instance Show Atom where
  show Null = "Null"
  show (Number n) = show n
  show (Words str) = str

type IndexedRow = Map String Atom

type ColType a = (Atom -> Either String a, a -> Atom)

wrongType atom typeName = Left $ "Value '"++ show atom ++"' doesn't have type '"++ typeName ++"'"

numberT :: ColType Int
numberT =
  let unwrapNumber (Number n) = Right n
      unwrapNumber atom = wrongType atom "numberT"
  in (unwrapNumber, Number)

wordsT :: ColType String
wordsT = 
  let unwrapWords (Words n) = Right n
      unwrapWords atom = wrongType atom "wordsT"
  in (unwrapWords, Words)

newtype RowState a b = RowState { runRS :: a -> IndexedRow -> Either String (a, [(b, IndexedRow)]) }

applyRS a r rs = runRS rs a r

instance Functor (RowState a) where
  fmap = (<*>) . pure

instance Applicative (RowState a) where
  pure x = RowState $ \a row -> Right (a, [(x, row)])
  (<*>) = ap

instance Monad (RowState a) where
  (RowState h) >>= f = RowState $ \a row -> do
    (a', pairs) <- h a row
    foldM (\(aAcc, pairsAcc) (x, row') ->
        f x & applyRS aAcc row' <&> over _2 (++pairsAcc)
      ) (a', []) pairs

getRow :: RowState a IndexedRow
getRow = RowState $ \a row -> Right (a, [(row, row)])

--putRow :: IndexedRow -> RowState a ()
--putRow row = RowState $ \a _ -> Right (a, [((), row)])

putRows :: [IndexedRow] -> RowState a ()
putRows rows = RowState $ \a _ -> 
  rows
  & zip (repeat ())
  & ((,) a)
  & Right

putRow = singleton .- putRows

assembleModify g p f = g <&> f >>= p

modifyRow :: (IndexedRow -> IndexedRow) -> RowState a ()
modifyRow = assembleModify getRow putRow

multiplyRow :: (IndexedRow -> [IndexedRow]) -> RowState a ()
multiplyRow = assembleModify getRow putRows

deleteRow :: RowState a ()
deleteRow = putRows []

getAcc = RowState $ \acc row -> Right (acc, [(acc, row)])

putAcc acc = RowState $ \_ row -> Right (acc, [((), row)])

modifyAcc = assembleModify getAcc putAcc

bringIn :: [IndexedRow] -> RowState a ()
bringIn otherRows = multiplyRow $ \row -> map (Map.union row) otherRows

liftEither :: Either String b -> RowState a b
liftEither (Right x) = return x
liftEither (Left e) = RowState $ \_ _ -> Left e

access :: ColType b -> String -> RowState a b
access (unwrap, _) key = do
  let errMsg = "field '"++ key ++"' not found"
  row <- getRow
  atom <- Map.lookup key row & explain errMsg & liftEither
  atom & unwrap & liftEither

whenExists :: ColType a -> String -> RowState a () -> RowState a ()
whenExists (unwrap, _) key rowOp = do
  row <- getRow
  let exists = (Map.lookup key row & explain "") >>= unwrap & isRight
  when exists rowOp

bind :: ColType a -> String -> a -> RowState a ()
bind (_, wrap) key val = Map.insert key (wrap val) & modifyRow

release :: String -> RowState a ()
release = Map.delete .- modifyRow

keepOnly :: [String] -> RowState a ()
keepOnly fields = zip fields (repeat ())
  & Map.fromList 
  & flip Map.intersection 
  & modifyRow

applyAccum :: a -> [IndexedRow] -> RowState a b -> Either String (a, [b], [IndexedRow])
applyAccum acc rows rs = 
  putRows rows >> rs 
  & applyRS acc Map.empty
  <&> over _2 (reverse .- unzip)
  <&> (\(acc', (xs, rows)) -> (acc', xs, rows))

row1 = Map.fromList [("a",Number 1),("b",Number 2)]

row2 = Map.fromList [("n",Number 1),("m",Number 2)]

row3 = Map.fromList [("x",Number 1),("y",Number 2)]

testRS = do
  row <- getRow
  when ("n" `Map.member` row) $ modifyRow (Map.insert "u" $ Words "bla")
  bringIn [row1, row2, row3]
  getAcc >>= bind numberT "i"
  modifyAcc (+1)
