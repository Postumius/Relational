#!/usr/bin/env cabal
{- cabal:
ghc-options: -Wunused-imports
build-depends: base, containers, mtl, lens, transformers
other-modules: Utilities, Table
-}

module RowState
( ColType
, numberT
, wordsT
, RowOp
, rowOp
, applyOp
, runOp
, getRow
, putRow
, modifyRow
, multiplyRow
, deleteRow
, getAcc
, putAcc
, modifyAcc
, bringIn
, access
, bind
, release
, whenExists
, keepOnly
, applyAccum
) where

import Utilities
import Table
import Data.Function
import qualified Data.Map.Strict as Map
import Data.Functor
import Control.Monad
import Control.Monad.Trans.Cont
import Data.Either
import Control.Lens
import Data.List

type ColType a = (Atom -> Either String a, a -> Atom)

wrongType atom typeName = Left $ "Value '"++ show atom ++"' doesn't have type '"++ typeName ++"'"

numberT :: ColType Integer
numberT =
  let unwrapNumber (Number n) = Right n
      unwrapNumber atom = wrongType atom "numberT"
  in (unwrapNumber, Number)

wordsT :: ColType String
wordsT = 
  let unwrapWords (Words n) = Right n
      unwrapWords atom = wrongType atom "wordsT"
  in (unwrapWords, Words)

-- the monad instance of this isn't associative, but it becomes associative when wrapped in a ContT
newtype InternalOp a b = InternalOp { runInternal :: a -> IndexedRow -> Either String (a, [(b, IndexedRow)]) }

instance Functor (InternalOp a) where
  fmap = (<*>) . pure

instance Applicative (InternalOp a) where
  pure x = InternalOp $ \a row -> Right (a, [(x, row)])
  (<*>) = ap

instance Monad (InternalOp a) where
  (InternalOp h) >>= f = InternalOp $ \a row -> do
    (a', pairs) <- h a row
    foldM (\(aAcc, pairsAcc) (x, row') ->
        runInternal (f x) aAcc row' <&> over _2 (++pairsAcc)
      ) (a', []) pairs

-- the wrapping type which is exported, obeys monad laws as far as I can tell
newtype RowOp r a b = RowOp { unwrapOp :: ContT r (InternalOp a) b }

instance Functor (RowOp r a) where
  fmap = pure .- (<*>)

instance Applicative (RowOp r a) where
  pure = pure .- RowOp 
  (<*>) = ap

instance Monad (RowOp r a) where
  s >>= f = unwrapOp s >>= (f .- unwrapOp) & RowOp

liftND :: InternalOp a b -> RowOp r a b
liftND op = RowOp $ ContT $ \k -> op >>= k

rowOp = InternalOp .- liftND

applyOp acc row op = 
  let identity x = InternalOp $ \acc row -> Right (acc, [(x, row)])
  in runInternal ((op & unwrapOp & runContT) identity) acc row

runOp op acc row = applyOp acc row op

getRow :: RowOp r a IndexedRow
getRow = rowOp $ \a row -> Right (a, [(row, row)])

--putRow :: IndexedRow -> RowOp r a ()
--putRow row = rowOp $ \a _ -> Right (a, [((), row)])

putRows :: [IndexedRow] -> RowOp r a ()
putRows rows = rowOp $ \a _ -> 
  rows
  & zip (repeat ())
  & ((,) a)
  & Right

putRow = singleton .- putRows

assembleModify g p f = g <&> f >>= p

modifyRow :: (IndexedRow -> IndexedRow) -> RowOp r a ()
modifyRow = assembleModify getRow putRow

multiplyRow :: (IndexedRow -> [IndexedRow]) -> RowOp r a ()
multiplyRow = assembleModify getRow putRows

deleteRow :: RowOp r a ()
deleteRow = putRows []

getAcc = rowOp $ \acc row -> Right (acc, [(acc, row)])

putAcc acc = rowOp $ \_ row -> Right (acc, [((), row)])

modifyAcc = assembleModify getAcc putAcc

bringIn :: Table -> RowOp r a ()
bringIn otherTable = multiplyRow $ \row -> map (Map.union row) (otherTable & view rowsL)

liftEither :: Either String b -> RowOp r a b
liftEither (Right x) = return x
liftEither (Left e) = rowOp $ \_ _ -> Left e

access :: ColType b -> String -> RowOp r a b
access (unwrap, _) key = do
  let errMsg = "field '"++ key ++"' not found"
  row <- getRow
  atom <- Map.lookup key row & explain errMsg & liftEither
  atom & unwrap & liftEither

whenExists :: ColType a -> String -> RowOp r a () -> RowOp r a ()
whenExists (unwrap, _) key rowOp = do
  row <- getRow
  let exists = (Map.lookup key row & explain "") >>= unwrap & isRight
  when exists rowOp

bind :: ColType b -> String -> b -> RowOp r a ()
bind (_, wrap) key val = Map.insert key (wrap val) & modifyRow

release :: String -> RowOp r a ()
release = Map.delete .- modifyRow

keepOnly :: [String] -> RowOp r a ()
keepOnly fields = zip fields (repeat ())
  & Map.fromList 
  & flip Map.intersection 
  & modifyRow

applyAccum :: a -> Table -> RowOp b a b -> Either String (a, [b], [IndexedRow])
applyAccum acc table op = 
  table 
  & view rowsL 
  & putRows >> op
  & applyOp acc Map.empty
  <&> over _2 (reverse .- unzip)
  <&> (\(acc', (xs, rows)) -> (acc', xs, rows))

row1 = Map.fromList [("a",Number 1),("b",Number 2)]

row2 = Map.fromList [("n",Number 1),("m",Number 2)]

row3 = Map.fromList [("u",Number 1),("v",Number 2)]

row4 = Map.fromList [("x",Number 1),("y",Number 2)]

-- test1 = do
--   bringIn [row3, row4]
--   --getAcc >>= bind numberT "i"
--   modifyAcc (++"0")
-- 
-- test2 = do
--   getAcc >>= bind wordsT "j"
--   modifyAcc (++"1")
-- 
-- testCombined = do
--   bringIn [row3, row4]
--   --getAcc >>= bind numberT "i"
--   modifyAcc (++"0")
--   getAcc >>= bind wordsT "j"
--   modifyAcc (++"1")

test1 = applyAccum 0 things (do
    item <- access wordsT "item"
    location <- access wordsT "location"
    item ++ location & bind wordsT "itemlocation"
  )
  & sortIntoTables


test2 = applyAccum 0 things (do
    item <- access wordsT "item"
    when (item /= "player") deleteRow
    suffix "2" things & bringIn
    getAcc >>= bind numberT "i"
    modifyAcc (+1)
    -- loc1 <- access wordsT "location"
    -- loc2 <- access wordsT "location2"
    -- when (loc1 /= loc2) deleteRow
    -- keepOnly ["description2", "item2"]
  )
  & sortIntoTables
  <&> map (assignKey ["i"])

testAcc = applyAccum 0 things (do
    getAcc >>= bind numberT "i"
    modifyAcc (+1)
  )
  & sortIntoTables
  <&> map (assignKey ["i"])

testJoin = applyAccum 0 things (do
    whenM ((/=) <$> access wordsT "item" <*> (pure "player"))
      deleteRow
    bringIn leadsTo
    whenM ((/=) <$> access wordsT "location" <*> access wordsT "from")
      deleteRow
    keepOnly ["to"]
  )
  & sortIntoTables
