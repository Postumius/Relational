#!/usr/bin/env cabal
{- cabal:
ghc-options: -Wunused-imports
build-depends: base, containers, mtl, transformers, lens
other-modules: Utilities, Table
-}

module RowState
( ColType
, numberT
, wordsT
, access
, whenM
, whenExists
, bind
, release
, exec
) where

import Utilities
import Table
import Data.Function
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Control.Monad.State
import Data.Either
import Data.Functor
import Control.Monad.Error.Class
import Control.Monad.Trans.Except


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

type RowState a = StateT IndexedRow (ExceptT String []) a

deleteRow :: RowState ()
deleteRow = StateT $ const $ lift []

bring :: Table -> RowState ()
bring table = StateT $ \row ->
  Map.union <$> pure row <*> (table & toRows)
  & zip (repeat ())
  & lift

access :: ColType a -> String -> RowState a
access (unwrap, _) key = do
  let errMsg = "field '"++ key ++"' not found"
  row <- get
  atom <- Map.lookup key row & explain errMsg & liftEither
  atom & unwrap & liftEither

whenExists :: ColType a -> String -> RowState () -> RowState ()
whenExists (unwrap, _) key rowOp = do
  row <- get
  let exists = (Map.lookup key row & explain "") >>= unwrap & isRight
  when exists rowOp

bind :: ColType a -> String -> a -> RowState ()
bind (_, wrap) key val = Map.insert key (wrap val) & modify

release :: String -> RowState ()
release = Map.delete .- modify

keepOnly :: [String] -> RowState ()
keepOnly fields = zip fields (repeat ())
  & Map.fromList 
  & flip Map.intersection 
  & modify

exec :: RowState a -> [IndexedRow] -> Either String [IndexedRow]
exec rowState = map (execStateT rowState .- runExceptT) .- concat .- sequence

foldRows :: (a -> RowState a) -> a -> Table -> Either String [IndexedRow]
foldRows f first table = foldM (\(prevResult, diffRows') row -> do
    (results, rows') <- runStateT (f prevResult) row 
      & runExceptT 
      & sequence 
      <&> unzip
    return $ if null results 
      then (prevResult, diffRows')
      else (last results,  diffRows' . (rows'++))
  ) (first, id) (table & toRows)
  <&> snd
  <&> ($ [])

row2 = Map.fromList [("n",Number 1),("m",Number 2)]

row1 = Map.fromList [("a",Number 1),("b",Number 2)]

row3 = Map.fromList [("x",Number 1),("y",Number 2)]

queryAddCol = sortIntoTables $ foldRows (\acc -> do
    bind numberT "i" acc
    return $ acc+1
  ) 0 things

queryJoin = sortIntoTables $ foldRows (\_ -> do
    whenM (("player" /=) <$> access wordsT "item") $ 
      deleteRow
    bring leadsTo
    location <- access wordsT "location"
    from <- access wordsT "from"
    when (location /= from) $ deleteRow
    keepOnly ["location", "to"]
  ) () things
    
