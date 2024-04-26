#!/usr/bin/env cabal
{- cabal:
ghc-options: -Wunused-imports
build-depends: base, containers, mtl
other-modules: Utilities
-}

module RowState
( Atom(Null, Number, Words)
, IndexedRow
, ColType
, numT
, wordT
, access
, whenM
, whenExists
, bind
, release
, exec
) where

import Utilities
import Data.Function
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Control.Monad.State
import Control.Monad.Error.Class
import Data.Either

data Atom = Null | Number Int | Words String
  deriving (Eq, Ord)

instance Show Atom where
  show Null = "Null"
  show (Number n) = show n
  show (Words str) = str

type IndexedRow = Map String Atom

type ColType a = (Atom -> Either String a, a -> Atom)

wrongType atom typeName = Left $ "Value '"++ show atom ++"' doesn't have type '"++ typeName ++"'"

numT :: ColType Int
numT =
  let unwrapNumber (Number n) = Right n
      unwrapNumber atom = wrongType atom "numT"
  in (unwrapNumber, Number)

wordT :: ColType String
wordT = 
  let unwrapWords (Words n) = Right n
      unwrapWords atom = wrongType atom "wordT"
  in (unwrapWords, Words)

type RowState a = StateT (Map String Atom) (Either String) a

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

-- deleteRow :: RowState ()
-- deleteRow = Left Nothing & liftEither 

exec :: RowState a -> IndexedRow -> Either String IndexedRow
exec = execStateT

row = Map.fromList [("n",Number 1),("m",Number 2)]
