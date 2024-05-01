#!/usr/bin/env cabal
{- cabal:
ghc-options: -Wunused-imports
build-depends: base, containers, mtl, transformers
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
, exec
) where

import Utilities
import Data.Function
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Functor
import Control.Monad.State
import Data.Either
import Control.Monad.Error.Class
import Control.Monad.Trans.Except

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

newtype RowState a = RowState { runRS :: [IndexedRow] -> Either String [(a, IndexedRow)] }

type RowState' a = StateT IndexedRow (ExceptT String []) a

instance Functor RowState where
  fmap = (<$>)

instance Applicative RowState where
  pure x = RowState $ \rows -> zip (repeat x) rows & Right
  (<*>) = ap

instance Monad RowState where
  (RowState h) >>= f = RowState $ \rows -> do
    (as, rows') <- h rows <&> unzip
    let gs = map (f .- runRS) as
    zip gs rows'
      & mapM (\(g, row) -> g [row])
      <&> concat

getRow :: RowState IndexedRow
getRow = RowState $ \rows -> zip rows rows & Right

putRow :: IndexedRow -> RowState ()
putRow row = RowState $ \rows -> repeat ((), row) & take (length rows) & Right

modifyRow :: (IndexedRow -> IndexedRow) -> RowState ()
modifyRow f = getRow >>= (f .- putRow)

deleteRow :: RowState' ()
deleteRow = StateT $ const $ lift []

bring :: [IndexedRow] -> RowState' ()
bring otherRows = StateT $ \row ->
  Map.union <$> pure row <*> otherRows
  & zip (repeat ())
  & lift

liftEither' :: Either String a -> RowState a
liftEither' (Right a) = return a
liftEither' (Left e) = RowState $ const (Left e)

access :: ColType a -> String -> RowState' a
access (unwrap, _) key = do
  let errMsg = "field '"++ key ++"' not found"
  row <- get
  atom <- Map.lookup key row & explain errMsg & liftEither
  atom & unwrap & liftEither

whenExists :: ColType a -> String -> RowState () -> RowState ()
whenExists (unwrap, _) key rowOp = do
  row <- getRow
  let exists = (Map.lookup key row & explain "") >>= unwrap & isRight
  when exists rowOp

bind :: ColType a -> String -> a -> RowState' ()
bind (_, wrap) key val = Map.insert key (wrap val) & modify

release :: String -> RowState ()
release = Map.delete .- modifyRow

keepOnly :: [String] -> RowState ()
keepOnly fields = zip fields (repeat ())
  & Map.fromList 
  & flip Map.intersection 
  & modifyRow

exec :: RowState' a -> [IndexedRow] -> Either String [IndexedRow]
exec rowState = map (execStateT rowState .- runExceptT) .- concat .- sequence

row2 = Map.fromList [("n",Number 1),("m",Number 2)]

row1 = Map.fromList [("a",Number 1),("b",Number 2)]

row3 = Map.fromList [("x",Number 1),("y",Number 2)]
