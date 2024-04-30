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
, number
, words
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

data Atom = Null | Number Int | Words String
  deriving (Eq, Ord)

instance Show Atom where
  show Null = "Null"
  show (Number n) = show n
  show (Words str) = str

type IndexedRow = Map String Atom

type ColType a = (Atom -> Either String a, a -> Atom)

wrongType atom typeName = Left $ "Value '"++ show atom ++"' doesn't have type '"++ typeName ++"'"

number :: ColType Int
number =
  let unwrapNumber (Number n) = Right n
      unwrapNumber atom = wrongType atom "number"
  in (unwrapNumber, Number)

words :: ColType String
words = 
  let unwrapWords (Words n) = Right n
      unwrapWords atom = wrongType atom "words"
  in (unwrapWords, Words)

newtype RowState a = RowState { runRS :: [IndexedRow] -> Either String [(a, IndexedRow)] }

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

deleteRow :: RowState ()
deleteRow = RowState $ const [] .- Right

bring :: [IndexedRow] -> RowState ()
bring otherRows = RowState $ \rows ->
  Map.union <$> rows <*> otherRows
  & zip (repeat ())
  & Right

liftEither :: Either String a -> RowState a
liftEither (Right a) = return a
liftEither (Left e) = RowState $ const (Left e)

access :: ColType a -> String -> RowState a
access (unwrap, _) key = do
  let errMsg = "field '"++ key ++"' not found"
  row <- getRow
  atom <- Map.lookup key row & explain errMsg & liftEither
  atom & unwrap & liftEither

whenExists :: ColType a -> String -> RowState () -> RowState ()
whenExists (unwrap, _) key rowOp = do
  row <- getRow
  let exists = (Map.lookup key row & explain "") >>= unwrap & isRight
  when exists rowOp

bind :: ColType a -> String -> a -> RowState ()
bind (_, wrap) key val = Map.insert key (wrap val) & modifyRow

release :: String -> RowState ()
release = Map.delete .- modifyRow

keepOnly :: [String] -> RowState ()
keepOnly fields = zip fields (repeat ())
  & Map.fromList 
  & flip Map.intersection 
  & modifyRow

exec :: RowState a -> [IndexedRow] -> Either String [IndexedRow]
exec rowState rows = runRS rowState rows <&> map snd

row1 = Map.fromList [("n",Number 1),("m",Number 2)]

row2 = Map.fromList [("a",Number 1),("b",Number 2)]

row3 = Map.fromList [("x",Number 1),("y",Number 2)]

testRS = do
  row <- getRow
  when ("n" `Map.member` row) $ modifyRow (Map.insert "u" $ Words "bla")
