#!/usr/bin/env cabal
{- cabal:
ghc-options: -Wunused-imports
build-depends: base, containers, lens, mtl
other-modules: Utilities, RowState
-}

module Table
( Table
, makeTable
, rowsL
, colsL
, addCol
, headerL
, fieldsL
, select
, Table.filter
, hasVal
, Table.insert
, Table.lookup
, filterByKey
, innerJoin
, onCols
, natural
, renameCols
, suffix
, Table.groupBy
, toRows
, fromRows
, sortIntoTables
, mapRowOps_
, items
) where

import Utilities
import RowState
import Data.List
import Data.Function
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set(Set)
import qualified Data.Set as Set
import Control.Lens
import Data.List.NonEmpty (groupAllWith, toList)


newtype Table = Table { toMap :: (Map IndexedRow IndexedRow) }

instance Show Table where
  showsPrec _ table =
    let intercalateS sep = intersperse sep .- foldr (.) id
        justifyCol (field, col) =
          let stringCol = field : (map show col)
              colWidth = stringCol & (map length) & maximum
              padText text = 
                (text++) . 
                (replicate (colWidth - length text) ' ' ++)
          in stringCol & map padText
        title : rows = table & view colsL
          & Map.toList
          & map justifyCol
          & transpose
          & map ( \row -> 
            (" | "++) . 
            intercalateS (" | "++) row .
            (" |"++) )
        line = (" "++) . showString (replicate (length (title "") - 1) '-')
    in ("\n"++) . 
       intercalateS ("\n"++) (title:line:rows) . 
       ("\n"++)

toKeyVal hdr' row = 
  let (key, val) = hdr' & both %~ Map.restrictKeys row
  in (key, val `Map.difference` key)

internalMakeTable header rows = rows
  & map (toKeyVal header)
  & Map.fromList
  & Table

makeTable listHeader rowData = 
  let header = listHeader & both %~ Set.fromList
      rows = rowData & map (zip (uncurry (++) listHeader) .- Map.fromList)
  in internalMakeTable header rows

rowsL :: Lens' Table [IndexedRow]
rowsL f (Table mp) = 
  let rows = mp & Map.toList & map (uncurry Map.union)
      header = Table mp ^. headerL 
      fromRows rows' = rows' 
        & map (toKeyVal header) 
        & Map.fromList 
        & Table
  in fmap fromRows (f rows)

zipWithSet st ls = zip (Set.elems st) ls & Map.fromAscList

colsL :: Lens' Table (Map String [Atom])
colsL f table =
  let fields = table ^. headerL & uncurry Set.union
      cols = table & view rowsL 
        & map Map.elems 
        & transpose 
        & zipWithSet fields
      fromCols cols' = table & set rowsL (
        cols' 
          & Map.elems 
          & transpose 
          & map (zipWithSet fields))
  in fmap fromCols (f cols)

zipPairsWith f (a,b) (x,y) = (f a x, f b y)

fromHdrCols hdrCols' = 
  let header' = hdrCols' & both %~ Map.keysSet
  in hdrCols' 
     & both %~ Map.elems .- transpose
     & uncurry zip
     & map (zipPairsWith zipWithSet header')
     & Map.fromList
     & Table

hdrColsL f (Table mp) = 
  let header = Table mp & view headerL 
      verticalPair = mp 
        & Map.toList
        & map (both %~ Map.elems)
        & unzip
        & both %~ transpose
      hdrCols = zipPairsWith zipWithSet 
        (Table mp ^. headerL) 
        verticalPair
  in fmap fromHdrCols (f hdrCols)

addCol field col = hdrColsL._2.(at field) .~ Just col

headerL :: Lens' Table (Set String, Set String)
headerL f (Table mp) =
  let header = case mp & Map.toList & map (both %~ Map.keysSet) of
        hdr:_ -> hdr
        _ -> (Set.empty, Set.empty)
      setHeader hdr' = Table mp & view rowsL 
        & map (toKeyVal hdr') 
        & Map.fromList 
        & Table
  in fmap setHeader (f header)

setHeader listHeader = set headerL (listHeader & both %~ Set.fromList)

fieldsL :: Lens' Table (Set String)
fieldsL f (Table mp) = 
  let (keyFields, valFields) = Table mp & view headerL
      fields = keyFields `Set.union` valFields
      restrictColumns fields' (key, val) = 
        (if keyFields `Set.isSubsetOf` fields'
         then (key, val)
         else (key`Map.union`val, Map.empty))
        & both %~ flip Map.restrictKeys fields'
      setFields fields' = mp
        & Map.toList
        & map (restrictColumns fields')
        & Map.fromList
        & Table
  in fmap setFields (f fields)

select = Set.fromList .- set fieldsL

filter = Data.List.filter .- over rowsL

hasVal :: String -> Atom -> IndexedRow -> Bool
hasVal field val = Map.lookup field .- (== Just val)

insert :: IndexedRow -> Table -> Table
insert row (Table mp) = 
  let header@(keyFields, _) = Table mp ^. headerL
  in if Map.keysSet row == uncurry Set.union header
     then toKeyVal header row & (\(k,v)->Map.insert k v mp) & Table
     else error "row columns don't match table columns"

lookup key (Table mp) = Map.lookup key mp

filterByKey key = 
  Table.lookup key
  .- maybe Map.empty (Map.singleton key) 
  .- Table
  
innerJoin condition table2 table1 =
  let unionFields _n =
        (table1^.headerL._n) `Set.union` (table2^.headerL._n)
      header = (unionFields _1, unionFields _2)
      cartesianProduct = (,) <$> table1^.rowsL <*> table2^.rowsL
      rows = cartesianProduct
        & Data.List.filter (uncurry condition) 
        & map (uncurry Map.union)
  in internalMakeTable header rows

onCols :: [(String, String)] -> IndexedRow -> IndexedRow -> Bool
onCols pairs row1 row2 = 
  let colsMatch pair = zipPairsWith Map.lookup pair (row1, row2) 
        & uncurry (==)
  in all colsMatch pairs

natural :: IndexedRow -> IndexedRow -> Bool
natural row1 row2 = Map.intersectionWith (==) row1 row2 
  & Map.elems 
  & or

renameKeys pairs mp = 
  let pairMap = Map.fromList pairs
      (piece, remainder) = 
        ( mp `Map.intersection` pairMap
        , mp `Map.difference` pairMap )
      pairMapRestricted = pairMap `Map.intersection` piece
  in piece
     & Map.elems
     & zip (Map.elems pairMapRestricted)
     & Map.fromList
     & Map.union remainder

renameCols pairs = hdrColsL.both %~ renameKeys pairs

suffix str table = 
  let fields = table & view fieldsL & Set.elems
      suffixed = map (++ str) fields
  in renameCols (zip fields suffixed) table

groupBy fields table = table
  & select fields
  & view rowsL
  & map (Map.isSubmapOf .- (`Table.filter` table))

-- data Err = 
--   FieldNotFound String |
--   WrongType Atom String
--   deriving (Show)

toRows :: Table -> [IndexedRow]
toRows = view rowsL

fromRows = (`zip` repeat Map.empty) .- Map.fromList .- Table

sortIntoTables :: Either String [IndexedRow] -> Either String [Table]
sortIntoTables = fmap $ groupAllWith Map.keys .- map (toList .- fromRows)

mapRowOps_ rowOps = toRows .- mapM (exec rowOps)

items = makeTable
  (["item"],       ["location"])
  [[Words "laptop", Words "living room"],
   [Words "bed",    Words "back room"],
   [Words "couch",  Words "living room"],
   [Words "living room", Words "space"],
   [Words "back room", Words "space"],
   [Words "space", Words "space"]]

