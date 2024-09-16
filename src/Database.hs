{- cabal:
  ghc-options: -Wunused-imports
  build-depends: base, containers, lens, transformers
  other-modules: Utilities, Table
-}

module Database 
( Index
, getTable
, Database
, makeRelation
, withSubrow
, query
, RuleType (Insertion, Deletion)
, Rule
, Database.insert
, Database.delete
, overwrite
, addForeignKeyConstraint
, world
) where

import Table
import Utilities
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set(Set)
import qualified Data.Set as Set
import Control.Lens hiding (Index)
import Data.Maybe
import Control.Monad


type Index = Map [Atom] Table

data Relation = Relation 
  { getTable :: Table
  , getIndices :: Map (Set String) Index
  , getRules :: [Rule]
  }

instance Show Relation where
  show = getTable .- show

tableL :: Lens' Relation Table
tableL f (Relation tbl indcs rs) = 
  fmap (\tbl' -> Relation tbl' indcs rs) (f tbl)

indicesL :: Lens' Relation (Map (Set String) Index)
indicesL f (Relation tbl indcs rs) = 
  fmap (\indcs' -> Relation tbl indcs' rs) (f indcs)

rulesL :: Lens' Relation [Rule]
rulesL f (Relation tbl indcs rs) = 
  fmap (\rs' -> Relation tbl indcs rs') (f rs)

type Database = Map String Relation

makeIndex table iFields = 
  let groups = table & groupBy (Set.elems iFields)
      iValsOf table = table 
        & view rowsL
        & head
        & (`Map.restrictKeys` iFields)
        & Map.elems
      removeICols table = table & fieldsL %~ (`Set.difference` iFields)
  in if Prelude.null iFields 
     then error "no fields given"
     else if not (iFields `Set.isSubsetOf` (table & view fieldsL)) 
     then error "fields not present"
     else groups 
          & map (\group -> (iValsOf group, removeICols group))
          & Map.fromList

makeRelation table indexFields =
  let indexSets = indexFields & map Set.fromList
      indices = indexSets `zip` map (makeIndex table) indexSets & Map.fromList
  in Relation table indices []

searchIndex :: [Atom] -> Index -> Table
searchIndex vals = Map.lookup vals .- maybe (fromMap Map.empty) id

withSubrow subrow rel = 
  case rel & getIndices & Map.lookup (Map.keysSet subrow) of
    Just index -> index & searchIndex (Map.elems subrow)
    Nothing -> rel & getTable & filterRows (subrow `Map.isSubmapOf`) & deselect (Map.keys subrow)

query tableName = lookupRel tableName .- (<&> getTable)

data RuleType = Insertion | Deletion
  deriving (Eq)

type Rule = (RuleType, (IndexedRow, Database) -> Maybe String)

lookupRel tableName = 
  Map.lookup tableName .- 
  explain ("table (" ++ tableName ++ ") not found") 

(insert, delete) = 
  let makeModifier :: RuleType -> (IndexedRow -> Table -> Table)
        -> IndexedRow -> String -> Database -> Either String Database
      makeModifier ruleType fInner = \row tableName db -> do 
        let rowFields = Map.keysSet row
        rel <- lookupRel tableName db
        let tableFields = rel & view (tableL . fieldsL)
        let showSet = Set.toList .- show
        unless (rowFields `Set.isSubsetOf` tableFields) $ 
          "fields " ++ showSet rowFields ++ 
          " don't match table fields " ++ showSet tableFields 
          & Left
        let constraints = rel
              & getRules 
              & filter (fst .- (== ruleType)) 
              & map snd
        let violations = constraints & map ($ (row, db)) & catMaybes
        unless (Prelude.null violations) $ 
          violations & unlines & Left
        db 
          & over (ix tableName . tableL) (fInner row) 
          & Right 
  in ( makeModifier Insertion internalInsert
     , makeModifier Deletion internalDelete
     )

overwrite f key tableName db = do
  row' <- db 
    & lookupRel tableName 
    <&> getTable 
    >>= Table.lookup key .- explain ("key "++show key++" not found in table ("++tableName++")")
    <&> f
    <&> Map.union key
  db & Database.insert row' tableName


immigration :: Set String -> String -> Rule
immigration colNames originName = 
  ( Insertion
  , \(row, db) ->
      let origin = db Map.! originName & getTable
          keyColNames = origin & view (headerL._1) & Set.toList
          keyVals = Map.restrictKeys row colNames & Map.elems
          key = zip keyColNames keyVals & Map.fromList
      in if origin & Table.lookup key & isJust
         then Nothing
         else Just $ "foreign key " ++ (show $ Map.toList key) ++ " not found in table (" ++ originName ++ ")"
  )

emigration :: Set String -> String -> Rule
emigration colNames destName =
  ( Deletion
  , \(key, db) ->
      let subrow = key & Map.elems & zip (Set.toList colNames) & Map.fromList
      in if db Map.! destName & withSubrow subrow & Table.null
         then Nothing
         else Just $ "table (" ++ destName ++ ") depends on foreign key " ++ (show $ Map.toList key)
  )

addForeignKeyConstraint fields originName destName = 
  let addRule here there makeRule = 
        over (ix here . rulesL) (makeRule (Set.fromList fields) there :)
  in addRule originName destName emigration .- 
     addRule destName originName immigration

world :: Database
world = Map.fromList
  [ ( "things"
    , Relation things Map.empty []
    )
  , ( "leadsTo"
    , Relation leadsTo Map.empty []
    )
  , ( "containers"
    , Relation containers Map.empty []
    )
  ]
  & addForeignKeyConstraint ["location"] "containers" "things"

errInsert = Database.insert 
  (Map.fromList [("item", Words "thing"),("description", Words ""),("location", Words "harmonica")])
  "things"
  world

errDelete = Database.delete (Map.fromList [("item", Words "basket")]) "containers" world
