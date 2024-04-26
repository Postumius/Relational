module Database 
(
) where

import Table
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set(Set)
import qualified Data.Set as Set
import Control.Lens hiding (Index)


type Index = Map [Atom] Table

data Relation = Relation {table::Table, indices::Map (Set String) Index}

data Database = Map String Relation

makeIndex iFields table = 
  let groups = table & groupBy (Set.elems iFields)
      iValsOf table = table 
        & view rowsL
        & head
        & (`Map.restrictKeys` iFields)
        & Map.elems
      removeICols table = table & fieldsL %~ (`Set.difference` iFields)
  in if null iFields 
     then Left "no fields given"
     else if not (iFields `Set.isSubsetOf` (table & view fieldsL)) 
     then Left "fields not present"
     else groups 
          & map (\group -> (iValsOf group, removeICols group))
          & Map.fromList
          & Right
    
