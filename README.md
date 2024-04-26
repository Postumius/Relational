# Relational
(small) relational databases as Haskell data structures

My intended use case for this is as a component of a text-game engine that I'm working on. Doesn't save the database to the disk or anything, this is an in-memory thing only.

## Running 
Inside of ```Relational/```, run ```cabal repl``` to load the Table module into ghci.

## Querying Example
Let's say we have the following tables, which represent the worldstate of a text-based game, the type where the player goes from room to room, picks up objects, and solves puzzles. 

This table, "things", represents the objects in the world. Rooms are also objects.
```
 | description                                     | item        | location    |
 -------------------------------------------------------------------------------
 | a woven container                               | basket      | living room |
 | It's not just for bathing!                      | bathroom    | space       |
 | it's like a long and soft chair                 | couch       | living room |
 | the room for dining                             | dining room | space       |
 | you beat it to make rhythms                     | drum        | basket      |
 | a mouth organ                                   | harmonica   | basket      |
 | the room that you live in                       | living room | space       |
 | It's meeee!                                     | player      | living room |
 | shake it, baby                                  | rattle      | basket      |
 | the player shouldn't ever encounter this object | space       | space       |
 | a raised surface for putting things on          | table       | living room |
 | where the real work gets done                   | toilet      | bathroom    |
```

This "leadsTo" table represents the connections between rooms. Note that the connections are one-way, and a two-way connection is made of two opposite one-way connections.
```
 | from        | to          |
 -----------------------------
 | bathroom    | living room |
 | dining room | living room |
 | living room | bathroom    |
 | living room | dining room |
```

If we want to know which rooms the player can go to *from* their current *location*, we can run this query in ghci:
```haskell
:{
things
& Table.filter ("item" `hasVal` Words "player") -- Values in the table have to be wrapped in the Atom type
& innerJoin ( onCols [("location", "from")] ) leadsTo
& select ["to"]
:}
```
It returns this table showing that the player can go to the dining room or the bathroom:
```
 | to          |
 ---------------
 | bathroom    |
 | dining room |
```
If the player types in the "look" command we might show them a list of the items they can see. Here's the query we run for that:
```haskell
:{
things
& Table.filter ("item" `hasVal` Words "player")
& suffix " of player"
& innerJoin ( onCols [("location of player", "location")] ) things
& Table.filter ( not . ("item" `hasVal` Words "player") ) -- We don't want to include the player themselves in the list
& select ["item", "description"]
:}
```
```
 | description                            | item   |
 ---------------------------------------------------
 | a raised surface for putting things on | table  |
 | a woven container                      | basket |
 | it's like a long and soft chair        | couch  |
```
