{- Copyright © 2012, Vincent Elisha Lee Frey.  All rights reserved.
 - This is open source software distributed under a MIT license.
 - See the file 'LICENSE' for further information.
 -}
module System.Console.CmdTheLine.Trie where

import qualified Data.Map as M

{-
 - This implementation maps any non-ambiguous prefix of a key to its value.
 -}

type CMap = M.Map Char

data Value a = Pre a -- Value bound by a prefix key.
             | Key a -- Value bound by an entire key.
             | Amb   -- No Value bound due to ambiguity in the key.
             | Nil   -- Attempt to retrieve a Value from an empty Trie.
               deriving (Eq)

data Trie a = Trie
  { val   :: Value a
  , succs :: CMap (Trie a)
  } deriving (Eq)

data LookupFail = Ambiguous | NotFound deriving (Show)

empty :: Trie a
empty = Trie Nil M.empty

isEmpty :: Eq a => Trie a -> Bool
isEmpty = (== empty)

add :: Trie a -> String -> a -> Trie a
add t k v = go t k (length k) 0 v (Pre v {- Allocate less. -})
  where
  go t k len i v preV =
    if i == len
       then Trie (Key v) (succs t)
       else Trie newVal  newSuccs
    where
    newVal = case val t of
      Amb       -> Amb
      Pre _     -> Amb
      v@(Key _) -> v
      Nil       -> preV

    newSuccs = M.insert (k !! i) (go t' k len (i + 1) v preV) (succs t)
      where
      t' = maybe empty id $ M.lookup (k !! i) (succs t)

findNode :: String -> Trie a -> Maybe (Trie a)
findNode k t = go t k (length k) 0
  where
  go t k len i =
    if i == len
       then Just t
       else goNext =<< M.lookup (k !! i) (succs t)
    where
    goNext t' = go t' k len (i + 1)


lookup :: String -> Trie a -> Either LookupFail a
lookup k t = case findNode k t of
  Nothing -> Left NotFound
  Just t' -> case val t' of
    Key v -> Right v
    Pre v -> Right v
    Amb   -> Left  Ambiguous
    Nil   -> Left  NotFound

ambiguities :: Trie a -> String -> [String]
ambiguities t pre = case findNode pre t of
  Nothing -> []
  Just t' -> case val t' of
    Amb -> go [] pre $ M.toList (succs t') : []
    _   -> []

  where
  go acc pre assocs = case assocs of
    []        -> error "saw lone empty list while searching for ambiguities"
    [[]]      -> acc
    [] : rest -> go acc (init pre) rest
    _         -> descend assocs
    where
    descend ((top : bottom) : rest) = go acc' pre' assocs'
      where
      ( c, t'' ) = top

      assocs'    = M.toList (succs t'') : bottom : rest

      pre'       = pre ++ return c
      acc'       = case val t'' of
        Key _ -> pre' : acc
        Nil   -> error "saw Nil on descent"
        _     -> acc

fromList :: [( String, a )] -> Trie a
fromList assoc = foldl consume empty assoc
  where
  consume t ( k, v ) = add t k v
