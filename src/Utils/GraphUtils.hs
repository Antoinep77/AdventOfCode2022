{-# LANGUAGE TupleSections #-}
module Utils.GraphUtils
  ( dijkstra,
  dijkstraMultipleEnds
  )
where
import Utils.Utils

import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Maybe


type Queue a = ([a],[a])

singleton :: a -> Queue a
singleton x = ([],[x])

pop :: Queue a -> (Maybe a ,Queue a)
pop ([],[]) = (Nothing, ([],[]))
pop (l1, h:t) = (Just h, (l1,t))
pop (l, []) = pop ([], reverse l)

add :: Queue a -> a -> Queue a
add (l1,l2) x = (x:l1,l2)


addAll :: Queue a -> [a] -> Queue a
addAll = foldl add

dijkstra :: (Ord a, Show a) => Map.Map a [a] -> a -> a -> Maybe (Int,[a])
dijkstra adjacency start end  = dijkstraWithVisited (Set.singleton start) (singleton (start,0)) adjacency
  where
    dijkstraWithVisited visited queue adjacency
        | isNothing nextStartMaybe = Nothing
        | nextStart == end = Just (dist, [nextStart])
        | otherwise = fmap (nextStart:) <$> dijkstraWithVisited newVisited newQueue adjacency
      where
        newQueue = addAll nextQueue $ map (,dist+1) newNeighbours
        newVisited = Set.union (Set.fromList newNeighbours) visited
        newNeighbours = filter ( not . (`Set.member` visited)) neighbours
        neighbours = getOrElse (Map.lookup nextStart adjacency) []
        Just (nextStart,dist) = nextStartMaybe
        (nextStartMaybe,nextQueue) = pop queue


dijkstraMultipleEnds :: (Ord a, Show a) => Map.Map a [a] -> a -> Set.Set a -> Maybe (Int,[a])
dijkstraMultipleEnds adjacency start ends  = dijkstraWithVisited (Set.singleton start) (singleton (start,0)) adjacency
  where
    dijkstraWithVisited visited queue adjacency
        | isNothing nextStartMaybe = Nothing
        | Set.member nextStart ends = Just (dist, [nextStart])
        | otherwise = fmap (nextStart:) <$> dijkstraWithVisited newVisited newQueue adjacency
      where
        newQueue = addAll nextQueue $ map (,dist+1) newNeighbours
        newVisited = Set.union (Set.fromList newNeighbours) visited
        newNeighbours = filter ( not . (`Set.member` visited)) neighbours
        neighbours = getOrElse (Map.lookup nextStart adjacency) []
        Just (nextStart,dist) = nextStartMaybe
        (nextStartMaybe,nextQueue) = pop queue