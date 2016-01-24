module Datastructures.Queue where

{-| This module implements a simple LIFO queue

# Definition
@docs Queue

This is based on the 

# Fundamentals 
@docs init, enqueue, dequeue, length

# Useful functions
@docs foldr, foldl, map, fromList, toList

-}

{-| A simple queue.
-}
type alias Queue a = (List a, List a)

{-| Creates an empty queue -}
                   
init : Queue a
init = ([], [])


{-| Enqueue an element on a queue -}
enqueue :  a -> Queue a -> Queue a
enqueue a (inqueue, outqueue) =
  ((a::inqueue), outqueue)

{-| Dequeues an element off the end of a queue, and also returns the
updated queue with that element removed -}

dequeue : Queue a -> (Maybe a, Queue a)
dequeue (inqueue,outqueue) =
  case outqueue of
    [] ->
      case inqueue of
        [] -> (Nothing,(inqueue, outqueue))

        (_::_) -> dequeue([], List.reverse inqueue)
                   
    (x::xs) ->
      (Just x, (inqueue, xs))


{-| Get the length(number of elements) in the queue -}
length : Queue a -> Int
length (inqueue, outqueue) =
  let
    inqueue_len  = List.length inqueue
    outqueue_len = List.length outqueue
  in
    inqueue_len + outqueue_len
  
{-| Fold across a queue front to back -}

foldr : ( a -> b -> b) -> b -> Queue a -> b
foldr f acc (inqueue, outqueue) =
  List.foldl f (List.foldr f acc inqueue) outqueue

{-| Fold across a queue back to front -}

foldl : ( a -> b -> b) -> b -> Queue a -> b
foldl f acc (inqueue, outqueue) =
  List.foldr f (List.foldl f acc outqueue) inqueue


{-| maps from a queue of type a to a queue containing elements of type
b -}

map : (a -> b) -> Queue a -> Queue b
map f (inqueue, outqueue) =
  (List.map f inqueue, List.map f outqueue)


{-| converts a queue into a list -}

fromList : List a -> Queue a
fromList l =
  (l, [])

  
{-| converts a list into a queue -}
  
toList : Queue a -> List a
toList (inqueue, outqueue) =
  inqueue ++ (List.reverse outqueue)
