module QueueTest where

import Datastructures.Queue as Queue

import Check exposing (..)
import Check.Investigator exposing (..)


claim_order_maintained =
  claim "order maintained"
          `that`
                 (\list -> Queue.toList (Queue.fromList (list)))
          `is`
               (\list -> list)
          `for`
                (list int)

claim_order_maintained_enqueue =
  claim "order maintained_enqueue"
          `that`
                 (\(list, e)->
                   let
                     queue = Queue.fromList list
                   in
                     Queue.toList (Queue.enqueue e queue)
                 )
          `is`
               (\(list, e) -> e::list )
          `for`
                tuple ((list int), int)


claim_order_maintained_dequeue =
  claim "order maintained_dequeue"
          `that`
                 (\(list, e)->
                   let
                     queue = Queue.fromList list
                     (_, queue') = (Queue.dequeue queue)
                   in
                     Queue.toList (queue')
                 )
          `is`
               (\(list, e) -> let len = List.length list
                                  list' = List.take (len - 1) list 
                              in list')
          `for`
                tuple ((list int), int)

                
claim_operations_eguevalence =
  claim "claim_operations_eguevalence"
          `that`
                 (\ops->
                   let
                     queue = Queue.init
                     (_, _, steps) = List.foldr (\op (q, i, steps) ->
                                         case op of
                                           True ->
                                             let q' = Queue.enqueue i q
                                             in (q', i+1, (q' :: steps))
                                           False ->
                                             let (_, q') = Queue.dequeue q
                                             in (q',i, (q' :: steps))
                     
                                         ) (queue, 0, [] ) ops
                   in
                     List.map Queue.toList steps
                 )
          `is`
                 (\ops->
                   let
                     queue = []
                     (_, _, steps) = List.foldr (\op (q, i, steps) ->
                                         case op of
                                           True ->
                                             (i::q, i+1, ((i::q) :: steps))
                                           False ->
                                             let
                                               q' = List.take ((List.length q) - 1) q
                                             in (q',i, (q'::steps))
                     
                                         ) (queue, 0, []) ops
                   in
                     steps
                 )

          `for`
                (list bool)


claim_correct_length =
  claim "claim_correct_length"
          `that`
                 (\ops->
                   let
                     queue = Queue.init
                     (_, _, steps) = List.foldr (\op (q, i, steps) ->
                                         case op of
                                           True ->
                                             let
                                               q' = Queue.enqueue i q
                                               l  = Queue.length q'
                                             in (q', i+1, ((q',l) :: steps))
                                           False ->
                                             let
                                               (_, q') = Queue.dequeue q
                                               l  = Queue.length q'
                                             in (q',i, ((q',l) :: steps))
                     
                                         ) (queue, 0, [] ) ops
                   in
                     List.map (\(q, l) -> (Queue.toList q, l))  steps
                 )
          `is`
                 (\ops->
                   let
                     queue = []
                     (_, _, steps) = List.foldr (\op (q, i, steps) ->
                                         case op of
                                           True ->
                                             let q' = (i::q)
                                                 l = List.length q'
                                             in
                                               (q', i+1, ((q', l) :: steps))
                                           False ->
                                             let
                                               q' = List.take ((List.length q) - 1) q
                                               l = List.length q'
                                             in (q',i, ((q', l)::steps))
                     
                                         ) (queue, 0, []) ops
                   in
                     steps
                 )

          `for`
                (list bool)

                

suite_reverse =
  suite "Queue Suite"
    [ claim_order_maintained
    , claim_order_maintained_enqueue
    , claim_order_maintained_dequeue
    , claim_operations_eguevalence
    , claim_correct_length]

    
result = quickCheck suite_reverse
