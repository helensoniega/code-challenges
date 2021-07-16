package com.intenthq.challenge;

import scala.annotation.tailrec

object SEnigma {

  // We have a system to transfer information from one place to another. This system
  // involves transferring only list of digits greater than 0 (1-9). In order to decipher
  // the message encoded in the list you need to have a dictionary that will allow
  // you to do it following a set of rules:
  //    > Sample incoming message: (​1,2,3,7,3,2,3,7,2,3,4,8,9,7,8)
  //    > Sample dictionary (​23->‘N’,234->‘ ’,89->‘H’,78->‘Q’,37 ->‘A’)
  //  - Iterating from left to right, we try to match sublists to entries of the map.
  //    A sublist is a sequence of one or more contiguous entries in the original list,
  //    eg. the sublist (1, 2) would match an entry with key 12, while the sublist (3, 2, 3)
  //    would match an entry with key 323.
  //  - Whenever a sublist matches an entry of the map, it’s replaced by the entry value.
  //    When that happens, the sublist is consumed, meaning that its elements can’t be used
  //    for another match. The elements of the mapping however, can be used as many times as needed.
  //  - If there are two possible sublist matches, starting at the same point, the longest one
  //    has priority, eg 234 would have priority over 23.
  //  - If a digit does not belong to any matching sublist, it’s output as is.
  //
  // Following the above rules, the message would be: “1N73N7 HQ”
  // Check the tests for some other (simpler) examples.

  def deciphe(map: Map[Int, Char])(message: List[Int]): String = {

    @tailrec
    def decipherMessage(msg: List[Int], keySize: Int = 1, result: String = ""): String = {
      msg match {
        case Nil => result
        case head :: tail =>
          val nextKeySize = keySize + 1
          val currentKey = msg.take(keySize).mkString.toInt
          val nextKey = msg.take(nextKeySize).mkString.toInt

          (map.get(currentKey), map.get(nextKey)) match {
            case (_, Some(value)) =>
              /*If nextKey is valid
                If removing keys from msg is non empty then return full msg because we don't want to drop anything if
                the nextKey is valid. Nothing will be added to result too. Next case will add it to result*/
              val (listToPass, res) =
                if (msg.drop(nextKeySize).nonEmpty) {
                  (msg, "")
                } else {
                  (msg.drop(nextKeySize), value.toString)
                }

              decipherMessage(listToPass, keySize = nextKeySize, result = result + res)
            case (Some(value), _) =>
              //If next key isn't valid
              decipherMessage(msg.drop(keySize), keySize = 1, result = result + value)
            case _ =>
              //Key isn't found so add key to result
              decipherMessage(tail, keySize = 1, result = result + head)
          }
      }
    }

    decipherMessage(message)
  }

}
