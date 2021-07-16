package com.intenthq.challenge

import scala.annotation.tailrec
import scala.collection.mutable.Queue

case class Node(value: Int, edges: List[Node] = List.empty)

object SConnectedGraph {

  // Find if two nodes in a directed graph are connected.
  // Based on http://www.codewars.com/kata/53897d3187c26d42ac00040d
  // For example:
  // a -+-> b -> c -> e
  //    |
  //    +-> d
  // run(a, a) == true
  // run(a, b) == true
  // run(a, c) == true
  // run(b, d) == false
  def run(source: Node, target: Node): Boolean = {
    val nodeQueue = new Queue[Node]()
    nodeQueue.enqueue(source)

    @tailrec
    def areNodesConnected(currentNode: Node, connected: Boolean): Boolean = {
      if (nodeQueue.nonEmpty && !connected) {
        val nodesAreConnected = currentNode.edges.map(_.value == target.value).contains(true)

        if (nodesAreConnected || currentNode.value == target.value) {
          areNodesConnected(currentNode, connected = true)
        } else {
          currentNode.edges.foreach(node => nodeQueue.enqueue(node))

          areNodesConnected(nodeQueue.dequeue(), connected = false)
        }
      } else {
        connected
      }
    }

    areNodesConnected(source, connected = false)
  }

}
