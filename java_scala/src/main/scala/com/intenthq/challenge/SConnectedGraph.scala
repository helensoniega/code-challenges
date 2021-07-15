package com.intenthq.challenge

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
    var currentNode = source
    var connected = false

    nodeQueue.enqueue(currentNode)

    //While queue isn't empty and not connected
    while(nodeQueue.nonEmpty && !connected) {
      currentNode = nodeQueue.dequeue()

      val nodesAreConnected = currentNode.edges.map(_.value == target.value).contains(true)

      if (nodesAreConnected || currentNode.value == target.value) {
        connected = true
      }

      if (currentNode.edges.nonEmpty) {
        currentNode.edges.foreach( node =>
          nodeQueue.enqueue(node)
        )
      }

    }

    connected
  }

}
