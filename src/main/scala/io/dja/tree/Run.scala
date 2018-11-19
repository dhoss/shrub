package io.dja.tree
object Run {

  def main(args: Array[String]): Unit = {
    println("tree")
    val tree = new AvlTree
    tree.insert(1)
    tree.insert(2)
    tree.insert(3)
    tree.insert(1)
    tree.traverse()
  }

  class AvlTree {
    var rootNode: Node = _
    var data: Int = _

    def insert(data: Int): Unit = {
      rootNode = insertNode(rootNode, data)
    }

    def insertNode(currentNode: Node, nodeData: Int): Node = {
      if (currentNode == null) {
        return new Node(nodeData)
      }

      if (nodeData.compareTo(currentNode.data) < 0) {
        currentNode.left = insertNode(currentNode.left, nodeData)
      } else {
        currentNode.right = insertNode(currentNode.right, nodeData)
      }

      currentNode
    }

    def traverse() = {
      preOrderTraversal(rootNode)
    }

    def preOrderTraversal(node: Node): Unit = {
      val left = node.left
      if (left != null) {
        preOrderTraversal(left)
      }
      println(s"${node}")
      val right = node.right
      if (right != null) {
        preOrderTraversal(right)
      }
    }


  }

  class Node(var data: Int) extends Comparable[Int] {
    var left: Node = _
    var right: Node = _
    var height: Int = _


    override def compareTo(otherData: Int): Int = {
      data - otherData
    }

    override def toString(): String = {
      s"${data}"
    }
  }

}

