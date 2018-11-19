package io.dja.tree

object Run {

  def main(args: Array[String]): Unit = {
    println("tree")
    val tree = new AvlTree
    tree.insert(new Node(1))
    tree.insert(new Node(2))
    tree.insert(new Node(3))
    tree.insert(new Node(1))
    tree.traverse()
    println("search for a key that exists: 3")
    println(tree.search(tree.rootNode, 3))
    println("search for a key that doesn't exist: 5")
    println(tree.search(tree.rootNode, 5))
    println(s"Tree balance factor: ${tree.balanceFactor(tree.rootNode)}")
    println(s"Tree balance factor at node 3: ${tree.balanceFactor(tree.search(tree.rootNode, 2))}")

    println(s"Tree height: ${tree.height(tree.rootNode)}")
    println(s"Tree height at node 3: ${tree.height(tree.search(tree.rootNode, 2))}")
  }

  class AvlTree {
    var rootNode: Node = _

    def insert(node: Node): Unit = {
      rootNode = insertNode(rootNode, node)
    }

    def insertNode(node: Node, newNode: Node): Node = {
      val nodeData = newNode.key
      if (node == null) {
        return new Node(nodeData)
      }
      if (nodeData.compareTo(node.key) < 0) {
        node.left = insertNode(node.left, newNode)
      } else {
        node.right = insertNode(node.right, newNode)
      }

      node
    }

    def height(node: Node): Int = {
      var leftHeight: Int = 0
      var rightHeight: Int = 0
      var currentNode = node
      while (currentNode != null) {
        if (currentNode.left != null) {
          leftHeight += 1
          currentNode = currentNode.left
        }
        rightHeight += 1
        currentNode = currentNode.right
      }
      // return the greater of the two numbers because that will be the total depth of the (sub)tree
      if (leftHeight > rightHeight) leftHeight else rightHeight
    }

    // Determine whether we're AVL tree height compliant
    def balanceFactor(node: Node): Int = {
      height(node.right) - height(node.left)
    }

    // binary search for a node
    def search(node: Node, key: Int): Node = {
      if (node == null || node.key == key) {
        return node
      }

      if (node.key > key) {
        return search(node.left, key)
      }

      search(node.right, key)
    }

    def traverse() = {
      preOrderTraversal(rootNode)
    }

    // TODO: pass in a function to execute instead of just printing
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
/*
    def rebalance(node: Node): Node = {
      // left > right
      if (balanceFactor(node) > 1) {

      }
    }
    */
  }

  class Node(var key: Int) extends Comparable[Int] {
    var parent: Node = _
    var left: Node = _
    var right: Node = _
    var data: Int = _

    override def compareTo(otherData: Int): Int = {
      data - otherData
    }

    override def toString(): String = {
      s"${key}"
    }
  }

}

