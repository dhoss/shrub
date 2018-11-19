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
    var data: Int = _

    def insert(data: Int): Unit = {
      rootNode = insertNode(rootNode, data)
    }

    // nodeData could really be any uniquely identifiable key
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

    def height(root: Node): Int = {
      var leftHeight: Int = 0
      var rightHeight: Int = 0
      var currentNode = root
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
    def balanceFactor(root: Node): Int = {
      height(root.right) - height(root.left)
    }

    // binary search for a node
    def search(root: Node, key: Int): Node = {
      if (root == null || root.data == key) {
        return root
      }

      if (root.data > key) {
        return search(root.left, key)
      }

      search(root.right, key)
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
  }

  class Node(var data: Int) extends Comparable[Int] {
    var left: Node = _
    var right: Node = _

    override def compareTo(otherData: Int): Int = {
      data - otherData
    }

    override def toString(): String = {
      s"${data}"
    }
  }

}

