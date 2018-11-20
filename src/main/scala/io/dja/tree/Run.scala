package io.dja.tree

object Run {

  def main(args: Array[String]): Unit = {
    println("tree")
    val tree = new BsTree
    tree.insert(new Node(6))
    tree.insert(new Node(3))
    tree.insert(new Node(1))
    tree.insert(new Node(2))
    tree.insert(new Node(4))
    tree.insert(new Node(5))
    tree.inOrder()
  }

  class BsTree {

    var root: Node = _

    def search(root: Node, key: Int): Node = {
      if (root == null || root.key == key) {
        return root
      }
      if (root.key > key) {
        search(root.left, key)
      }
      search(root.right, key)
    }

    def insert(node: Node) = {
      root = insertNode(root, node)
    }

    def insertNode(rootNode: Node, node: Node): Node = {
      val key = node.key
      var root: Node = rootNode
      if (root == null) {
        root = new Node(key)
        return root
      }

      node.parent = root
      println(s"PARENT ${node.parent}")
      if (root.key > key) {
        root.left = insertNode(root.left, node)
      }
      root.right = insertNode(root.right, node)

      root
    }

    def inOrder(): Unit = {
      inOrderTraversal(root)
    }

    def inOrderTraversal(root: Node): Unit = {
      if (root != null) {
        inOrderTraversal(root.left)
        println(root)
        inOrderTraversal(root.right)
      }
    }

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
      s"Node(parent: ${parent} key: ${key} data: ${data})"
    }
  }

}

