package io.dja.btree

trait Btree {

  def insert(node: Node): Unit

  def remove(nodeId: Int): Unit

  def search(nodeId: Int): Node

  def contains(nodeId: Int): Boolean
  
  case class Node(id: Int, data: String)

}
