package com.rockthejvm.trees

import scala.annotation.tailrec

sealed abstract class BTree[+T]:
  def value: T
  def left: BTree[T]
  def right: BTree[T]

  def isEmpty: Boolean

  def isLeaf: Boolean 
  def collectLeaves: List[BTree[T]]
  def leafCount: Int

  def size: Int

  def collectNodesAtLevel(level: Int): List[BTree[T]]


case object BEnd extends BTree[Nothing]:
  override def value = throw new NoSuchElementException 
  override def left = throw new NoSuchElementException
  override def right = throw new NoSuchElementException
  override def isEmpty: Boolean = true 

  override def isLeaf = false
  override def collectLeaves = List.empty
  override def leafCount = 0

  override val size = 0

  override def collectNodesAtLevel(level: Int) = List.empty

end BEnd

case class BNode[+T](override val value: T, override val left: BTree[T], override val right: BTree[T]) extends BTree[T]:
  override def isEmpty: Boolean = false

  override def isLeaf = left.isEmpty && right.isEmpty
  override def collectLeaves =

    @tailrec
    def trCollectLeaves(t: BTree[T], toExplore: List[BTree[T]], leaves: List[BTree[T]]): List[BTree[T]] =
      if t.isLeaf then 
        toExplore match
          case Nil => t :: leaves 
          case x :: xs => trCollectLeaves(x, xs, t :: leaves)
      else if t.isEmpty then
        toExplore match
          case Nil => leaves 
          case x :: xs => trCollectLeaves(x, xs, leaves)
      else
        trCollectLeaves(t.right, t.left :: toExplore, leaves )

    trCollectLeaves(this, List.empty, List.empty)

  override def leafCount =
    collectLeaves.size

  def sizeMethod =

    @tailrec
    def trGetSize(todo: List[BTree[T]], numNodes: Int): Int =
      
      todo match
        case Nil => numNodes
        case t :: ts => 
          if t.isEmpty then 
            trGetSize(ts, numNodes)
          else if t.isLeaf then 
            trGetSize(ts, numNodes + 1)
          else 
            trGetSize(t.right :: t.left :: ts, numNodes + 1)

    trGetSize(List(this), 0)

  override val size = 1 + left.size + right.size 
  // Note this is Stack safe, since it is a val. There are no method calls.
  // This is possible, since the tree is IMMUTABLE

  override def collectNodesAtLevel(level: Int): List[BTree[T]] =

    @tailrec
    def trCollectNodes(todo: List[(Int, BTree[T])], nodesAtLevel: List[BTree[T]]): List[BTree[T]] = 
      todo match
        case Nil => nodesAtLevel
        case (tLevel, t) :: ts => 
          if tLevel < level then
            if t.isEmpty || t.isLeaf then 
              trCollectNodes(ts, nodesAtLevel)
            else 
              trCollectNodes( (tLevel + 1, t.right) :: (tLevel + 1, t.left) :: ts, nodesAtLevel)
          else if tLevel == level then 
            if t.isEmpty then 
              trCollectNodes(ts, nodesAtLevel)
            else 
              trCollectNodes(ts, t :: nodesAtLevel)
          else 
              throw new IllegalStateException("We should never process this part of the tree")

    if level < 0 then 
      List.empty
    else 
      trCollectNodes(List((0, this)), List.empty)


      


