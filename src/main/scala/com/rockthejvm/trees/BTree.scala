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

  def mirror: BTree[T]

  def sameShapeAs[S >: T](that: BTree[S]): Boolean

  def toList: List[T]


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

  override def mirror: BTree[Nothing] = this

  override def sameShapeAs[S >: Nothing](that: BTree[S]): Boolean =
    that == BEnd

  override def toList: List[Nothing] = Nil

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


  override def mirror: BTree[T] =

    @tailrec
    def mirrorTailRec(toExplore: List[BTree[T]], nodeStack: List[BTree[T]], childStack: List[BTree[T]]): BTree[T] = 

      toExplore match 
        case Nil => childStack.head 
        case t :: ts => 
          t match 
            case BEnd => mirrorTailRec(ts, nodeStack, BEnd :: childStack)
            case leaf @ BNode(v, BEnd, BEnd) => mirrorTailRec(ts, nodeStack, leaf :: childStack)
            case node @ BNode(v, l, r) if nodeStack.headOption == Some(node) => 
              childStack match 
                case l :: r :: subtrees =>   
                  val newNode = BNode(v, l, r)
                  mirrorTailRec(ts, nodeStack.tail, newNode :: subtrees)
                case _ => mirrorTailRec(ts, nodeStack.tail, childStack)
            case node @ BNode(v, l, r) => mirrorTailRec(l :: r :: t :: ts, node :: nodeStack, childStack)
 
    mirrorTailRec(List(this), List(), List())
            
  override def sameShapeAs[S >: T](that: BTree[S]): Boolean = 

    @tailrec
    def loop(thisSubtrees: List[BTree[T]], thatSubTrees: List[BTree[S]]): Boolean =

      (thisSubtrees, thatSubTrees) match
        case (Nil, Nil) => true 
        case (Nil, _) => false 
        case (_, Nil) => false 
        case (x :: xs, y :: ys) =>
          (x, y) match
            case (BEnd, BEnd) => loop(xs, ys)
            case (BNode(_, BEnd, BEnd), BNode(_, BEnd, BEnd)) => loop(xs, ys)
            case (BNode(_, lx, rx), BNode(_, ly, ry)) => loop(lx :: rx :: xs, ly :: ry :: ys)
            case _ => false

    loop(List(this), List(that))

  override def toList: List[T] =
    //left.toList ++ List(value) ++ right.toList

    @tailrec
    def toListTailrec(toExplore: List[BTree[T]], agg: List[T]): List[T] =
      if toExplore.isEmpty then 
        agg.reverse
      else 
        val node = toExplore.head 
        node match
          case BEnd => toListTailrec(toExplore.tail, agg)
          case BNode(v, BEnd, BEnd) => toListTailrec(toExplore.tail, v :: agg) 
          case BNode(v, l, r) => 
            //use following for in-order
            toListTailrec(l :: BNode(v, BEnd, BEnd) :: r :: toExplore.tail, agg) 

            //use following for pre-order
            //toListTailrec(BNode(v, BEnd, BEnd) :: l :: r :: toExplore.tail, agg) 

            //use following for post-order
            //toListTailrec(l :: r :: BNode(v, BEnd, BEnd) :: toExplore.tail, agg) 

    toListTailrec(List(this), List())


