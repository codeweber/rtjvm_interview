package com.rockthejvm.graphs

import scala.annotation.tailrec
import scala.collection.immutable.Queue

object GraphProblems:

    type Graph[T] = Map[T, Set[T]]

    val socialNetwork: Graph[String] = Map(
        "A" -> Set("B", "C", "D"),
        "B" -> Set(),
        "C" -> Set("D"),
        "D" -> Set("B", "E"),
        "E" -> Set("B", "C")
    )

    def isPathSimple[T](g: Graph[T], start: T, end: T): Boolean = 

        g.get(start) match
            case None => false 
            case Some(children) => 
                children.contains(end) || children.exists(n => isPath(g - start, n, end))


    def isPath[T](g: Graph[T], start: T, end: T): Boolean = 

        @annotation.tailrec 
        def isPathTailRec(frontier: List[T], visited: Set[T]): Boolean = 

            frontier match
                case Nil => false
                case n :: ns => 
                    if n == end then 
                        true
                    else if visited(n) then 
                        isPathTailRec(ns, visited)
                    else
                        val nextNodes = g.getOrElse(n, Set.empty[T]).filter(!visited(_)).toList
                        isPathTailRec(nextNodes ++ ns, visited + n )

        isPathTailRec(g.getOrElse(start, Set.empty[T]).toList, Set(start))

    def findPath[T](g: Graph[T], startNode: T, endNode: T): List[T] = 

        @tailrec
        def findPathTailRec(paths: Queue[List[T]], currentPath: List[T], frontier: List[T], visited: Set[T]): List[T] = 

            frontier match 
                case Nil => 
                    if paths.isEmpty then 
                        Nil
                    else
                        val (newPath, newPaths) = paths.dequeue
                        val newFrontier = g.getOrElse(newPath.head, Set()) diff visited
                        findPathTailRec(newPaths, newPath, newFrontier.toList, visited)
                case n :: ns =>
                    val newPath = n :: currentPath
                    if n == endNode then 
                        newPath.reverse
                    else 
                        findPathTailRec(paths.enqueue(elem = newPath), currentPath, ns, visited + n)

        val p = Queue.empty[List[T]]
        val initialNeighbours: Set[T] = g.getOrElse(startNode, Set.empty[T])
        val initialPaths: List[List[T]] = initialNeighbours.toList.map(_ :: List(startNode))
        
        if endNode == startNode then
            // permit searching for a single loop
            findPathTailRec(p.enqueueAll(initialPaths), Nil, Nil, initialNeighbours)
        else 
            // avoid loops, so add startNode to visited
            findPathTailRec(p.enqueueAll(initialPaths), Nil, Nil, initialNeighbours + startNode)

    def existsLoop[T](g: Graph[T]): Boolean =

        g.keySet.exists(n => !findPath(g, n, n).isEmpty)


    def makeUndirected[T](graph: Graph[T]): Graph[T] = 

        @tailrec
        def makeUndirectedTailRec(remainingGraph: Graph[T], newGraph: Graph[T]): Graph[T] =

            if remainingGraph.isEmpty then
                newGraph 
            else 
                val (node, edges) = remainingGraph.head 
                val newRemainingGraph = remainingGraph.tail.map((n, es) => if edges(n) then (n, es+node) else (n, es))
                val newEdges = edges ++ remainingGraph.tail.filter(_._2(node)).keySet
                makeUndirectedTailRec(newRemainingGraph, newGraph + (node -> newEdges))

        makeUndirectedTailRec(graph, Map() )




