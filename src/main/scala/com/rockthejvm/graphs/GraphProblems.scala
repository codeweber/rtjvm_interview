package com.rockthejvm.graphs

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

