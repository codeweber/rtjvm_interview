package com.rockthejvm.graphs

import GraphProblems.{Graph, existsLoop}
import scala.annotation.tailrec

object UniCourses {

    private def createGraph(n: Int, edges: List[(Int, Int)]): Graph[Int] =
            val gInit = (0 until n).map(_ -> Set.empty[Int]).toMap
            edges.foldLeft(gInit)( (g, e) => g + (e._1 -> (g.getOrElse(e._1, Set())+e._2)) )
  
    def canTakeAllCourses(nCourses: Int, prerequisities: List[(Int, Int)]): Boolean = 

        // convert into a graph
        // check to see if there are any cycles
        val graph = createGraph(nCourses, prerequisities)
        !existsLoop(graph)

    def findOrder(nCourses: Int, prerequisities: List[(Int, Int)]): List[Int] = 

        @tailrec
        def loop(remaining: Graph[Int], coursesStack: List[Int], coursesTaken: Set[Int]): List[Int] = 
            if remaining.isEmpty then
                coursesStack
            else 
                val nextCourses = remaining.filter((_,v) => v.subsetOf(coursesTaken)).keySet
                if nextCourses.isEmpty then
                    Nil 
                else
                    loop(remaining.removedAll(nextCourses), coursesStack ++ nextCourses.toList.sorted, coursesTaken ++ nextCourses)

        val g = createGraph(nCourses, prerequisities)
        loop(g, List(), Set())

}
