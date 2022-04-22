package com.rockthejvm.graphs

object TownJudge {
  
    def findJudge(numNodes: Int, trustEdges: Set[(Int, Int)]): Option[Int] = 

        type numIn = Int 
        type numOut = Int

        val edgeInOutCount = trustEdges.foldLeft(Map.empty[Int, (numOut, numIn)]){ 
            (m, e) => 
                val (fromNode, toNode) = e
                val edgesFromNode = m.getOrElse(fromNode, (0,0))
                val edgesToNode   = m.getOrElse(toNode, (0,0))
                m + (fromNode -> edgesFromNode.copy(_1 = edgesFromNode._1 + 1)) + (toNode -> edgesToNode.copy(_2 = edgesToNode._2 + 1))
        }

        edgeInOutCount.filter((k,v) => v._1 == 0 && v._2 == numNodes - 1).headOption.map(_._1)
}
