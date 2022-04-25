package com.rockthejvm.trees

import scala.annotation.tailrec

object PathSum:

    def hasPathSum(tree: BTree[Int], target: Int): Boolean = 

        // A short, stack-recursive definition
        /* 
        tree match
            case BEnd => false
            case BNode(v, l, r) => (v == target) || hasPathSum(l, target-v) || hasPathSum(r, target-v)
         */

        @tailrec
        def hasPathSumTailrec(paths: List[(Int, BTree[Int])]): Boolean =
            
            paths match
                case Nil => false 
                case (pathTotal, lastNode) :: pts => 
                    lastNode match 
                        case BEnd => hasPathSumTailrec(pts)
                        case BNode(v, BEnd, BEnd) => (pathTotal + v == target) || hasPathSumTailrec(pts)
                        case BNode(v, l, r) => hasPathSumTailrec((pathTotal + v, l) :: (pathTotal + v, r) :: pts)

        hasPathSumTailrec(List((0,tree)))

    def findSumPaths(tree: BTree[Int], target: Int): List[List[Int]] =

        @tailrec
        def findSumPathsTailrec(pathsToExplore: List[(Int, List[Int], BTree[Int])], pathsSuccessful: List[List[Int]]): List[List[Int]] =
            
            pathsToExplore match
                case Nil => pathsSuccessful 
                case (pathTotal, path, lastNode) :: pts => 
                    lastNode match 
                        case BEnd => findSumPathsTailrec(pts, pathsSuccessful)
                        case BNode(v, BEnd, BEnd) => 
                            if (pathTotal + v == target) then  
                                val newSuccessfulPath = (v :: path).reverse
                                findSumPathsTailrec(pts, newSuccessfulPath :: pathsSuccessful)
                            else 
                                findSumPathsTailrec(pts, pathsSuccessful)
                        case BNode(v, l, r) => 
                            val newPathTotal = pathTotal + v
                            val newPath = v :: path
                            val newPathsToExplore = List(l, r).filter(!_.isEmpty).map(n => (newPathTotal, newPath, n))
                            findSumPathsTailrec(pts.prependedAll(newPathsToExplore), pathsSuccessful)

        findSumPathsTailrec(List((0, List(),tree)), List())

                    