package com.rockthejvm.trees

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import PathSum.*

class PathSumSpec extends AnyFlatSpec with Matchers:

    val simpleTree = BNode(1, BNode(10, BEnd, BEnd), BNode(100, BEnd, BEnd))

    val testTree = BNode(
        1, 
        BNode(2, BNode(3, BEnd, BEnd), BNode(4, BEnd, BNode(5, BEnd, BEnd))),
        BNode(6, BNode(7, BEnd, BEnd), BNode(8, BEnd, BEnd))
    )

    val testTree2 = BNode(
        1, 
        BNode(2, BNode(3, BEnd, BEnd), BNode(4, BEnd, BNode(7, BEnd, BEnd))),
        BNode(6, BNode(7, BEnd, BEnd), BNode(8, BEnd, BEnd))
    )

    "hasPathSum" should "return case 1" in {
        hasPathSum(testTree, 15) shouldBe true
    }

    it should "always return false for a BEnd" in {
        hasPathSum(BEnd, 0) shouldBe false
    }

    it should "return true for a simple tree via a left traversal" in {
        hasPathSum(simpleTree, 11) shouldBe true
    }
    
    it should "return true for a simple tree via a right traversal" in {
        hasPathSum(simpleTree, 101) shouldBe true
    }

    it should "return false for a simple tree where no path exists" in {
        hasPathSum(simpleTree, 111) shouldBe false
    }

    "findSumPaths" should "return a list of List[Int]" in {
        findSumPaths(simpleTree, 101) shouldBe List(List(1,100))
    }

    it should "return a list of List[Int] with multiple entries" in {
        findSumPaths(testTree2, 14) should contain theSameElementsAs List(List(1,6,7), List(1,2,4,7))
    }