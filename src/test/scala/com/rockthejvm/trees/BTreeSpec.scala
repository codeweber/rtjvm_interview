package com.rockthejvm.trees

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class BTreeSpec extends AnyFlatSpec with Matchers:

    val emptyTree = BEnd
    val depth1Tree = BNode(1, BEnd, BEnd)
    val depth2Tree = BNode(2, BNode(1, BEnd, BEnd), BNode(3, BEnd, BEnd))

    "collectLeaves" should "return 2 leave for depth2Tree" in {
        depth2Tree.collectLeaves should contain theSameElementsInOrderAs List(BNode(1, BEnd, BEnd), BNode(3, BEnd, BEnd))
    }

    it should "return a list of length 1 for depth1Tree" in {
        depth1Tree.collectLeaves should contain theSameElementsInOrderAs List(BNode(1, BEnd, BEnd))
    }

    it should "return an empty list for an empty tree" in {
        emptyTree.collectLeaves shouldBe List()
    }

    "size" should "return 0 for emptyTree" in {
        emptyTree.size shouldBe 0
    }

    it should "return 1 for depth1Tree" in {
        depth1Tree.size shouldBe 1
    }

    it should "return 3 for depth2Tree" in {
        depth2Tree.size shouldBe 3
    }

    "collectNodesAtLevel" should "return bottom nodes for depth2Tree and level 1" in {
        depth2Tree.collectNodesAtLevel(1) should contain theSameElementsInOrderAs List(BNode(1, BEnd, BEnd), BNode(3, BEnd, BEnd))
    }

    it should "return the complete tree for level 0" in {
        depth2Tree.collectNodesAtLevel(0) shouldBe List(depth2Tree)
    }

    it should "return return an empty list for a negative level" in {
        depth2Tree.collectNodesAtLevel(-1) shouldBe List.empty
    }

    it should "return return an empty list for a level equal to depth of tree" in {
        depth2Tree.collectNodesAtLevel(2) shouldBe List.empty
    }


    it should "return return an empty list for a level greater than depth of tree" in {
        depth2Tree.collectNodesAtLevel(3) shouldBe List.empty
    }

    "mirror" should "return an empty tree for an empty tree" in {
        BEnd.mirror shouldBe BEnd
    }

    it should "return a tree that is a leaf unchanged" in {
        val t = BNode(1, BEnd, BEnd)
        t.mirror shouldBe t
    }

    it should "for a tree (1, BEnd, (2, BEnd, BEnd)) return (1, (2, BEnd, BEnd), BEnd)" in {
        val t = BNode(1, BEnd, BNode(2, BEnd, BEnd))
        val tMirrored = BNode(1, BNode(2, BEnd, BEnd), BEnd)
        t.mirror shouldBe tMirrored
    }

    it should "work for a more complex tree" in {
        val t = BNode(
            1, 
            BNode(2, BNode(3, BEnd, BEnd), BNode(4, BEnd, BNode(5, BEnd, BEnd))),
            BNode(6, BNode(7, BEnd, BEnd), BNode(8, BEnd, BEnd))
        )
        val tMirrored = BNode(
            1, 
            BNode(6, BNode(8, BEnd, BEnd), BNode(7, BEnd, BEnd)),
            BNode(2,  BNode(4, BNode(5, BEnd, BEnd), BEnd), BNode(3, BEnd, BEnd))
        )
        t.mirror shouldBe tMirrored
    }

    "sameShapeAs" should "return true for a simple tree" in {
        val t = BNode(1, BNode(2, BEnd, BEnd), BNode(3, BEnd, BNode(4, BEnd, BEnd)))
        val s = BNode(10, BNode(20, BEnd, BEnd), BNode(30, BEnd, BNode(40, BEnd, BEnd)))
        t.sameShapeAs(s) shouldBe true
    }

    it should "return false for a node compared to an end" in {
        BNode(3, BEnd, BEnd).sameShapeAs(BEnd) shouldBe false
    }

    it should "return true for two BEnd" in {
        BEnd.sameShapeAs(BEnd) shouldBe true
    }

    it should "return true for two nodes" in {
        BNode(1, BEnd, BEnd).sameShapeAs(BNode(11, BEnd, BEnd)) shouldBe true
    }

    "toList" should "return in-order travesal" in {
        BNode(1, BNode(2, BEnd, BEnd), BNode(3, BEnd, BEnd)).toList shouldBe List(2,1,3)
    }

    it should "return an empty list on BEnd" in {
        BEnd.toList shouldBe List()
    }
    
    it should "return a single element list from a leaf" in {
        BNode(100, BEnd, BEnd).toList shouldBe List(100)
    }

    it should "return a correctly ordered list for a more complex example" in {
        val t = BNode(
        1, 
        BNode(2, BNode(3, BEnd, BEnd), BNode(4, BEnd, BNode(5, BEnd, BEnd))),
        BNode(6, BNode(7, BEnd, BEnd), BNode(8, BEnd, BEnd))
        )
        t.toList shouldBe List(3,2,4,5,1,7,6,8)
    }