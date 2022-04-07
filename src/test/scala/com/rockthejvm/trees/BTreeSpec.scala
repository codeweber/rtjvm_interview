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