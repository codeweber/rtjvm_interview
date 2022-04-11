package com.rockthejvm.lists

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class RListSpec extends AnyFlatSpec with Matchers:

    "RNil" should "print correctly" in {

        RNil.toString shouldBe "[]"

    }

    "RCons" should "print correctly" in {

        (1 :+: 2 :+: 3 :+: RNil).toString shouldBe "1 :: 2 :: 3 :: []"

    }

    it should "construct correctly" in {
        

        val l = 1 :+: 2 :+: RNil
        l.toString shouldBe "1 :: 2 :: []"

    }

    val l = 1 :+: 2 :+: 3 :+: 4 :+: RNil

    "apply" should "return the kth element" in {

        l(1) shouldBe 2

    }

    it should "throw an exception when applied to an empty list" in {
        an [NoSuchElementException] should be thrownBy RNil(2)
    }

    it should "throw an exception when index is out of bounds" in {
        an [NoSuchElementException] should be thrownBy l(5)
    }

    "length" should "correctly return the length of a string" in {
        l.length shouldBe 4
    }

    it should "be 0 for an empty list" in {
        RNil.length shouldBe 0
    }

    it should "be stack safe" in {
        val longList = (1 to 100000).toList.foldLeft(RList.empty[Int])((rlist, n) => n :+: rlist)
        longList.length shouldBe 100000
    }

    "reverse" should "return an empty list for an empty list" in {
        RNil.reverse shouldBe RNil
    }

    it should "return a reversed list for a non-empty list" in {
        val l = 1 :+: 2 :+: 3 :+: RNil 
        val lReversed = 3 :+: 2 :+: 1 :+: RNil 
        l.reverse shouldBe lReversed
    }

    "from" should "return a RList with the same order as input iterator" in {
        RList.from(1 to 4) shouldBe 1 :+: 2 :+: 3 :+: 4 :+: RNil
    }

    it should "return a RList with the correct size" in {
        val numElements = 10000
        RList.from(1 to numElements).length shouldBe numElements
    }

    "++" should "concatenate two lists" in {
        val firstList = 1 :+: 2 :+: RNil 
        val secondList = 11 :+: 21 :+: RNil

        firstList ++ secondList shouldBe 1 :+: 2 :+: 11 :+: 21 :+: RNil 

    }

    it should "return the non-empty list, when one list non-empty" in {
        val l = 1 :+: 2 :+: RNil
        RNil ++ l shouldBe l
    }

    "removeAt" should "return a list with the kth element absent" in {
        val l = RList.from(0 to 5)
        l.removeAt(3) shouldBe RList.from((0 to 5).toList.filter(_ != 3))
    }

    it should "leave an empty list unchanged" in {
        RNil.removeAt(1) shouldBe RNil
    }

    it should "leave a list unchanged if index is out of bounds" in {
        val listLength = 10
        val l = RList.from(0 until listLength)
        l.removeAt(listLength) shouldBe l
    }

    "map" should "transform element in the list" in {
        val l = RList.from(1 to 10)
        l.map(_ + 1) shouldBe RList.from(2 to 11)
    }

    "flatMap" should "trasform each element in the list to a list and concat results" in {
        val l = RList.from(1 to 3)
        l.flatMap(x => RList.from(1 to x)) shouldBe RList.from( (1 to 3).toList.flatMap(n => (1 to n).toList))
    }

    "filter" should "return only those elements that satisfy the predicate" in {
        val l = RList.from(0 to 10)
        l.filter(_ % 2 == 0) shouldBe RList.from(0 to 10 by 2)
    }
      
    "rle" should "return a correct run-length encoding" in {
        val l = RList.from(1 to 10).flatMap(n => RList.from(List.fill(n)(n)))
        l.rle shouldBe RList.from(1 to 10).map(n => (n,n))
    }

    it should "return an empty list when acting upon an empty list" in {
        RNil.rle shouldBe RNil
    }

    it should "return a list with counts of 1 when all elements are unique" in {
        RList.from(1 to 10).rle shouldBe RList.from(1 to 10).map(x => (x,1))
    }

    it should "return a correct run-length encoding when duplicates are present" in {
        val basicList = (1 to 10).toList ++ (10 to 1 by -1).toList
        val l = RList.from(basicList)
        l.rle shouldBe RList.from(basicList.foldLeft(List.empty[(Int, Int)])((b,a) => if !b.isEmpty && b.head._1 == a then (a,b.head._2+1)::b.tail else (a,1) :: b).reverse)
    }

    "duplicateEach" should "replicate each element the required number of times" in {
        val l = RList.from(1 to 3)
        l.duplicateEach(3) shouldBe l.flatMap(x => x :+: x :+: x :+: RNil )
    }

    it should "return an empty list when applied to an empty list" in {
        RNil.duplicateEach(3) shouldBe RNil
    }

    it should "return the same list when duplicateEach is 1" in {
        val l = RList.from(1 to 10)
        l.duplicateEach(1) shouldBe l
    }

    "rotate" should "rotate a list to the left, when shift is less than length of list" in {
        val l = RList.from(1 to 10)
        l.rotate(3) shouldBe (RList.from(4 to 10) ++ RList.from(1 to 3))
    }

    it should "leave a list unchanged if shifted by length of the list" in {
        val l = RList.from(1 to 10)
        l.rotate(10) shouldBe l
    }

    it should "rotate list in a wrap around fashion when shift is greater than length of list" in {
        val l = RList.from(1 to 10)
        l.rotate(13) shouldBe (RList.from(4 to 10) ++ RList.from(1 to 3))
    }

    it should "accept negative shifts and interpret as shift to the right" in {
        val l = RList.from(1 to 10)
        l.rotate(-1) shouldBe RList.from(10 to 10) ++ RList.from(1 to 9)
    }

    it should "leave an empty list unchanged" in {
        RNil.rotate(10) shouldBe RNil
    }

    "sample" should "return a list of the desired length" in {
        RList.from(1 to 10).sample(100).length shouldBe 100
    }