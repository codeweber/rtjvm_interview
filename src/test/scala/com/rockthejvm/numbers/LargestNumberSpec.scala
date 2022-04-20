package com.rockthejvm.numbers

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import LargestNumber.*

class LargestNumberSpec extends AnyFlatSpec with Matchers:

    "largestNumber" should "given [10,2] return 210" in {
        largestNumber(List(10,2)) shouldBe "210"
    }

    "largestNumber" should "given [3, 30, 5, 9, 34] return 9534330" in {
        largestNumber(List(3, 30, 5, 9, 34)) shouldBe "9534330"
    }

    "largestNumber" should "given [21, 3] return 321" in {
        largestNumber(List(21, 3)) shouldBe "321"
    }

    "largestNumber" should "given [31, 3] return 331" in {
        largestNumber(List(31, 3)) shouldBe "331"
    }

    "largestNumber" should "given [41, 3] return 413" in {
        largestNumber(List(41, 3)) shouldBe "413"
    }

    "largestNumber" should "given [34, 3] return 343" in {
        largestNumber(List(34, 3)) shouldBe "343"
    }

    "largestNumber" should "given [0,0] return 0" in {
        largestNumber(List(0,0)) shouldBe "0"
    }

    "largestNumber" should "given [] return 0" in {
        largestNumber(List()) shouldBe "0"
    }
