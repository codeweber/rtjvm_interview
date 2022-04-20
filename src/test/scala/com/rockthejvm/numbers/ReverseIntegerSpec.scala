package com.rockthejvm.numbers

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import ReverseInteger.*

class ReverseIntegerSpec extends AnyFlatSpec with Matchers:

    "reverseInteger" should "given 109 return 901" in {
        reverseInteger(109) shouldBe 901
    }

    it should "given 100 return 0" in {
        reverseInteger(100) shouldBe 0
    }

    it should "given Int.MaxValue return 0" in {
        reverseInteger(Int.MaxValue) shouldBe 0
    }

    it should "given -109 return -901" in {
        reverseInteger(-109) shouldBe -901
    }

    it should "given Int.MinValue return 0" in {
        reverseInteger(Int.MinValue) shouldBe 0
    }
