package com.rockthejvm.numbers

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import UglyNumber.*

class UglyNumberSpec extends AnyFlatSpec with Matchers:

    "uglyNumber" should "return true for 1" in {
        uglyNumber(1) shouldBe true
    }

    it should "return true for numbers containing only 2 as a factor" in {
        uglyNumber(2*2*2*2) shouldBe true
    }

    it should "return true for numbers containing only 2 and 3 as a factor" in {
        uglyNumber(2*2*2*2*3*3*3) shouldBe true
    }

    it should "return true for numbers containing only 2,3 and 5 as a factor" in {
        uglyNumber(2*2*2*2*3*3*3*5*5*5*5) shouldBe true
    }

    it should "return false for a number containing a factor other than 2,3 and 5" in {
        uglyNumber(14) shouldBe false
    }

    "nthUgly" should "return 1 for index 1" in {
        nthUgly(1) shouldBe 1
    }

    it should "return 2 for index 2" in {
        nthUgly(2) shouldBe 2
    }

    it should "return 3 for index 3" in {
        nthUgly(3) shouldBe 3
    }

    it should "return 4 for index 4" in {
        nthUgly(4) shouldBe 4
    }

    it should "return 5 for index 5" in {
        nthUgly(5) shouldBe 5
    }

    it should "return 6 for index 6" in {
        nthUgly(6) shouldBe 6
    }

    it should "return 8 for index 7" in {
        nthUgly(7) shouldBe 8
    }
    
    it should "return 9 for index 8" in {
        nthUgly(8) shouldBe 9
    }

    it should "return 10 for index 9" in {
        nthUgly(9) shouldBe 10
    }

    it should "return 12 for index 10" in {
        nthUgly(10) shouldBe 12
    }

    it should "return 15 for index 11" in {
        nthUgly(11) shouldBe 15
    }
