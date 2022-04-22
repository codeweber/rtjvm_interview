package com.rockthejvm.strings

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import MultiplyStrings.*

class MultiplyStringsSpec extends AnyFlatSpec with Matchers:

    "convertToDigitsList" should "given 100 return 0 :: 0 :: 1" in {
        convertToDigitsList("100") shouldBe List(0,0,1)
    }

    "convertToString" should "given 0 :: 0 :: 0 return 100" in {
        convertToString(List(0,0,1)) shouldBe "100"
    }

    "normaliseDigits" should "given the inputs (12, 9, 2), 0 and Nil output (2, 0, 3)" in {
        normaliseDigitList(List(12, 9, 2), 0, Nil) shouldBe List(2,0,3)
    }

    "sumDigitsList" should "given (3,2,1) and (9,8,7) return (2,1,9)" in {
        sumDigitLists(List(3,2,1), List(9,8,7)) shouldBe List(2,1,9)
    }

    "multiplyDigitsList" should "given (1,0,2) and (0, 1) output (0, 1, 0, 2)" in {
        multiplyDigitLists(List(1,0,2), List(0,1)) shouldBe List(0,1,0,2)
    }

    it should "given (1,0,2) and (1) output (1, 0, 2)" in {
        multiplyDigitLists(List(1,0,2), List(1)) shouldBe List(1,0,2)
    }

    it should "given (1,0,2) and (1, 1) output (1, 1, 2, 2)" in {
        multiplyDigitLists(List(1,0,2), List(1,1)) shouldBe List(1,1,2,2)
    }

    "multiplyStrings" should "given 1034 and 235 give 242990" in {
        multiplyStrings("1034", "235") shouldBe "242990"
    }

    it should "given 999998 * 102344363 output 102344158311274" in {
        multiplyStrings("999998", "102344363") shouldBe "102344158311274"
    }

    it should "given 0 * 1024 output 0" in {
        multiplyStrings("0", "1024") shouldBe "0"
    }
