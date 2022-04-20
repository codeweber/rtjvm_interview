package com.rockthejvm.numbers

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import RecurringDecimals.*

class RecurringDecimalsSpec extends AnyFlatSpec with Matchers:

    "prettyPrintFraction" should "correctly print a rational number that is equal to an int" in {

        prettyPrintFraction(4, 2) shouldBe "2"

    }

    it should "correctly print 1/2" in {
        prettyPrintFraction(1,2) shouldBe "0.5"
    }

    it should "correctly print 1/3" in {
        prettyPrintFraction(1,3) shouldBe "0.(3)"
    }

    it should "correctly print 1/6" in {
        prettyPrintFraction(1,6) shouldBe "0.1(6)"
    }

    it should "correctly print 1/7" in {
        prettyPrintFraction(1,7) shouldBe "0.(142857)"
    }

    it should "correctly print 1/8" in {
        prettyPrintFraction(1,8) shouldBe "0.125"
    }

    it should "correctly print 1/2003" in {
        prettyPrintFraction(1,2003) should fullyMatch regex """0\.\([0-9]+\)"""
    }

    it should "correctly print -1/2" in {
        prettyPrintFraction(-1,2) shouldBe "-0.5"
    }
