package com.rockthejvm.strings

import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec
import CompareVersionNumbers.*

class CompareVersionNumbersSpec extends AnyFlatSpec with Matchers:

    "compareVersionNumbers" should "return 0 for two identical version numbers" in {
        val v = "1.0.0.1"
        compareVersionNumbers(v,v) shouldBe 0
    }


    it should "return 0 for two version numbers that are idential up to leading zeros" in {
        val v1 = "1.0.0.1"
        val v2 = "1.0.0.01"
        compareVersionNumbers(v1,v2) shouldBe 0
    }

    it should "return 0 for two version numbers that are idential up to trailing 0 revisions" in {
        val v1 = "1.0.0.1"
        val v2 = "1.0.0.1.0.0"
        compareVersionNumbers(v1,v2) shouldBe 0
    }

    it should "return 1 for two version numbers where first is larger" in {
        val v1 = "1.0.0.1"
        val v2 = "1.0.0.0"
        compareVersionNumbers(v1,v2) shouldBe 1
    }

    it should "return -1 for two version numbers where first is smaller" in {
        val v1 = "1.0.0.0"
        val v2 = "1.0.0.1"
        compareVersionNumbers(v1,v2) shouldBe -1
    }

    it should "return -1 for two version numbers where first is smaller and different length" in {
        val v1 = "1.0.0.1"
        val v2 = "2.1"
        compareVersionNumbers(v1,v2) shouldBe -1
    }

