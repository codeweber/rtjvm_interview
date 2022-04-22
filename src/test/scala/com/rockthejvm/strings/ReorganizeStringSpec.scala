package com.rockthejvm.strings

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import ReorganizeString.*

class ReorganizeStringSpec extends AnyFlatSpec with Matchers:

    "reorganizeString" should "return an empty string for aa" in {
        reorganizeString("aa") shouldBe ""
    }

    it should "return abab for aabb" in {
        reorganizeString("aabb") shouldBe "abab"
    }

    it should "return aba for aab" in {
        reorganizeString("aab") shouldBe "aba"
    }

    it should "return babab for aabbb" in {
        reorganizeString("aabbb") shouldBe "babab"
    }

    it should "return empty for aabbbb" in {
        reorganizeString("aabbbb") shouldBe ""
    }