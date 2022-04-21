package com.rockthejvm.strings

import org.scalatest.matchers.should.Matchers
import ReverseWords.*
import org.scalatest.flatspec.AnyFlatSpec


class ReverseWordsSpec extends AnyFlatSpec with Matchers:

    "reverseWords" should "reverse words in a string" in {
        reverseWords("Alice loves Scala") shouldBe "Scala loves Alice"
    }

    it should "treat ignore whitespacing in input" in {
        reverseWords("    hello     world    ") shouldBe "world hello"
    }
