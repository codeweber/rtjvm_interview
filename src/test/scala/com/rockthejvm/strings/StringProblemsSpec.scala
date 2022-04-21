package com.rockthejvm.strings

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import StringProblems.* 

class StringProblemsSpec extends AnyFlatSpec with Matchers:

    "countChars" should "correctly count characters in hello" in {   
        countChars("hello") shouldBe Map('h' -> 1, 'e' -> 1, 'l' -> 2, 'o' -> 1)
    }

    it should "return an empty map when given an empty string" in {
        countChars("") shouldBe Map()
    }

    "checkAnagram" should "return true for a pair of anagrams" in {
        checkAnagrams("hello", "lloeh") shouldBe true
    }

    it should "return false for a pair of unrelated strings" in {
        checkAnagrams("foo", "bar") shouldBe false
    }
