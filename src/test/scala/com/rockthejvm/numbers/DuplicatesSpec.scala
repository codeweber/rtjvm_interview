package com.rockthejvm.numbers

import org.scalatest.matchers.should.Matchers
import Duplicates.*
import org.scalatest.flatspec.AnyFlatSpec

class DuplicatesSpec extends AnyFlatSpec with Matchers:

    "duplicates" should "return the one number that is not duplicated" in {
        val maxVal = 10000
        val l = (1 to maxVal).toList 
        duplicates(l ++ l ++ List(maxVal+1)) shouldBe (maxVal+1)
    }
