package com.rockthejvm.graphs

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import TownJudge.*

class TownJudgeSpec extends AnyFlatSpec with Matchers:

    "findJudge" should "return Some(1)" in {
        findJudge(2, Set((2, 1))) shouldBe Some(1)
    }

    it should "return None" in {
        findJudge(2, Set((1,2), (2,1))) shouldBe None
    }

