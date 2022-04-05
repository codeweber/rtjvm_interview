package com.rockthejvm.graphs

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class GraphProblemsSpec extends AnyFlatSpec with Matchers:

    import GraphProblems.*

    "isPath" should "identify a direct path" in {

        isPath(socialNetwork, "A", "B") shouldBe true

    }

    it should "identify an indirect path" in {

        isPath(socialNetwork, "A", "E") shouldBe true
    }

    it should "return false when no path exists" in {

        isPath(socialNetwork, "B", "C") shouldBe false

    }
