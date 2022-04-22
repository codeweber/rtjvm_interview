package com.rockthejvm.graphs

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import UniCourses.*

class UniCoursesSpec extends AnyFlatSpec with Matchers:

    "findOrder" should "return a correctly ordered list" in {

        val prereqs = List(
            (0,1), (0,2), (1,2), (1,3), (2,3), (2,4), (4,5)
        )

        findOrder(6, prereqs) shouldBe List(3, 5, 4, 2, 1, 0)

    }



