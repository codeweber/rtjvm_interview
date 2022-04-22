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

    "findPath" should "return a path between A and E" in {
        findPath(socialNetwork, "A", "E") shouldBe List("A", "D", "E")
    }

    "existsLoop" should "return true for the social network" in {
        existsLoop(socialNetwork) shouldBe true
    }

    it should "return false for a graph without loops" in {

        val acyclicGraph = Map(
            "A" -> Set("B", "C"),
            "B" -> Set("D", "C"),
            "C" -> Set("D", "E"),
            "D" -> Set(),
            "E" -> Set("F")
        )

        existsLoop(acyclicGraph) shouldBe false

    }

    "makeUndirected" should "given a graph with 2 nodes and 1 edge, return the graph with 2 edges" in {
        val simpleDirectedGraph = Map('A' -> Set('B'), 'B' -> Set())
        makeUndirected(simpleDirectedGraph) shouldBe Map('A' -> Set('B'), 'B' -> Set('A'))
    }

    it should "correctly handle the social network graph" in {

        val socialNetworkUndirected: Graph[String] = Map(
            "A" -> Set("B", "C", "D"),
            "B" -> Set("A", "D", "E"),
            "C" -> Set("D", "A", "E"),
            "D" -> Set("B", "E", "A", "C"),
            "E" -> Set("B", "C", "D")
        )

        makeUndirected(socialNetwork) shouldBe socialNetworkUndirected
    }
