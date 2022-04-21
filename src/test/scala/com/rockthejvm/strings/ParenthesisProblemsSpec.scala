package com.rockthejvm.strings

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import ParenthesisProblems.*

class ParenthesisProblemsSpec extends AnyFlatSpec with Matchers:

    "hasValidParentheses" should "return true for ()" in {
        hasValidParentheses("()") shouldBe true
    }

    it should "return false for )(" in {
        hasValidParentheses(")(") shouldBe false
    }

    it should "return true for ((()))()()" in {
        hasValidParentheses("((()))()()") shouldBe true
    }

    it should "return false for ((())))()" in {
        hasValidParentheses("((())))()") shouldBe false
    }

    it should "return false for (" in {
        hasValidParentheses("(") shouldBe false
    }

    it should "return false for )()" in {
        hasValidParentheses(")()") shouldBe false
    }

    it should "return true for ''" in {
        hasValidParentheses("") shouldBe true
    }

    "generateAllValidParethenses" should "for n=1 give [()]" in {
        generateAllValidParentheses(1) shouldBe List("()")
    }
        
    "generateAllValidParethenses" should "for n=2 give [()(), (())]" in {
        generateAllValidParentheses(2) should contain theSameElementsAs List("()()", "(())")
    }

    "generateAllValidParethenses" should "for n=3 give [()()(), ((())), (())(), ()(()), (()())]" in {
        generateAllValidParentheses(3) should contain theSameElementsAs List("()()()", "((()))", "(())()", "()(())", "(()())")
    }