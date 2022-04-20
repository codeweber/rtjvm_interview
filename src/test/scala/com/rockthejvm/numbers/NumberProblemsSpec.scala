package com.rockthejvm.numbers

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import NumberProblems.*

class NumberProblemsSpec extends AnyFlatSpec with Matchers:

    "isPrime" should "be true for a set of prime numbers" in {
        
        val somePrimes = List(2,3,5,7,11,13,17,19,23)
        somePrimes.filter(isPrime) shouldBe somePrimes
    
    }

    it should "work for a set of negative primes" in {

        val someNegativePrimes = List(2,3,5,7,11,13,17,19,23).map(-_)
        someNegativePrimes.filter(isPrime) shouldBe someNegativePrimes

    }

    it should "work for a set of multiples of 2" in {

        val someEvenNumbers = (2 to 20).toList.map(2*_)
        someEvenNumbers.filter(isPrime) shouldBe Nil

    }

    it should "be false for zero" in {
        isPrime(0) shouldBe false
    }

    it should "be false for 1" in {
        isPrime(1) shouldBe false
    }

    "decompose" should "work for a small number" in {
        decompose(18) should contain theSameElementsAs List(2, 3, 3)
    }

    it should "work for another small number" in {
        decompose(26) should contain theSameElementsAs List(2, 13)
    }

    it should "work for a prime number" in {
        decompose(13) should contain theSameElementsAs List(13)
    }

    it should "work for a power of 2" in {
        decompose(16) should contain theSameElementsAs List(2,2,2,2)
    }

    it should "work for 1" in {
        decompose(1) shouldBe List()
    }

    it should "work for 0" in {
        decompose(0) shouldBe List()
    }