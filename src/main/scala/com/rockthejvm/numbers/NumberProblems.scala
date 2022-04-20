package com.rockthejvm.numbers

import scala.math.sqrt
import scala.annotation.tailrec

object NumberProblems:

    def isPrimeSimple(n: Int): Boolean = 
    
        val m = sqrt(n).toInt
        val oddFactorCandidates = 3 to m by 2
        (n == 2) || ((n%2 != 0) && oddFactorCandidates.filter(n % _ == 0).isEmpty )


    def isPrime(n: Int): Boolean = 

        val maxDivisor = sqrt(n.abs)

        @tailrec
        def isPrimeTailRec(currentDivisor: Int): Boolean = 
            (currentDivisor > maxDivisor) || ((n % currentDivisor != 0) && isPrimeTailRec(currentDivisor + 1))

        (maxDivisor > 1) && isPrimeTailRec(2)


    def decomposeInitial(n: Int): List[Int] =
        /* Decompose an input integer into primes. Based on the fundamental theorem of arithmetic, this 
            decomposition is unique, so halt at the first set of prime factors obtained */

        @tailrec
        def decomposeTailRec(currentNumber: Int, currentDivisor: Int, primes: List[Int]): List[Int] = 

            if isPrime(currentNumber) then
                currentNumber :: primes 
            else
                if isPrime(currentDivisor) && (currentNumber % currentDivisor == 0) then 
                        decomposeTailRec(currentNumber/currentDivisor, currentDivisor, currentDivisor :: primes)
                else 
                    val nextDivisor = currentDivisor + (if currentDivisor % 2 == 0 then 1 else 2) 
                    decomposeTailRec(currentNumber, nextDivisor, primes)

        if n == 0 then 
            Nil 
        else if n.abs == 1 then 
            List(n)
        else if isPrime(n) then
            List(n.abs, 1)
        else
            decomposeTailRec(n.abs, 2, Nil)
    
    def decompose(n: Int): List[Int] =
        /* Decompose an input integer into primes. Based on the fundamental theorem of arithmetic, this 
            decomposition is unique, so halt at the first set of prime factors obtained */

        @tailrec
        def decomposeTailRec(remainder: Int, currentDivisor: Int, primes: List[Int]): List[Int] = 

            if currentDivisor > sqrt(remainder) then
                remainder :: primes 
            else if remainder % currentDivisor == 0 then 
                decomposeTailRec(remainder / currentDivisor, currentDivisor, currentDivisor :: primes)    
            else 
                val nextDivisor = currentDivisor + (if currentDivisor % 2 == 0 then 1 else 2) 
                decomposeTailRec(remainder, nextDivisor, primes)

        val nAbs = n.abs
        if nAbs == 0 || nAbs == 1 then 
            Nil
        else
            decomposeTailRec(nAbs, 2, Nil)

    @main def TryNumberProb =

        (1 to 20).map(n => s"$n is${if !isPrime(n) then " not" else ""} prime").foreach(println)
        

end NumberProblems