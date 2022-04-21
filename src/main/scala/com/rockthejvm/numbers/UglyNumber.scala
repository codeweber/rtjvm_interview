package com.rockthejvm.numbers

import scala.annotation.tailrec

object UglyNumber:

    val uglyFactors = List(2,3,5)

    def uglyNumber(number: Int): Boolean = 

        /* 
        Assume positive inputs
        Check if number only has factors 2,3,5
         */

        @tailrec
        def loop(numToFactorise: Int, factors: List[Int]): Boolean = 

            if numToFactorise == 1 then 
                true
            else
                factors match
                    case Nil => false 
                    case f :: fs if numToFactorise % f == 0 => loop(numToFactorise / f, factors)
                    case f :: fs => loop(numToFactorise, fs)

        (number != 0) && loop(number, uglyFactors)


    def nthUgly(index: Int): Int =

        @tailrec
        def loop(n: Int, i: Int): Int = 
            if i >= index then 
                n 
            else 
                val newNumber = n + 1
                val j = 
                    if uglyNumber(newNumber) then 
                        i + 1 
                    else 
                        i 
                    
                loop(newNumber, j)

        loop(1, 1)

    

    @main def TryUgly =
        println((1 to 100).map(nthUgly))