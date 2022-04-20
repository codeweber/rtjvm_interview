package com.rockthejvm.numbers

import scala.util.Try
import scala.util.Success
import scala.util.Failure

object ReverseInteger:

    def reverseInteger(number: Int): Int =

        val sign = if number >= 0 then 1 else -1

        val numberStringReverse = number.toLong.abs.toString.reverse 

        val reversedInt = Try {
            if numberStringReverse.charAt(0) == '0' then
                throw new RuntimeException("first numeral is zero")
            else
                numberStringReverse.toInt
        }

        reversedInt match
            case Success(v) => sign * v 
            case Failure(_) => 0


    @main def TryReverseInteger =

        println(reverseInteger(109))
        println(reverseInteger(Int.MaxValue))
        println(reverseInteger(100))

