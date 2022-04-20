package com.rockthejvm.numbers

import scala.annotation.tailrec

object RecurringDecimals:

    def prettyPrintFraction(numerator: Int, denominator: Int): String = 

        @tailrec
        def loop(num: Long, den: Long, decimalPoint: Int, decimalValues: Vector[Long], numerators: Map[Long, Int]): (Vector[Long], Option[Int]) = 
            val quotient = num / den 
            val remainder = num % den

            if remainder == 0 then 
                (decimalValues :+ quotient, None)
            else
                val nextNum = 10 * remainder
                numerators.get(nextNum) match 
                    case None => loop(nextNum, den, decimalPoint + 1, decimalValues :+ quotient, numerators + (num -> decimalPoint)) 
                    case Some(idx) => 
                        if nextNum / den == quotient then 
                            (decimalValues, Some(idx))
                        else 
                            (decimalValues :+ quotient, Some(idx))
 
        val (decimals, repeatPoint) = loop(numerator.toLong.abs, denominator.toLong.abs, 0, Vector.empty[Long], Map.empty[Long,Int])

        val numDecimals = decimals.length 
        val sign = if numerator.sign == denominator.sign then "" else "-"
        val preDecimalPoint = s"${sign}${decimals(0)}"

        if numDecimals == 1 then
            preDecimalPoint
        else
            val idxRepeatSequence = repeatPoint.getOrElse(numDecimals)
            val nonRepeatingDecimals = (1 until idxRepeatSequence).map(n => decimals(n)).mkString
            val repeatingDecimals = 
                if idxRepeatSequence < numDecimals then 
                    "(" + (idxRepeatSequence until numDecimals).map(n => decimals(n)).mkString + ")"
                else ""
            preDecimalPoint + "." + nonRepeatingDecimals + repeatingDecimals