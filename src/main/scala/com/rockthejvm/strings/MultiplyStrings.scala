package com.rockthejvm.strings

import scala.annotation.tailrec

object MultiplyStrings {
  
    private[strings] def convertToDigitsList(s: String): List[Int] =
        s.toList.map(_ - '0').reverse

    private[strings] def convertToString(dl: List[Int]): String = 
        val dlOut = dl.reverse.dropWhile(_ == 0)

        if dlOut.isEmpty then
            "0"
        else
            dlOut.mkString

    @tailrec
    private[strings] def normaliseDigitList(dl: List[Int], carryover: Int, agg: List[Int]): List[Int] = 
        if dl.isEmpty && carryover == 0 then
            agg.reverse 
        else
            val (newDl, total) =
                dl match
                    case Nil => (Nil, carryover)
                    case d :: ds => (ds, d + carryover)

            val newDigit = total % 10
            val newCarryover = total / 10
            normaliseDigitList(newDl, newCarryover, newDigit :: agg)


    private[strings] def sumDigitLists(dl1: List[Int], dl2: List[Int]): List[Int] = 
        normaliseDigitList(dl1.zipAll(dl2, 0, 0).map(_ + _), 0, Nil)

    private[strings] def multiplyDigitLists(dl1: List[Int], dl2: List[Int]): List[Int] = 

        def multiplyDigitListByFactor(mantissa: Int, exponent: Int, dl: List[Int]): List[Int] = 
            List.fill(exponent)(0) ++ normaliseDigitList(dl.map(_*mantissa), 0, Nil)

        def loop(remaining: List[Int], currentExponent: Int, other: List[Int], agg: List[Int]): List[Int] = 

            remaining match
                case Nil => agg 
                case d :: ds =>
                    val newComponent = 
                        if d > 0 then
                            multiplyDigitListByFactor(d, currentExponent, other)
                        else Nil
                    val newAgg = sumDigitLists(newComponent, agg)
                    loop(ds, currentExponent+1, other, newAgg)    

        if dl1.length < dl2.length then
            loop(dl1, 0, dl2, Nil)
        else 
            loop(dl2, 0, dl1, Nil)

    def multiplyStrings(a: String, b: String): String = 

        val digitsA = convertToDigitsList(a)
        val digitsB = convertToDigitsList(b)

        convertToString(multiplyDigitLists(digitsA, digitsB))

}
