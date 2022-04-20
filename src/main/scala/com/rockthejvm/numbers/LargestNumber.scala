package com.rockthejvm.numbers

object LargestNumber:

    def largestNumber(numbers: List[Int]): String =

        def isBigger(x: String, y: String): Boolean =
            x + y > y + x

        val largest = numbers.map(n => s"${n}").sortWith(isBigger).mkString("")

        if largest.isEmpty || largest.head == '0' then 
            "0"
        else
            largest
    