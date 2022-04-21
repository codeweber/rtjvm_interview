package com.rockthejvm.numbers

object Duplicates:

    def duplicates(l: List[Int]): Int =
        l.reduce(_ ^ _)
