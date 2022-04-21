package com.rockthejvm.strings

object ReverseWords:

    def reverseWords(str: String): String =
        str.split(" ").filter(!_.isEmpty).reverse.mkString(" ")

