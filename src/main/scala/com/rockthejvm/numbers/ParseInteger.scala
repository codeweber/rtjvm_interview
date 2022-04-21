package com.rockthejvm.numbers

import scala.util.{Try, Success, Failure}

object ParseInteger:

    def parseInteger(string: String): Int =

        import scala.util.matching.Regex

        val numberPattern = "([+-]?)([0-9]+)".r

        numberPattern.findFirstMatchIn(string) match
            case None => 0
            case Some(m) =>
                val signInt = if m.group(1) == "-" then -1 else 1
                val tryNumber = Try {
                    m.group(2).toInt
                }
                tryNumber match 
                    case Success(n) => signInt * n 
                    case Failure(_) => 
                        if signInt > 0 then 
                            Int.MaxValue
                        else 
                            Int.MinValue

        
    @main def TryParseInt =

        println(parseInteger("   +123 "))
        println(parseInteger("   -123 "))
        println(parseInteger("   123 "))
        println(parseInteger("   123 456"))
        println(parseInteger("aaaa   123 456"))
        println(parseInteger("aaaa   123456789101112"))
        println(parseInteger("aaaa   -123456789101112"))
        println(parseInteger("aaaa   "))
        

