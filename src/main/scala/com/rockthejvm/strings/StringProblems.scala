package com.rockthejvm.strings

import scala.annotation.tailrec

object StringProblems {
  
    def countChars(s: String): Map[Char, Int] = 
        
        @tailrec
        def loop(remainingString: String, charOccurence: Map[Char, Int]): Map[Char, Int] = 
            if remainingString.isEmpty then 
                charOccurence
            else 
                val nextChar = remainingString.head 
                charOccurence.get(nextChar) match 
                    case Some(nOcc) => loop(remainingString.tail, charOccurence + (nextChar -> (nOcc + 1)))
                    case None => loop(remainingString.tail, charOccurence + (nextChar -> 1))

        loop(s, Map.empty[Char, Int])


    def checkAnagrams(s1: String, s2: String): Boolean = 
        (s1.length == s2.length) && (s1.sorted == s2.sorted)
}
