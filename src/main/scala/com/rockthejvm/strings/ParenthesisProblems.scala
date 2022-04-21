package com.rockthejvm.strings

import scala.annotation.tailrec

object ParenthesisProblems {

    def hasValidParentheses(str: String): Boolean = 

        @tailrec
        def trackOpenParentheses(substr: String, numOpenParentheses: Int): Boolean = 
            if substr.isEmpty then
                numOpenParentheses == 0
            else
                substr.head match
                    case '(' => trackOpenParentheses(substr.tail, numOpenParentheses + 1)
                    case ')' if numOpenParentheses == 0 => false 
                    case ')' => trackOpenParentheses(substr.tail, numOpenParentheses - 1)
                    case _ => trackOpenParentheses(substr.tail, numOpenParentheses)

        trackOpenParentheses(str, 0)


    def generateAllValidParentheses(n: Int): List[String] = 

        @tailrec
        def loop(incompleteStrings: List[(Int, Int, String)], validStrings: List[String]): List[String] = 
            incompleteStrings match 
                case Nil => validStrings
                case (numLeft, numOpen, str) :: icss => 
                    if numLeft == n then
                        val closingBrackets = (1 to numOpen).map(_ => ")").mkString 
                        loop(icss, (str + closingBrackets) :: validStrings)
                    else if numOpen == 0 then 
                        loop((numLeft+1, numOpen+1, str + "(") :: icss, validStrings)
                    else
                        loop((numLeft+1, numOpen+1, str + "(") :: (numLeft, numOpen-1, str + ")") :: icss, validStrings)
                    
        loop(List((0,0,"")), List.empty[String])
}
