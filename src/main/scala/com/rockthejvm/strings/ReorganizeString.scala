package com.rockthejvm.strings
import scala.collection.immutable.ListMap

object ReorganizeString:

    def reorganizeString(str: String): String = 

        def countCharsSorted(s: String): ListMap[Char, Int] =
            val charsCounted = s.foldLeft(Map.empty[Char,Int])( (m, c) => m + (c -> (m.getOrElse(c,0)+1) ))
            ListMap.from(charsCounted.toSeq.sortWith{(c1n1, c2n2) => 
                c1n1._2 < c2n2._2 || (c1n1._2 == c2n2._2 && c1n1._1 < c2n2._2)
            })
            
        def intersperseChar(char: Char, nOcc: Int, processed: String, remaining: String): String =
            if nOcc == 0 then
                processed + remaining 
            else 
                if remaining.isEmpty then 
                    if nOcc == 1 && (processed.isEmpty || processed.last != char) then
                        processed + char
                    else 
                        ""
                else 
                    val nextChar = remaining.head
                    val prevChar = processed.lastOption 
                    if (nextChar == char) || (prevChar == Some(char)) then 
                        intersperseChar(char, nOcc, processed + nextChar, remaining.tail)
                    else
                        intersperseChar(char, nOcc-1, processed + char + nextChar, remaining.tail)

        def buildString(cs: ListMap[Char, Int], agg: String): String = 

            if cs.isEmpty then
                agg
            else if cs.size == 1 then
                val (c, n) = cs.head 
                intersperseChar(c, n, "", agg)
            else
                val (_,n) = cs.head 
                val newString = cs.keys.mkString.repeat(n)
                val newCs = cs.map( x => (x._1, x._2 - n)).filter(_._2 > 0)
                buildString(newCs, agg + newString)



        if str.length <= 1 then
            str
        else 
            val charCountMap = countCharsSorted(str)
            if charCountMap.size == 1 then 
                ""
            else
                buildString(charCountMap, "")
