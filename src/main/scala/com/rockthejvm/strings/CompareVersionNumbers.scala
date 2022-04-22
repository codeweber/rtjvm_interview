package com.rockthejvm.strings

object CompareVersionNumbers {
  

    def compareVersionNumbers(v1: String, v2: String): Int = 

        def getRevisions(s: String): List[Int] =
            s.split('.').toList.map(_.toInt)

        val revisions1 = getRevisions(v1)
        val revisions2 = getRevisions(v2)

        val revisionsCompare = revisions1.zipAll(revisions2, 0, 0).map(_.compare(_))
        val revisionsDifferent = revisionsCompare.dropWhile(_ == 0)

        if revisionsDifferent.isEmpty then
            0
        else 
            1 * revisionsDifferent.head.sign

}