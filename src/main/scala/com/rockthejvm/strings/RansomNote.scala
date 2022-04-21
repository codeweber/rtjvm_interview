package com.rockthejvm.strings

import StringProblems.countChars

object RansomNote:

    def ransomNote(note: String, magazine: String): Boolean = 

        val charCountsMagazine = countChars(magazine)
        val charCountsNote = countChars(note)

        charCountsNote.dropWhile((c, nOcc) => charCountsMagazine.getOrElse(c, 0) >= nOcc).isEmpty


    def ransomNoteAlt(note: String, magazine: String): Boolean = ???
    