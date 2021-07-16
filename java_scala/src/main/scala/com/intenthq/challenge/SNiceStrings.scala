package com.intenthq.challenge

object SNiceStrings {

// From http://adventofcode.com/day/5
//  --- Day 5: Doesn't He Have Intern-Elves For This? ---
//
//  Santa needs help figuring out which strings in his text file are naughty or nice.
//
//    A nice string is one with all of the following properties:
//
//    It contains at least three vowels (aeiou only), like aei, xazegov, or aeiouaeiouaeiou.
//  It contains at least one letter that appears twice in a row, like xx, abcdde (dd), or aabbccdd (aa, bb, cc, or dd).
//    It does not contain the strings ab, cd, pq, or xy, even if they are part of one of the other requirements.
//    For example:
//
//    ugknbfddgicrmopn is nice because it has at least three vowels (u...i...o...), a double letter (...dd...), and none of the disallowed substrings.
//  aaa is nice because it has at least three vowels and a double letter, even though the letters used by different rules overlap.
//    jchzalrnumimnmhp is naughty because it has no double letter.
//    haegwjzuvuyypxyu is naughty because it contains the string xy.
//    dvszwmarrgswjxmb is naughty because it contains only one vowel.
//    How many strings are nice?

  def nice(xs: List[String]): Int = {
    //Regex checks if String contains at least 3 vowels. Regex doesn't take into account UPPER case chars
    val vowelsRegex: String = "^([a-z]*[aeiou][a-z]*){3}"
    val invalidSubstrings: List[String] = List("ab", "cd", "pq", "xy")

    xs.map {
      str =>
        val atLeastThreeVowels = str.matches(vowelsRegex)

        //Check if two characters are the same and count it
        val adjacentChars = str.sliding(2).count(letter => letter(0) == letter(1))
        val containsAdjacentChars = if (adjacentChars > 0) true else false

        //Go through each invalid string and check if str contains it then check if new list contains true
        val containsInvalidStrings = invalidSubstrings.map(invalid => str.contains(invalid)).contains(true)

        //Check Stirng if there are at least three vowels, there are adj chars and no invalid substrings
        if (atLeastThreeVowels && containsAdjacentChars && !containsInvalidStrings) {
          true
        } else {
          false
        }
    }.count(_ == true)
  }
}
