package forcomp

import common._

object Anagrams {

  /** A word is simply a `String`. */
  type Word = String

  /** A sentence is a `List` of words. */
  type Sentence = List[Word]

  /** `Occurrences` is a `List` of pairs of characters and positive integers saying
   *  how often the character appears.
   *  This list is sorted alphabetically w.r.t. to the character in each pair.
   *  All characters in the occurrence list are lowercase.
   *  
   *  Any list of pairs of lowercase characters and their frequency which is not sorted
   *  is **not** an occurrence list.
   *  
   *  Note: If the frequency of some character is zero, then that character should not be
   *  in the list.
   */
  type Occurrences = List[(Char, Int)]

  /** The dictionary is simply a sequence of words.
   *  It is predefined and obtained as a sequence using the utility method `loadDictionary`.
   */
  val dictionary: List[Word] = loadDictionary

  /** Converts the word into its character occurence list.
   *  
   *  Note: the uppercase and lowercase version of the character are treated as the
   *  same character, and are represented as a lowercase character in the occurrence list.
   */
  def wordOccurrences(w: Word): Occurrences = {

//    val sortedChars = (for ( c <- w.toLowerCase ) yield  c ).toList.sorted
//    for (c <- sortedChars.distinct) yield ( c, sortedChars.count( x => x == c ) )

    w.toLowerCase.groupBy((c:Char) => c).map( p => (p._1, p._2.length)).toList.sorted
  }

  /** Converts a sentence into its character occurrence list. */
  def sentenceOccurrences(s: Sentence): Occurrences = {
    wordOccurrences(s.flatten.mkString)
//    if (s.isEmpty) List()
//    else {
//      val raw = (for (w <- s) yield wordOccurrences(w)).flatten
//      val sortedByChar = raw.sorted(Ordering.by[(Char, Int), Char](_._1))
//
//      sortedByChar.dropRight(1).foldRight(List(sortedByChar.last))((p: (Char, Int), l: Occurrences) => if (p._1 == l.head._1) (p._1, p._2 + l.head._2) :: l.tail else p :: l)
//    }
  }

  /** The `dictionaryByOccurrences` is a `Map` from different occurrences to a sequence of all
   *  the words that have that occurrence count.
   *  This map serves as an easy way to obtain all the anagrams of a word given its occurrence list.
   *  
   *  For example, the word "eat" has the following character occurrence list:
   *
   *     `List(('a', 1), ('e', 1), ('t', 1))`
   *
   *  Incidentally, so do the words "ate" and "tea".
   *
   *  This means that the `dictionaryByOccurrences` map will contain an entry:
   *
   *    List(('a', 1), ('e', 1), ('t', 1)) -> Seq("ate", "eat", "tea")
   *
   */
  lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] = {
    val dictionary: List[Word] = loadDictionary
    dictionary.groupBy((w:Anagrams.Word) => Anagrams.wordOccurrences(w))
  }

  /** Returns all the anagrams of a given word. */
  def wordAnagrams(word: Word): List[Word] = {
    val o = dictionaryByOccurrences.get(wordOccurrences(word))
    o match {
      case None => List()
      case Some(_) => o.get
    }
  }

  /** Returns the list of all subsets of the occurrence list.
   *  This includes the occurrence itself, i.e. `List(('k', 1), ('o', 1))`
   *  is a subset of `List(('k', 1), ('o', 1))`.
   *  It also include the empty subset `List()`.
   * 
   *  Example: the subsets of the occurrence list `List(('a', 2), ('b', 2))` are:
   *
   *    List(
   *      List(),
   *      List(('a', 1)),
   *      List(('a', 2)),
   *      List(('b', 1)),
   *      List(('a', 1), ('b', 1)),
   *      List(('a', 2), ('b', 1)),
   *      List(('b', 2)),
   *      List(('a', 1), ('b', 2)),
   *      List(('a', 2), ('b', 2))
   *    )
   *
   *  Note that the order of the occurrence list subsets does not matter -- the subsets
   *  in the example above could have been displayed in some other order.
   */
  def combinations(occurrences: Occurrences): List[Occurrences] = {

    occurrences match {
      case List() => List(List())
      case oh :: ot => {
        val y = combinations(ot)
        (for {
          c <- y
          i <- 1 to oh._2
        } yield (oh._1,i) :: c ) ++ y
      }
    }

    /* https://class.coursera.org/progfun-004/forum/thread?thread_id=1142#post-4665
      Let's say I know all the subsets (which includes the empty set) of some set X. Call this list of subsets Y.

      Now I want all the subsets of X + a, where a is not in X. Call this Y' ("Y prime").

      Y' can be formed by adding a to to each subset in the list Y, and concatenating Y to it.

      Concrete example:
      X = {1, 2}
      Y = {{1, 2},     {1},    {2},    {}}
      Add a to each subset in Y:
      Y' = {1, 2, a}, {1, a}, {2, a}, {a}} + Y
      Resulting in:
      {1, 2, a}, {1, a}, {2, a}, {a}, {1, 2},     {1},    {2},    {}}

      Or to put it another way, Y + (for each subset y  in Y, y+ a)
     */
  }

  /** Subtracts occurrence list `y` from occurrence list `x`.
   * 
   *  The precondition is that the occurrence list `y` is a subset of
   *  the occurrence list `x` -- any character appearing in `y` must
   *  appear in `x`, and its frequency in `y` must be smaller or equal
   *  than its frequency in `x`.
   *
   *  Note: the resulting value is an occurrence - meaning it is sorted
   *  and has no zero-entries.
   */
  def subtract(x: Occurrences, y: Occurrences): Occurrences = {
    val ym = y.toMap

    val diff = for (c<-x) yield (c._1, c._2 - ym.getOrElse(c._1, 0))
    diff.filter( p => p._2 > 0)

  }

  /** Returns a list of all anagram sentences of the given sentence.
   *  
   *  An anagram of a sentence is formed by taking the occurrences of all the characters of
   *  all the words in the sentence, and producing all possible combinations of words with those characters,
   *  such that the words have to be from the dictionary.
   *
   *  The number of words in the sentence and its anagrams does not have to correspond.
   *  For example, the sentence `List("I", "love", "you")` is an anagram of the sentence `List("You", "olive")`.
   *
   *  Also, two sentences with the same words but in a different order are considered two different anagrams.
   *  For example, sentences `List("You", "olive")` and `List("olive", "you")` are different anagrams of
   *  `List("I", "love", "you")`.
   *  
   *  Here is a full example of a sentence `List("Yes", "man")` and its anagrams for our dictionary:
   *
   *    List(
   *      List(en, as, my),
   *      List(en, my, as),
   *      List(man, yes),
   *      List(men, say),
   *      List(as, en, my),
   *      List(as, my, en),
   *      List(sane, my),
   *      List(Sean, my),
   *      List(my, en, as),
   *      List(my, as, en),
   *      List(my, sane),
   *      List(my, Sean),
   *      List(say, men),
   *      List(yes, man)
   *    )
   *
   *  The different sentences do not have to be output in the order shown above - any order is fine as long as
   *  all the anagrams are there. Every returned word has to exist in the dictionary.
   *  
   *  Note: in case that the words of the sentence are in the dictionary, then the sentence is the anagram of itself,
   *  so it has to be returned in this list.
   *
   *  Note: There is only one anagram of an empty sentence.
   */
  def sentenceAnagrams(sentence: Sentence): List[Sentence] = {
    def subSentenceAnagrams(occ: Occurrences): List[Sentence] = {
      if (occ.isEmpty) List(List())
      else
        for {
          validCombination <- combinations(occ) if dictionaryByOccurrences.contains(validCombination)
          word <- dictionaryByOccurrences(validCombination)
          rest <- subSentenceAnagrams(subtract(occ, validCombination))
        } yield word :: rest
    }
    subSentenceAnagrams(sentenceOccurrences(sentence))
  }


  // Original implementation.
  // I was trying to call sentenceAnagrams by regenerating a Sentence from subtract.
  // I have some failure with the degenerate case - I only get the first valid word?
  // I am getting too tired to debug further since I figured out the subSentenceAnagrams approach
  def sentenceAnagrams2(sentence: Sentence): List[Sentence] = {

    println(sentence)

    if (sentence.isEmpty) List(List())
    else {

      val sentenceCombinations = sentenceOccurrences(sentence)
      val allCombinations = combinations(sentenceCombinations)

      for {
        validCombination <- allCombinations if dictionaryByOccurrences.contains(validCombination)
        word <- dictionaryByOccurrences(validCombination)
        rest <- sentenceAnagrams2(List((for (c <- subtract(sentenceCombinations, validCombination)) yield 1 to c._2 foreach { _ => c._1}).mkString))
      } yield word :: rest

    }
  }

}
