package forcomp

//10 / 10

import scala.io.{Codec, Source}

object Anagrams extends AnagramsInterface {

  /** A word is simply a `String`. */
  type Word = String

  /** A sentence is a `List` of words. */
  type Sentence = List[Word]

  /** `Occurrences` is a `List` of pairs of characters and positive integers saying
   * how often the character appears.
   * This list is sorted alphabetically w.r.t. to the character in each pair.
   * All characters in the occurrence list are lowercase.
   *
   * Any list of pairs of lowercase characters and their frequency which is not sorted
   * is **not** an occurrence list.
   *
   * Note: If the frequency of some character is zero, then that character should not be
   * in the list.
   */
  type Occurrences = List[(Char, Int)]

  /** The dictionary is simply a sequence of words.
   * It is predefined and obtained as a sequence using the utility method `loadDictionary`.
   */
  val dictionary: List[Word] = Dictionary.loadDictionary

  /** Converts the word into its character occurrence list.
   *
   * Note: the uppercase and lowercase version of the character are treated as the
   * same character, and are represented as a lowercase character in the occurrence list.
   *
   * Note: you must use `groupBy` to implement this method!
   */
  def wordOccurrences(w: Word): Occurrences = {
    val charGroups = w.toCharArray
      .map(c => c.toLower)
      .groupBy(c => c)
    // 'x' -> Array('x', 'x', 'x')

    charGroups.toList
      .map(pair => (pair._1, pair._2.length))
      .sortWith((pair1, pair2) => pair1._1 < pair2._1) //sort by letter
  }

  /** Converts a sentence into its character occurrence list. */
  def sentenceOccurrences(s: Sentence): Occurrences = {
    if (s == Nil)
      Nil
    else
      wordOccurrences(s.reduce((word1, word2) => word1 + word2))
  }

  /** The `dictionaryByOccurrences` is a `Map` from different occurrences to a sequence of all
   * the words that have that occurrence count.
   * This map serves as an easy way to obtain all the anagrams of a word given its occurrence list.
   *
   * For example, the word "eat" has the following character occurrence list:
   *
   * `List(('a', 1), ('e', 1), ('t', 1))`
   *
   * Incidentally, so do the words "ate" and "tea".
   *
   * This means that the `dictionaryByOccurrences` map will contain an entry:
   *
   * List(('a', 1), ('e', 1), ('t', 1)) -> Seq("ate", "eat", "tea")
   *
   */
  lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] = {
    dictionary.groupBy(word => wordOccurrences(word))
  }

  /** Returns all the anagrams of a given word. */
  def wordAnagrams(word: Word): List[Word] = {
    val occurences = wordOccurrences(word)

    val anagrams = dictionaryByOccurrences.get(occurences)

    anagrams.get
  }

  /** Returns the list of all subsets of the occurrence list.
   * This includes the occurrence itself, i.e. `List(('k', 1), ('o', 1))`
   * is a subset of `List(('k', 1), ('o', 1))`.
   * It also include the empty subset `List()`.
   *
   * Example: the subsets of the occurrence list `List(('a', 2), ('b', 2))` are:
   *
   * List(
   * List(),
   * List(('a', 1)),
   * List(('a', 2)),
   * List(('b', 1)),
   * List(('a', 1), ('b', 1)),
   * List(('a', 2), ('b', 1)),
   * List(('b', 2)),
   * List(('a', 1), ('b', 2)),
   * List(('a', 2), ('b', 2))
   * )
   *
   * Note that the order of the occurrence list subsets does not matter -- the subsets
   * in the example above could have been displayed in some other order.
   */

  def combinations(occurrences: Occurrences): List[Occurrences] = occurrences match {
    case Nil => List(Nil)
    case _ => List() :: combinationsHelper(occurrences)
  }

  private def combinationsHelper(occurrences: Occurrences): List[Occurrences] = occurrences match {
    case Nil => List(Nil)
    case head :: Nil =>
      val results = for (count <- 1 to head._2) yield List((head._1, count))
      results.toList
    case _ =>
      //pick this answer's current position
      //List(('a', 2), ('b', 2), ('c', 3)
      val results = for {
        idx <- occurrences.indices
        remainderIdx <- idx + 1 to occurrences.length
        remainderCombo <- combinationsHelper(occurrences.drop(remainderIdx))
        count <- 1 to occurrences(idx)._2
      }
      yield {
        val head = (occurrences(idx)._1, count)

        //println(head :: remainderCombo)
        //head ('a', 1) ; remainder ('b', 2))
        //head ('a', 2) ; remainder ('b', 2))
        head :: remainderCombo
      }

      results.toList
  }

  /** Subtracts occurrence list `y` from occurrence list `x`.
   *
   * The precondition is that the occurrence list `y` is a subset of
   * the occurrence list `x` -- any character appearing in `y` must
   * appear in `x`, and its frequency in `y` must be smaller or equal
   * than its frequency in `x`.
   *
   * Note: the resulting value is an occurrence - meaning it is sorted
   * and has no zero-entries.
   */
  def subtract(x: Occurrences, y: Occurrences): Occurrences = {
    if (x == Nil)
      Nil
    else if (y == Nil)
      x
    else {
      //      val jimmy = List(('i', 1), ('j', 1), ('m', 2), ('y', 1))
      //      val my = List(('m', 1), ('y', 1))

      x match {
        case head :: tail if head._1 == y.head._1 =>
          val newCount = head._2 - y.head._2
          if (newCount <= 0)
            subtract(tail, y.tail)
          else
            (head._1, newCount) :: subtract(tail, y.tail)
        case head :: tail if head._1 < y.head._1 => head :: subtract(tail, y)
        case head :: _ if head._1 > y.head._1 => subtract(x, y.tail)
        case _ => throw new IllegalArgumentException("reee")
      }


    }

  }

  /** Returns a list of all anagram sentences of the given sentence.
   *
   * An anagram of a sentence is formed by taking the occurrences of all the characters of
   * all the words in the sentence, and producing all possible combinations of words with those characters,
   * such that the words have to be from the dictionary.
   *
   * The number of words in the sentence and its anagrams does not have to correspond.
   * For example, the sentence `List("I", "love", "you")` is an anagram of the sentence `List("You", "olive")`.
   *
   * Also, two sentences with the same words but in a different order are considered two different anagrams.
   * For example, sentences `List("You", "olive")` and `List("olive", "you")` are different anagrams of
   * `List("I", "love", "you")`.
   *
   * Here is a full example of a sentence `List("Yes", "man")` and its anagrams for our dictionary:
   *
   * List(
   * List(en, as, my),
   * List(en, my, as),
   * List(man, yes),
   * List(men, say),
   * List(as, en, my),
   * List(as, my, en),
   * List(sane, my),
   * List(Sean, my),
   * List(my, en, as),
   * List(my, as, en),
   * List(my, sane),
   * List(my, Sean),
   * List(say, men),
   * List(yes, man)
   * )
   *
   * The different sentences do not have to be output in the order shown above - any order is fine as long as
   * all the anagrams are there. Every returned word has to exist in the dictionary.
   *
   * Note: in case that the words of the sentence are in the dictionary, then the sentence is the anagram of itself,
   * so it has to be returned in this list.
   *
   * Note: There is only one anagram of an empty sentence.
   */
  def sentenceAnagrams(sentence: Sentence): List[Sentence] = {
    if (sentence == Nil)
      List(Nil)
    else {
      sentenceAnagramsHelper(sentence)
    }

  }

  //process one valid word at the current place at a time.
  def sentenceAnagramsHelper(sentence: Sentence): List[Sentence] = {
    if(sentence == Nil)
      List(Nil)
    else {
      val occurences = sentenceOccurrences(sentence)

      for {
        combo <- combinations(occurences)
        if dictionaryByOccurrences.contains(combo)
        anagramWord <- dictionaryByOccurrences(combo) //this current spot's word
        futureSentence <- sentenceAnagramsHelper(occurrencesToNonsenseWords(subtract(occurences, combo)))
      }
      yield {
        anagramWord :: futureSentence
      }
    }
  }

  /**
   * converts occurences to a blob of words
   */
  private def occurrencesToNonsenseWords(occurences: Occurrences): Sentence = occurences match {
    case Nil => Nil
    case head :: Nil => List(multiplyString(head._1, head._2))
    case head :: tail => multiplyString(head._1, head._2) :: occurrencesToNonsenseWords(tail)
  }

  //note: probably something out there already zzzz
  private def multiplyString(c: Char, n: Int): String = {
    if (n == 0)
      ""
    else
      c + multiplyString(c, n - 1)

  }

}

object Dictionary {
  def loadDictionary: List[String] = {
    val wordstream = Option {
      getClass.getResourceAsStream(List("forcomp", "linuxwords.txt").mkString("/", "/", ""))
    } getOrElse {
      sys.error("Could not load word list, dictionary file not found")
    }
    try {
      val s = Source.fromInputStream(wordstream)(Codec.UTF8)
      s.getLines().toList
    } catch {
      case e: Exception =>
        println("Could not load word list: " + e)
        throw e
    } finally {
      wordstream.close()
    }
  }
}
