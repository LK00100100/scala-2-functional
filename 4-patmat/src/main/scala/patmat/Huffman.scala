package patmat

// 9.43 / 10.00
// 'createCodeTree(someText)' does not give an optimal encoding. reeee

/**
 * A huffman code is represented by a binary tree.
 *
 * Every `Leaf` node of the tree represents one character of the alphabet that the tree can encode.
 * The weight of a `Leaf` is the frequency of appearance of the character.
 *
 * The branches of the huffman tree, the `Fork` nodes, represent a set containing all the characters
 * present in the leaves below it. The weight of a `Fork` node is the sum of the weights of these
 * leaves.
 */
abstract class CodeTree

case class Fork(left: CodeTree, right: CodeTree, chars: List[Char], weight: Int) extends CodeTree

case class Leaf(char: Char, weight: Int) extends CodeTree

/**
 * Assignment 4: Huffman coding
 *
 */
trait Huffman extends HuffmanInterface {

  // tree match
  // note : just get the weight of the top?
  //given a tree, return total weight of leaves
  def weight(tree: CodeTree): Int = tree match {
    case Fork(left, right, _, _) => this.weight(left) + this.weight(right)
    case Leaf(_, weight) => weight
  }

  // Given a tree, return all the chars (in leaves)
  def chars(tree: CodeTree): List[Char] = tree match {
    case Fork(left, right, _, _) => this.chars(left) ++ this.chars(right)
    case Leaf(char, _) => List(char)
  }

  def makeCodeTree(left: CodeTree, right: CodeTree) =
    Fork(left, right, chars(left) ::: chars(right), weight(left) + weight(right))

  // Part 2: Generating Huffman trees

  /**
   * In this assignment, we are working with lists of characters. This function allows
   * you to easily create a character list from a given string.
   */
  def string2Chars(str: String): List[Char] = str.toList

  /**
   * This function computes for each unique character in the list `chars` the number of
   * times it occurs. For example, the invocation
   *
   * times(List('a', 'b', 'a'))
   *
   * should return the following (the order of the resulting list is not important):
   *
   * List(('a', 2), ('b', 1))
   *
   * The type `List[(Char, Int)]` denotes a list of pairs, where each pair consists of a
   * character and an integer. Pairs can be constructed easily using parentheses:
   *
   * val pair: (Char, Int) = ('c', 1)
   *
   * In order to access the two elements of a pair, you can use the accessors `_1` and `_2`:
   *
   * val theChar = pair._1
   * val theInt  = pair._2
   *
   * Another way to deconstruct a pair is using pattern matching:
   *
   * pair match {
   * case (theChar, theInt) =>
   * println("character is: "+ theChar)
   * println("integer is  : "+ theInt)
   * }
   */
  def times(chars: List[Char]): List[(Char, Int)] = {
    if (chars.isEmpty)
      Nil
    else {
      val smallestChar = getSmallestChar(chars)
      times(remove(chars, smallestChar), List((smallestChar, 1)))
    }
  }

  //accumulator is never empty
  //accumulator head is the current thing we're building
  def times(chars: List[Char], acc: List[(Char, Int)]): List[(Char, Int)] = {
    require(acc.nonEmpty)
    if (chars.isEmpty)
      acc
    else {
      val smallestChar = getSmallestChar(chars)

      val headAcc: Char = acc.head._1

      smallestChar match {
        case `headAcc` => times(remove(chars, smallestChar), (smallestChar, acc.head._2 + 1) :: acc.tail)
        case _ => times(remove(chars, smallestChar), (smallestChar, 1) :: acc)
      }
    }
  }

  //remove target from chars
  def remove(chars: List[Char], target: Char): List[Char] = chars match {
    case Nil => throw new IllegalArgumentException("needs something")
    case List(head) if head == target => List()
    case List(head) if head != target => List(head)
    case head :: tail if head == target => tail
    case head :: tail if head != target => head :: remove(tail, target)
  }

  //a is the smallest. z is the largest
  def getSmallestChar(chars: List[Char]): Char = chars match {
    case Nil => throw new IllegalArgumentException("needs argument")
    case List(head) => head
    case head :: tail => {
      val smallestTailChar = this.getSmallestChar(tail)
      if (smallestTailChar < head) smallestTailChar else head
    }
  }

  def makeCodeTree(chars: List[Char]): CodeTree = chars match {
    case Nil => throw new IllegalArgumentException("chars are empty")
    case List(head) => Leaf(head, 1)
    case head :: tail => makeCodeTree(Leaf(head, 1), makeCodeTree(tail))
  }

  /**
   * Returns a list of `Leaf` nodes for a given frequency table `freqs`.
   *
   * The returned list should be ordered by ascending weights (i.e. the
   * head of the list should have the smallest weight), where the weight
   * of a leaf is the frequency of the character.
   */
  def makeOrderedLeafList(freqs: List[(Char, Int)]): List[Leaf] = {
    freqs match {
      case Nil => Nil
      case List(head) => List(Leaf(head._1, head._2))
      case _ => {
        val smallestWeight = getSmallestWeight(freqs)
        val target = (smallestWeight.char, smallestWeight.weight)
        smallestWeight ::
          makeOrderedLeafList(removeSmallestWeight(freqs, target))
      }
    }

  }

  def getSmallestWeight(freqs: List[(Char, Int)]): Leaf = freqs match {
    case Nil => throw new IllegalArgumentException("needs argument")
    case List(head) => Leaf(head._1, head._2)
    case head :: tail => {
      val smallestTailWeight = this.getSmallestWeight(tail)
      if (smallestTailWeight.weight < head._2) smallestTailWeight else Leaf(head._1, head._2)
    }
  }

  //remove target from weights
  //todo: could make this generic with remove()
  def removeSmallestWeight(weights: List[(Char, Int)], target: (Char, Int)): List[(Char, Int)] =
    weights match {
      case Nil => throw new IllegalArgumentException("needs something")
      case List(head) if head == target => List()
      case List(head) if head != target => List(head)
      case head :: tail if head == target => tail
      case head :: tail if head != target => head :: removeSmallestWeight(tail, target)
    }

  /**
   * Checks whether the list `trees` contains only one single code tree.
   */
  def singleton(trees: List[CodeTree]): Boolean = trees match {
    case _ :: tail if tail == Nil => true
    case _ => false
  }

  /**
   * The parameter `trees` of this function is a list of code trees ordered
   * by ascending weights.
   *
   * This function takes the first two elements of the list `trees` and combines
   * them into a single `Fork` node. This node is then added back into the
   * remaining elements of `trees` at a position such that the ordering by weights
   * is preserved.
   *
   * If `trees` is a list of less than two elements, that list should be returned
   * unchanged.
   */
  def combine(trees: List[CodeTree]): List[CodeTree] = {

    trees match {
      case Nil => List()
      case List(_) => trees
      case head :: tail  => //combine two
        val totalChars = chars(head) ++ chars(tail.head)
        val totalWeight = weight(head) + weight(tail.head)
        val newFork = Fork(head, tail.head, totalChars, totalWeight)

        val remainderCombined = combine(trees.tail.tail) //skip two

        insert(remainderCombined, newFork)
    }
  }

  //note: recalc'ing target weight is bad
  //trees is ordered
  def insert(trees: List[CodeTree], target: CodeTree): List[CodeTree] =
    trees match {
      case Nil => List(target)
      case head :: _ if weight(target) <= weight(head) => target :: trees
      case _ => trees.head :: insert(trees.tail, target)
    }

  /**
   * This function will be called in the following way:
   *
   * until(singleton, combine)(trees)
   *
   * where `trees` is of type `List[CodeTree]`, `singleton` and `combine` refer to
   * the two functions defined above.
   *
   * In such an invocation, `until` should call the two functions until the list of
   * code trees contains only one single tree, and then return that singleton list.
   */
  def until(done: List[CodeTree] => Boolean, merge: List[CodeTree] => List[CodeTree])(trees: List[CodeTree]): List[CodeTree] = {
    if (done(trees))
      trees
    else {
      this.until(done, merge)(merge(trees))
    }
  }

  //note: can make more optimal
  /**
   * This function creates a code tree which is optimal to encode the text `chars`.
   *
   * The parameter `chars` is an arbitrary text. This function extracts the character
   * frequencies from that text and creates a code tree based on them.
   */
  def createCodeTree(chars: List[Char]): CodeTree = {
    val freqChars = times(chars)
    val orderedLeaves = makeOrderedLeafList(freqChars)

    until(singleton, combine)(orderedLeaves).head
  }

  // Part 3: Decoding

  type Bit = Int

  /**
   * This function decodes the bit sequence `bits` using the code tree `tree` and returns
   * the resulting list of characters.
   */
  def decode(tree: CodeTree, bits: List[Bit]): List[Char] = bits match {
    case Nil => Nil
    case _ =>
      val (remainingBits, outputChar) = decodeHelper(tree, bits)

      outputChar :: decode(tree, remainingBits)
  }

  //given a tree and some bits of 0,1s and it goes left/right and finds the leaf-char
  //assumed char is found
  //assumed tree has enough nodes
  //returns remaining list of bits and foundChar
  private def decodeHelper(tree: CodeTree, bits: List[Bit]): (List[Bit], Char) =
    tree match {
      case Fork(left, _, _, _) if bits.head == 0 => decodeHelper(left, bits.tail)
      case Fork(_, right, _, _) if bits.head == 1 => decodeHelper(right, bits.tail)
      case Leaf(char, _) => (bits, char)
    }

  /**
   * A Huffman coding tree for the French language.
   * Generated from the data given at
   * http://fr.wikipedia.org/wiki/Fr%C3%A9quence_d%27apparition_des_lettres_en_fran%C3%A7ais
   */
  val frenchCode: CodeTree = Fork(Fork(Fork(Leaf('s', 121895), Fork(Leaf('d', 56269), Fork(Fork(Fork(Leaf('x', 5928), Leaf('j', 8351), List('x', 'j'), 14279), Leaf('f', 16351), List('x', 'j', 'f'), 30630), Fork(Fork(Fork(Fork(Leaf('z', 2093), Fork(Leaf('k', 745), Leaf('w', 1747), List('k', 'w'), 2492), List('z', 'k', 'w'), 4585), Leaf('y', 4725), List('z', 'k', 'w', 'y'), 9310), Leaf('h', 11298), List('z', 'k', 'w', 'y', 'h'), 20608), Leaf('q', 20889), List('z', 'k', 'w', 'y', 'h', 'q'), 41497), List('x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q'), 72127), List('d', 'x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q'), 128396), List('s', 'd', 'x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q'), 250291), Fork(Fork(Leaf('o', 82762), Leaf('l', 83668), List('o', 'l'), 166430), Fork(Fork(Leaf('m', 45521), Leaf('p', 46335), List('m', 'p'), 91856), Leaf('u', 96785), List('m', 'p', 'u'), 188641), List('o', 'l', 'm', 'p', 'u'), 355071), List('s', 'd', 'x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q', 'o', 'l', 'm', 'p', 'u'), 605362), Fork(Fork(Fork(Leaf('r', 100500), Fork(Leaf('c', 50003), Fork(Leaf('v', 24975), Fork(Leaf('g', 13288), Leaf('b', 13822), List('g', 'b'), 27110), List('v', 'g', 'b'), 52085), List('c', 'v', 'g', 'b'), 102088), List('r', 'c', 'v', 'g', 'b'), 202588), Fork(Leaf('n', 108812), Leaf('t', 111103), List('n', 't'), 219915), List('r', 'c', 'v', 'g', 'b', 'n', 't'), 422503), Fork(Leaf('e', 225947), Fork(Leaf('i', 115465), Leaf('a', 117110), List('i', 'a'), 232575), List('e', 'i', 'a'), 458522), List('r', 'c', 'v', 'g', 'b', 'n', 't', 'e', 'i', 'a'), 881025), List('s', 'd', 'x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q', 'o', 'l', 'm', 'p', 'u', 'r', 'c', 'v', 'g', 'b', 'n', 't', 'e', 'i', 'a'), 1486387)

  /**
   * What does the secret message say? Can you decode it?
   * For the decoding use the `frenchCode' Huffman tree defined above.
   */
  val secret: List[Bit] = List(0, 0, 1, 1, 1, 0, 1, 0, 1, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 1, 0, 1, 1, 0, 0, 1, 1, 1, 1, 1, 0, 1, 0, 1, 1, 0, 0, 0, 0, 1, 0, 1, 1, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 1)

  /**
   * Write a function that returns the decoded secret
   */
  def decodedSecret: List[Char] = List('h', 'u', 'f', 'f', 'm', 'a', 'n', 'e', 's', 't', 'c', 'o', 'o', 'l')

  // Part 4a: Encoding using Huffman tree

  /**
   * This function encodes `text` using the code tree `tree`
   * into a sequence of bits.
   */
  //todo: assumed target chars exists?
  def encode(tree: CodeTree)(text: List[Char]): List[Bit] =
    text match {
      case Nil => Nil
      case head :: tail =>

        //note: i can probably do away with this reverse somehow
        val encoded = getEncodePath(tree, Nil, head).reverse

        encoded ++ encode(tree)(tail)
    }

  //figure out the path from root to leaf with char. 0 is left, 1 is right
  //assumed char exists.
  //acc - accumulator
  //returns answer backwards
  def getEncodePath(tree: CodeTree, acc: List[Bit], target: Char): List[Bit] =
    tree match {
      case Leaf(char, _) if target != char => Nil
      case Leaf(char, _) if target == char => acc
      case Fork(left, right, _, _) =>
        val leftPath = getEncodePath(left, 0 :: acc, target)
        if (leftPath != Nil)
          leftPath
        else {
          val rightPath = getEncodePath(right, 1 :: acc, target)
          if (rightPath != Nil)
            rightPath
          else
            Nil
        }
    }

  // Part 4b: Encoding using code table

  type CodeTable = List[(Char, List[Bit])]

  /**
   * This function returns the bit sequence that represents the character `char` in
   * the code table `table`.
   */
  def codeBits(table: CodeTable)(char: Char): List[Bit] = {
    //note: can use map
    for {
      tuple <- table
      if char == tuple._1
      bits <- tuple._2
    } yield {
      bits
    }
  }

  /**
   * Given a code tree, create a code table which contains, for every character in the
   * code tree, the sequence of bits representing that character.
   *
   * Hint: think of a recursive solution: every sub-tree of the code tree `tree` is itself
   * a valid code tree that can be represented as a code table. Using the code tables of the
   * sub-trees, think of how to build the code table for the entire tree.
   */
  def convert(tree: CodeTree): CodeTable = {
    getCodeTable(tree)
  }

  /**
   * This function takes two code tables and merges them into one. Depending on how you
   * use it in the `convert` method above, this merge method might also do some transformations
   * on the two parameter code tables.
   */
  def mergeCodeTables(a: CodeTable, b: CodeTable): CodeTable = ???

  /**
   * This function encodes `text` according to the code tree `tree`.
   *
   * To speed up the encoding process, it first converts the code tree to a code table
   * and then uses it to perform the actual encoding.
   */
  def quickEncode(tree: CodeTree)(text: List[Char]): List[Bit] = {
    val codeTable = getCodeTable(tree)

    for {
      c <- text
      bits <- codeBits(codeTable)(c)
    }
    yield {
      bits
    }
  }

  //creates a code table.
  //acc is the bit. left = 0, right = 1..
  //when the method ultimately completes, left, right, right => 100
  private def getCodeTable(tree: CodeTree, acc: List[Bit] = Nil): CodeTable = tree match {
    case Fork(left, right, _, _) => getCodeTable(left, 0 :: acc) ++ getCodeTable(right, 1 :: acc)
    case Leaf(char, _) => List((char, acc.reverse))
  }

}

object Huffman extends Huffman
