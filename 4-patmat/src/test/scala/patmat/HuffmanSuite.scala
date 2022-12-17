package patmat

class HuffmanSuite extends munit.FunSuite {

  import Huffman._

  trait TestTrees {
    val t1 = Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5)
    val t2 = Fork(Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5), Leaf('d', 4), List('a', 'b', 'd'), 9)
  }


  test("weight of a larger tree (10pts)") {
    new TestTrees {
      assertEquals(weight(t1), 5)
    }
  }


  test("chars of a larger tree (10pts)") {
    new TestTrees {
      assertEquals(chars(t2), List('a', 'b', 'd'))
    }
  }

  test("string2chars hello world") {
    assertEquals(string2Chars("hello, world"), List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("makeCodeTree with CodeTrees should work") {
    val t1 = Leaf('a', 2)
    val t2 = Leaf('b', 2)

    val result = makeCodeTree(t1, t2)

    print(result)
  }

  test("times should work") {
    val chars = List('a', 'b', 'a', 'c')

    val result = times(chars)

    assertEquals(result.head._1, 'c')
    assertEquals(result.head._2, 1)
    assertEquals(result(1)._1, 'b')
    assertEquals(result(1)._2, 1)
    assertEquals(result(2)._1, 'a')
    assertEquals(result(2)._2, 2)

    print(result)
  }


  test("createCodeTree should work") {
    val chars = List('a', 'b', 'a', 'b', 'a', 'c', 'd', 'd', 'e', 'f', 'g')

    val result = createCodeTree(chars)

    print(result)
  }

  test("make ordered leaf list for some frequency table (15pts)") {
    assertEquals(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))), List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 3)))
  }


  test("combine of some leaf list (15pts)") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assertEquals(combine(leaflist), List(Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3), Leaf('x', 4)))
  }

  test("until should work") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))

    val result = until(singleton, combine)(leaflist)

    print(result)
  }

  test("encode should work for leaves") {
    new TestTrees {
      val result: List[Bit] = encode(t1)(List('b', 'a'))

      print(result)
      assertEquals(result(0), 1)
      assertEquals(result(1), 0)
    }
  }

  test("quick encode should work for leaves") {
    new TestTrees {
      val result: List[Bit] = quickEncode(t1)(List('b', 'a'))

      print(result)
      assertEquals(result(0), 1)
      assertEquals(result(1), 0)
    }
  }

  test("encode should work for tree") {
    new TestTrees {
      val result: List[Bit] = encode(t2)(List('b', 'd', 'a'))

      print(result)
      assertEquals(result(0), 0)
      assertEquals(result(1), 1)
      assertEquals(result(2), 1)
      assertEquals(result(3), 0)
      assertEquals(result(4), 0)
    }
  }

  test("decode and encode a very short text should be identity (10pts)") {
    new TestTrees {
      assertEquals(decode(t1, encode(t1)("ab".toList)), "ab".toList)
    }
  }

  test("decode and quickEncode a very short text should be identity (10pts)") {
    new TestTrees {
      assertEquals(decode(t1, quickEncode(t1)("ab".toList)), "ab".toList)
    }
  }

  test("decode french code") {
    print(decode(frenchCode, secret))
  }

  import scala.concurrent.duration._

  override val munitTimeout = 10.seconds
}
