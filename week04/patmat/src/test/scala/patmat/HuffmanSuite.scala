package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
  trait TestTrees {
    val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
    val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
    val longString = "This is a longer string which I hope does not take too much time to parse"
    /*
      val T = Leaf('T', 1)
      val h = Leaf('h', 5)
      val i = Leaf('i', 5)
      val s = Leaf('s', 5)
      val space = Leaf(' ', 15)
      val a = Leaf('a', 3)
      val l = Leaf('l', 1)
      val o = Leaf('o', 7)
      val n = Leaf('n', 3)
      val g = Leaf('g', 2)
      val e = Leaf('e', 6)
      val r = Leaf('r', 3)
      val t = Leaf('t', 6)
      val w = Leaf('w', 1)
      val c = Leaf('c', 2)
      val I = Leaf('I', 1)
      val p = Leaf('p', 2)
      val d = Leaf('d', 1)
      val k = Leaf('k', 1)
      val m = Leaf('m', 2)
      val u = Leaf('u', 1)
      */
  }

  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
      assert(weight(t2) === 9)
    }
  }

  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t1) === List('a', 'b'))
      assert(chars(t2) === List('a','b','d'))
    }
  }

  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("times of unique list") {
    val l1 = List('a', 'b', 'c')
    val times1 = times(l1)
    assert(times1.length === 3)
    assert(times1.contains(('a', 1)), "times1 does not contain ('a', 1)")
    assert(times1.contains(('b', 1)), "times1 does not contain ('b', 1)")
    assert(times1.contains(('c', 1)), "times1 does not contain ('c', 1)")
  }

  test("times of list with repeated elements") {
    val l2 = List('a', 'b', 'a')
    val times2 = times(l2)
    assert(times2.length === 2)
    assert(times2.contains(('a', 2)), "times2 does not contain ('a', 2)")
    assert(times2.contains(('b', 1)), "times2 does not contain ('b', 1)")
  }

  test("times of empty list") {
    val l3 = List()
    val times3 = times(l3)
    assert(times3.isEmpty, "times3 was not empty")
  }

  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }

  test("singleton of empty list") {
    assert(singleton(List()) === false, "an empty list is not a singleton")
  }

  test("singleton of a 1-element leaf list") {
    assert(singleton(List(Leaf('e',1))) === true, "singleton leaf list was not identified as such")
  }

  test("singleton of a 1-element fork list") {
    assert(singleton(List(Fork(Leaf('a', 1), Leaf('b', 1), List('a', 'b'), 2))) === true, "singleton fork list was not identified as such")
  }

  test("singleton of a multi-element list") {
    assert(singleton(List(Leaf('a', 1), Leaf('b', 1))) === false, "singleton of multi-element list should not be identified as a singleton")
  }

  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }

  test("combine of empty list") {
    val leaflist = List()
    assert(combine(leaflist).isEmpty, "combine of empty list was not empty")
  }

  test("combine of 1-element list") {
    val leaflist = List(Leaf('e', 1))
    assert(combine(leaflist) === List(Leaf('e',1)), "combine of 1-element list was not unchanged")
  }

  test("combine where new fork does not fall at the beginning") {
    val leaflist = List(Leaf('e', 1), Leaf('f', 1), Leaf('g', 1))
    assert(combine(leaflist) === List(Leaf('g',1), Fork(Leaf('e',1), Leaf('f',1), List('e','f'), 2)))
  }

  test("combine forks") {
    val elem1 = Fork(Leaf('a',1), Leaf('b',1), List('a', 'b'), 2)
    val elem2 = Leaf('c',3)

    val list = List(elem1, elem2)
    assert(combine(list) === List(Fork(elem1, elem2, List('a', 'b', 'c'), 5)))
  }

  test("until of empty") {
    intercept[IllegalArgumentException] {
      until(singleton, combine)(List())
      fail("until(singleton, combine)(List()) did not throw an IllegalArgumentException")
    }
  }

  test("until of singleton") {
    val single = Leaf('c', 2)
    assert(until(singleton, combine)(List(single)) === single, "until of single did not yield single")
  }

  test("until of double") {
    val double = List(Leaf('a', 1), Leaf('b', 2))
    assert(until(singleton, combine)(double) === Fork(Leaf('a',1), Leaf('b', 2), List('a', 'b'), 3))
  }

  test("until of triple") {
    val a = Leaf('a', 1)
    val b = Leaf('b', 2)
    val c = Leaf('c', 3)

    val triple = List(a, b, c)

    val expected = makeCodeTree(makeCodeTree(a, b), c)

    assert(until(singleton, combine)(triple) === expected)
  }

  test("createCodeTree aba") {
    val orig = List('a', 'b', 'a')

    val actualCodeTree = createCodeTree(orig)
    val expectedCodeTree = Fork(Leaf('b', 1), Leaf('a', 2), List('b', 'a'), 3)

    assert(actualCodeTree === expectedCodeTree, "createCodeTree did not produce the expected code tree")
  }

  test("createCodeTree empty") {
    val orig = List()
    intercept[IllegalArgumentException] {
      createCodeTree(orig)
      fail("createCodeTree of empty list did not throw an IllegalArgumentException")
    }
  }

  test("createCodeTree long* string") {
    new TestTrees {
      val orig = longString.toList

      val codeTree = createCodeTree(orig)

      assert(weight(codeTree) === 73, "weight did not match")
    }
  }

  test("decode aba") {
    val orig = List('a', 'b', 'a')
    val codeTree = createCodeTree(orig)
    val bits = List(1,0,1)
    val decoded = decode(codeTree, bits)
    assert(decoded === orig)
  }

  test("decode frenchCode") {
    println(decodedSecret.mkString(""))
  }

  test("encode aba") {
    val orig = List('a', 'b', 'a')
    val codeTree = createCodeTree(orig)
    val bits = List(1,0,1)
    val encoded = encode(codeTree)(orig)
    assert(encoded === bits)
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

  test("decode and encode a long* string should be identity") {
    new TestTrees {
      val codeTree = createCodeTree(longString.toList)
      val tic = System.currentTimeMillis()
      assert(decode(codeTree, encode(codeTree)(longString.toList)) === longString.toList)
      val toc = System.currentTimeMillis()
      println("elapsed time for inefficient encode: " + (toc-tic) + " ms")
    }
  }

  test("decode and quick encode a long* string should be identity") {
    new TestTrees{
      val codeTree = createCodeTree(longString.toList)
      val tic = System.currentTimeMillis()
      assert(decode(codeTree, quickEncode(codeTree)(longString.toList)) === longString.toList)
      val toc = System.currentTimeMillis()
      println("elapsed time for efficient encode: " + (toc-tic) + " ms")
    }
  }
}
