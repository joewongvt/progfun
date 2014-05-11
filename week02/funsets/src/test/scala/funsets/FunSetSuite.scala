package funsets

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * This class is a test suite for the methods in object FunSets. To run
 * the test suite, you can either:
 *  - run the "test" command in the SBT console
 *  - right-click the file in eclipse and chose "Run As" - "JUnit Test"
 */
@RunWith(classOf[JUnitRunner])
class FunSetSuite extends FunSuite {


  /**
   * Link to the scaladoc - very clear and detailed tutorial of FunSuite
   *
   * http://doc.scalatest.org/1.9.1/index.html#org.scalatest.FunSuite
   *
   * Operators
   *  - test
   *  - ignore
   *  - pending
   */

  /**
   * Tests are written using the "test" operator and the "assert" method.
   */
  test("string take") {
    val message = "hello, world"
    assert(message.take(5) == "hello")
  }

  /**
   * For ScalaTest tests, there exists a special equality operator "===" that
   * can be used inside "assert". If the assertion fails, the two values will
   * be printed in the error message. Otherwise, when using "==", the test
   * error message will only say "assertion failed", without showing the values.
   *
   * Try it out! Change the values so that the assertion fails, and look at the
   * error message.
   */
  test("adding ints") {
    assert(1 + 2 === 3)
  }

  
  import FunSets._

  test("contains is implemented") {
    assert(contains(x => true, 100))
  }
  
  /**
   * When writing tests, one would often like to re-use certain values for multiple
   * tests. For instance, we would like to create an Int-set and have multiple test
   * about it.
   * 
   * Instead of copy-pasting the code for creating the set into every test, we can
   * store it in the test class using a val:
   * 
   *   val s1 = singletonSet(1)
   * 
   * However, what happens if the method "singletonSet" has a bug and crashes? Then
   * the test methods are not even executed, because creating an instance of the
   * test class fails!
   * 
   * Therefore, we put the shared values into a separate trait (traits are like
   * abstract classes), and create an instance inside each test method.
   * 
   */

  trait TestSets {
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)
    val isPositive = (x:Int) => x >0
    val negativeNumber = (x:Int) => x<0
    val evenNumber = (x:Int) => x % 2 == 0
    val anyNumber = (x:Int) => true
    val noNumber = (x:Int) => false
    val emptySet = (s:Set) => !exists(s,anyNumber)
  }

  /**
   * This test is currently disabled (by using "ignore") because the method
   * "singletonSet" is not yet implemented and the test would fail.
   * 
   * Once you finish your implementation of "singletonSet", exchange the
   * function "ignore" by "test".
   */
  test("singletonSet(1) contains 1") {
    
    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3". 
     */
    new TestSets {
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      assert(contains(s1, 1), "s1 does not contain 1")
    }
  }

  test("singletonSet(1) does not contain 2"){
    new TestSets{
      assert(!contains(s1, 2), "s1 contains 2")
    }
  }

  test("singletonSet(1) does not contain -1") {
    new TestSets{
      assert(!contains(s1, -1), "s1 contains -1")
    }
  }

  test("union contains all elements") {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }

  test("intersection only contains common elements") {
    new TestSets {
      val s = intersect(s1, s2)
      assert(!contains(s, 1), "Intersection(s1,s2) contains 1")
      assert(!contains(s, 2), "Intersection(s1,s2) contains 2")

      val t = intersect(s1, s1)
      assert(contains(t, 1), "Intersection(s1,s1) does not contain 1")
      assert(!contains(t, 2), "Intersection(s1,s1) contains 2")

      val u = intersect(union(s1, s2), s1)
      assert(contains(u, 1), "Intersection(u(s1,s2), s1) does not contain 1")
      assert(!contains(u, 2), "Intersection(u(s1,s2), s1) contains 2")
      assert(!contains(u, 3), "Intersection(u(s1,s2), s1) contains 3")
    }
  }

  test("diff only contains unique elements ") {
    new TestSets{
      val s = diff(s1,s2)
      assert(contains(s, 1), "Diff(s1,s2) does not contain 1")
      assert(!contains(s,2), "Diff(s1,s2) contains 2")

      val t = diff(s1, s1)
      assert(!contains(t,1), "Diff(s1,s1) contains 1")
      assert(!contains(t,2), "Diff(S1,s1) contains 2")

      val u = diff(union(s1,s2), s1)
      assert(!contains(u,1), "Diff(u(s1,s2), s1) contains 1")
      assert(contains(u,2), "Diff(u(s1,s2), s1) does not contain 2")
    }
  }

  test("filter only contains filtered elements") {
    new TestSets {
      val s = filter(s1, isPositive)
      assert(contains(s,1), "filter s1 by positive values does not contain 1")

      val t = filter(union(singletonSet(-1),s1), isPositive)
      assert(contains(t, 1), "filter u(-1,1) by positive values does not contain 1")
      assert(!contains(t,-1), "filter u(-1,1) by positive values contains -1")
    }
  }

  test("basic forall test") {
    new TestSets {
      val s = union(union(s1,s2),s3)
      assert(forall(s,isPositive), "all elements of u(s1,s2,s3) are not positive")

      val t = union(singletonSet(-1001), s1)
      assert(forall(t, isPositive), "all bounded elements of u(-1001, s1) are not positive")

      val u = union(singletonSet(-2), s2)
      assert(!forall(u, isPositive), "all bounded elements of u(-2,2) are positive")

      val w = intersect(s1,s2)
      // https://class.coursera.org/progfun-004/forum/thread?thread_id=203
      // The proposition "for all items belonging to a set X, proposition P holds true" is in itself true for any choice of P if X is an empty set.
      assert(forall(w, noNumber), "forall(X,P) where X is empty should return true regardless of P")
    }
  }

  test("exists tests") {
    new TestSets {
      val s = union(union(s1,s2), s3)
      assert(exists(s, evenNumber), "u(1,2,3) does not contain a positive number")
      assert(!exists(s, negativeNumber), "u(1,2,3) contains a negative number")
      assert(!exists(s, x => x>5), "u(1,2,3) contains a number > 5")
      assert(emptySet(intersect(s1,s2)), "intersect(1,2) is not empty")
      //Is the the only way to define an empty set !exists(emptySet, anyNumber), or can I do something like this?
      //assert(exists(intersect(s1,s2), noNumber), "interesect(1,2) does not contain noNumber")
    }
  }

  test("map tests") {
    new TestSets {
      val s = union(s1,s2)

      val d = diff(map(s, x => x*2), union(singletonSet(2), singletonSet(4)))
      assert(emptySet(d), "doubling of u(1,2) contains numbers not 2 or 4")

      val e = map(intersect(s1,s2), x => -x)
      assert(emptySet(e), "empty set should map to empty set")
    }
  }
}
