package recfun

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class CountChangeSuite extends FunSuite {
  import Main.countChange
  test("countChange: example given in instructions") {
    assert(countChange(4,List(1,2)) === 3)
  }

  test("countChange: sorted CHF") {
    assert(countChange(300,List(5,10,20,50,100,200,500)) === 1022)
  }

  test("countChange: no pennies") {
    assert(countChange(301,List(5,10,20,50,100,200,500)) === 0)
  }

  test("countChange: unsorted CHF") {
    assert(countChange(300,List(500,5,50,100,20,200,10)) === 1022)
  }

  test("countChange: repeated denominations") {
    assert(countChange(300, List(500,5,50,100,20,200,10, 200)) === 1022)
  }

  test("countChange: zero amount") {
    assert(countChange(0, List(500, 5, 50, 20)) === 1)
  }

  test("countChange: no coins") {
    assert(countChange(10, List()) === 0)
  }

  test("countChange: CHF < all denominations") {
    assert(countChange(1, List(5, 10, 20)) === 0)
  }

  test("countChange: CHF contains a 0 coin") {
    assert(countChange(100, List(25, 50, 0)) === 3)
  }

  test("countChange: CHF contains a negative coin") {
    assert(countChange(100, List(25, 50, 0, -10)) === 3)
  }
}
