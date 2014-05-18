package objsets

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TweetSetSuite extends FunSuite {
  trait TestSets {
    
    val a = new Tweet("a", "a body", 20) 
    val b = new Tweet("b", "b body", 20)
    
    val set1 = new Empty
    val set2 = set1.incl(a)
    val set3 = set2.incl(b)
    val c = new Tweet("c", "c body", 7)
    val d = new Tweet("d", "d body", 9)
    val set4c = set3.incl(c)
    val set4d = set3.incl(d)
    val set5 = set4c.incl(d)

    val e = new Tweet("e", "e body", 0)
    val f = new Tweet("f", "f body", 1)
    val g = new Tweet("g", "g body", 2)
    val h = new Tweet("h", "h body", 3)
    val i = new Tweet("i", "i body", 4)
    val j = new Tweet("j", "j body", 5)
    val k = new Tweet("k", "k body", 6)
    val l = new Tweet("l", "l body", 8)
    val m = new Tweet("m", "m body", 10)
    val n = new Tweet("n", "n body", 11)
    val o = new Tweet("o", "o body", 12)
    val p = new Tweet("p", "p body", 13)
    val q = new Tweet("q", "q body", 14)
    val r = new Tweet("r", "r body", 15)
    val s = new Tweet("s", "s body", 16)
    val t = new Tweet("t", "t body", 17)
    val u = new Tweet("u", "u body", 18)
    val v = new Tweet("v", "v body", 19)
    val w = new Tweet("w", "w body", 21)
    val x = new Tweet("x", "x body", 22)
    val y = new Tweet("y", "y body", 23)
    val z = new Tweet("z", "z body", 24)

    val big = set5 incl e incl f incl g incl h incl i incl j incl k incl l incl m incl n incl o incl p incl q incl r incl s incl t incl u incl v incl w incl x incl y incl z
  }

  def asSet(tweets: TweetSet): Set[Tweet] = {
    var res = Set[Tweet]()
    tweets.foreach(res += _)
    res
  }

  def size(set: TweetSet): Int = asSet(set).size

  test("operations on large sets") {
    new TestSets {
      assert(size(big.filter(tw => tw.retweets > 20)) === 4)
      assert(size(big.union(set5)) === size(big))
    }
  }

  test("filter: on empty set") {
    new TestSets {
      assert(size(set1.filter(tw => tw.user == "a")) === 0)
    }
  }

  test("filter: a on set5") {
    new TestSets {
      assert(size(set5.filter(tw => tw.user == "a")) === 1)
    }
  }

  test("filter: 20 on set5") {
    new TestSets {
      assert(size(set5.filter(tw => tw.retweets == 20)) === 2)
    }
  }

  test("filter: body contains on set4c") {
    new TestSets {
      assert(size(set4c.filter(tw => tw.text.contains("bodies"))) === 0)
    }
  }

  test("union: set4c and set4d") {
    new TestSets {
      val u2 = set4c.union(set4d)
      assert(size(u2) === 4)
      assert(u2.contains(a))
      assert(u2.contains(b))
      assert(u2.contains(c))
      assert(u2.contains(d))
    }
  }

  test("union: with empty set (1)") {
    new TestSets {
      assert(size(set5.union(set1)) === 4)
    }
  }

  test("union: with empty set (2)") {
    new TestSets {
      assert(size(set1.union(set5)) === 4)
    }
  }

  test("mostRetweeted: set 3") {
    new TestSets {
      assert(set3.mostRetweeted.retweets === 20)
    }
  }

  test("mostRetweeted: set 5") {
    new TestSets {
      assert(set5.mostRetweeted.retweets === 20)
    }
  }

  test("mostRetweeted: empty set") {
    new TestSets {
      intercept[NoSuchElementException] {
        assert(set1.mostRetweeted.retweets === -1, "mostRetweeted did not throw an exception for empty sets")
      }
    }
  }

  test("descending: set5") {
    new TestSets {
      val trends = set5.descendingByRetweet
      assert(!trends.isEmpty)
      assert(trends.head.user == "a" || trends.head.user == "b")
      assert(trends.tail.tail.head.user === "d")
      assert(trends.tail.tail.tail.head.user === "c")
      assert(trends.tail.tail.tail.tail.isEmpty)
    }
  }

  test("descending: set1") {
    new TestSets {
      val trends = set1.descendingByRetweet
      assert(trends.isEmpty)
    }
  }

  test("googleVsApple") {
    /*
    https://class.coursera.org/progfun-004/forum/thread?thread_id=646
    38 tweets in GoogleVsApple.googleTweets
    150 tweets in GoogleVsApple.appleTweets
    Top Tweet has 321 retweets in GoogleVsApple.trending
    total number of retweets in union of google/apple is 8118
     */

    assert(size(GoogleVsApple.googleTweets) === 38)
    assert(size(GoogleVsApple.appleTweets) === 150)
    assert(GoogleVsApple.trending.head.retweets === 321)

    var count = 0
    GoogleVsApple.trending.foreach((f:Tweet) => count += f.retweets)
    assert(count === 8118)

  }
}
