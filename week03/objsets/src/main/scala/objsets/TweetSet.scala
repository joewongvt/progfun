package objsets

import common._
import TweetReader._

/**
 * A class to represent tweets.
 */
class Tweet(val user: String, val text: String, val retweets: Int) {
  override def toString: String =
    "User: " + user + "," +
    "Text: " + text + " [" + retweets + "]"
}

/**
 * This represents a set of objects of type `Tweet` in the form of a binary search
 * tree. Every branch in the tree has two children (two `TweetSet`s). There is an
 * invariant which always holds: for every branch `b`, all elements in the left
 * subtree are smaller than the tweet at `b`. The eleemnts in the right subtree are
 * larger.
 *
 * Note that the above structure requires us to be able to compare two tweets (we
 * need to be able to say which of two tweets is larger, or if they are equal). In
 * this implementation, the equality / order of tweets is based on the tweet's text
 * (see `def incl`). Hence, a `TweetSet` could not contain two tweets with the same
 * text from different users.
 *
 *
 * The advantage of representing sets as binary search trees is that the elements
 * of the set can be found quickly. If you want to learn more you can take a look
 * at the Wikipedia page [1], but this is not necessary in order to solve this
 * assignment.
 *
 * [1] http://en.wikipedia.org/wiki/Binary_search_tree
 */
abstract class TweetSet {

  /**
   * This method takes a predicate and returns a subset of all the elements
   * in the original set for which the predicate is true.
   *
   * Question: Can we implment this method here, or should it remain abstract
   * and be implemented in the subclasses?
   */
  def filter(p: Tweet => Boolean): TweetSet = {
    filterAcc(p, new Empty)
  }

  /**
   * This is a helper method for `filter` that propagetes the accumulated tweets.
   */
  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet

  /**
   * Returns a new `TweetSet` that is the union of `TweetSet`s `this` and `that`.
   *
   * Question: Should we implment this method here, or should it remain abstract
   * and be implemented in the subclasses?
   */
  def union(that: TweetSet): TweetSet

  /**
   * Helper method to avoid control flow via try/catch
   */
  def moreRetweetsThan(t: Tweet): Boolean

  /**
   * Returns the tweet from this set which has the greatest retweet count.
   *
   * Calling `mostRetweeted` on an empty set should throw an exception of
   * type `java.util.NoSuchElementException`.
   *
   * Question: Should we implment this method here, or should it remain abstract
   * and be implemented in the subclasses?
   */
  def mostRetweeted: Tweet

  /**
   * Returns a list containing all tweets of this set, sorted by retweet count
   * in descending order. In other words, the head of the resulting list should
   * have the highest retweet count.
   *
   * Hint: the method `remove` on TweetSet will be very useful.
   * Question: Should we implment this method here, or should it remain abstract
   * and be implemented in the subclasses?
   *
   * This method reflects a common pattern when transforming data structures. While traversing one data structure (in this case, a TweetSet),
   * we’re building a second data structure (here, an instance of class TweetList). The idea is to start with the empty list Nil (containing
   * no tweets), and to find the tweet with the most retweets in the input TweetSet. This tweet is removed from the TweetSet (that is, we obtain
   * a new TweetSet that has all the tweets of the original set except for the tweet that was “removed”; this immutable set operation, remove,
   * is already implemented for you), and added to the result list by creating a new Cons. After that, the process repeats itself, but now
   * we are searching through a TweetSet with one less tweet.
   */
  def descendingByRetweet: TweetList
  /* This was my first attempt, much slower
  def descendingByRetweet: TweetList = {
    def leastRetweeted(ts: TweetSet, min: Tweet): Tweet = {
      try {
        val max = ts.mostRetweeted
        leastRetweeted(ts.remove(max), max)
      } catch {
        case nse : NoSuchElementException => min
      }
    }
    def recDescendingByRetweet(ts: TweetSet, tl: TweetList): TweetList = {
      try {
        val min = leastRetweeted(ts, ts.mostRetweeted)
        recDescendingByRetweet(ts.remove(min), new Cons(min, tl))
      } catch {
        case nse : NoSuchElementException => tl
      }
    }
    recDescendingByRetweet(this, Nil)
  }
  */


  /**
   * The following methods are already implemented
   */

  /**
   * Returns a new `TweetSet` which contains all elements of this set, and the
   * the new element `tweet` in case it does not already exist in this set.
   *
   * If `this.contains(tweet)`, the current set is returned.
   */
  def incl(tweet: Tweet): TweetSet

  /**
   * Returns a new `TweetSet` which excludes `tweet`.
   */
  def remove(tweet: Tweet): TweetSet

  /**
   * Tests if `tweet` exists in this `TweetSet`.
   */
  def contains(tweet: Tweet): Boolean

  /**
   * This method takes a function and applies it to every element in the set.
   */
  def foreach(f: Tweet => Unit): Unit
}

class Empty extends TweetSet {
  override def filterAcc(p: (Tweet) => Boolean, acc: TweetSet): TweetSet = acc

  override def union(that: TweetSet): TweetSet = that

  override def moreRetweetsThan(t: Tweet): Boolean = false

  override def mostRetweeted: Tweet = throw new NoSuchElementException("Empty sets do not have a mostRetweeted")

  override def descendingByRetweet: TweetList = Nil

  /**
   * The following methods are already implemented
   * Why don't these have override keywords?
   */

  def contains(tweet: Tweet): Boolean = false

  def incl(tweet: Tweet): TweetSet = new NonEmpty(tweet, new Empty, new Empty)

  def remove(tweet: Tweet): TweetSet = this

  def foreach(f: Tweet => Unit): Unit = ()
}

class NonEmpty(elem: Tweet, left: TweetSet, right: TweetSet) extends TweetSet {

  override def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet = {
    val newTweetSet: TweetSet = if (p(elem)) acc.incl(elem) else acc
    left.filterAcc(p, right.filterAcc(p, newTweetSet))
  }

  override def union(that: TweetSet): TweetSet = left.union(right.union(that)).incl(elem)
//    (left union (right union that)) incl elem // identical to 1)
//    ((left union right) union that) incl elem // stack overflow - recursing through right 2x
//    left union (right union (that incl elem)) // ok

  override def moreRetweetsThan(t: Tweet): Boolean = {
    if (elem.retweets > t.retweets) true else right.moreRetweetsThan(t) || left.moreRetweetsThan(t)
  }

  override def mostRetweeted: Tweet = {
    val lMax = if (left.moreRetweetsThan(elem)) left.mostRetweeted else elem
    if (right.moreRetweetsThan(lMax)) right.mostRetweeted else lMax
    /* Original attempt, using isEmpty equivalently to try/catch for control flow
    val lMax = if (left.isEmpty) elem else {
      val l = left.mostRetweeted
      if (l.retweets > elem.retweets) l else elem
    }

    if (right.isEmpty) lMax
    else {
      val r = right.mostRetweeted
      if (r.retweets > lMax.retweets) r else lMax
    }
    */
  }

  override def descendingByRetweet: TweetList = {
    // 1st attempt was in abstract class
    val max = mostRetweeted
    new Cons(max, remove(max).descendingByRetweet)
  }

  /**
   * The following methods are already implemented
   */

  def contains(x: Tweet): Boolean =
    if (x.text < elem.text) left.contains(x)
    else if (elem.text < x.text) right.contains(x)
    else true

  def incl(x: Tweet): TweetSet = {
    if (x.text < elem.text) new NonEmpty(elem, left.incl(x), right)
    else if (elem.text < x.text) new NonEmpty(elem, left, right.incl(x))
    else this
  }

  def remove(tw: Tweet): TweetSet =
    if (tw.text < elem.text) new NonEmpty(elem, left.remove(tw), right)
    else if (elem.text < tw.text) new NonEmpty(elem, left, right.remove(tw))
    else left.union(right)

  def foreach(f: Tweet => Unit): Unit = {
    f(elem)
    left.foreach(f)
    right.foreach(f)
  }
}

trait TweetList {
  def head: Tweet
  def tail: TweetList
  def isEmpty: Boolean
  def foreach(f: Tweet => Unit): Unit =
    if (!isEmpty) {
      f(head)
      tail.foreach(f)
    }
}

object Nil extends TweetList {
  def head = throw new java.util.NoSuchElementException("head of EmptyList")
  def tail = throw new java.util.NoSuchElementException("tail of EmptyList")
  def isEmpty = true
}

class Cons(val head: Tweet, val tail: TweetList) extends TweetList {
  def isEmpty = false
}


object GoogleVsApple {
  val google = List("android", "Android", "galaxy", "Galaxy", "nexus", "Nexus")
  val apple = List("ios", "iOS", "iphone", "iPhone", "ipad", "iPad")
  val either = google.union(apple)

  lazy val googleTweets: TweetSet = TweetReader.allTweets.filter((t:Tweet) => google.exists((k: String) => t.text.contains(k)))
  lazy val appleTweets: TweetSet = TweetReader.allTweets.filter((t:Tweet) => apple.exists((k: String) => t.text.contains(k)))

  /**
   * A list of all tweets mentioning a keyword from either apple or google,
   * sorted by the number of retweets.
   */
  lazy val trending: TweetList = googleTweets.union(appleTweets).descendingByRetweet
}

object Main extends App {
  // Print the trending tweets
  val tic = System.currentTimeMillis()
  GoogleVsApple.trending foreach println
  val toc = System.currentTimeMillis()
  println("Elapsed: " + (toc-tic) + " ms")
}
