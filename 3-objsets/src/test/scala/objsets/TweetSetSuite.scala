package objsets

class TweetSetSuite extends munit.FunSuite {
  trait TestSets {
    val set1 = new Empty
    val set2: TweetSet = set1.incl(new Tweet("a", "a body", 20))
    val set3: TweetSet = set2.incl(new Tweet("b", "b body", 20))
    val c = new Tweet("c", "c body", 7)
    val d = new Tweet("d", "d body", 9)
    val set4c: TweetSet = set3.incl(c)
    val set4d: TweetSet = set3.incl(d)
    val set5: TweetSet = set4c.incl(d)
  }

  def asSet(tweets: TweetSet): Set[Tweet] = {
    var res = Set[Tweet]()
    tweets.foreach(res += _)
    res
  }

  def size(set: TweetSet): Int = asSet(set).size

  test("filter: on empty set") {
    new TestSets {
      val filteredSet: TweetSet = set1.filter(tw => tw.user == "a")
      assertEquals(size(filteredSet), 0)
    }
  }

  test("filter: a on set5") {
    new TestSets {
      assertEquals(size(set5.filter(tw => tw.user == "a")), 1)
    }
  }

  test("filter: twenty on set5") {
    new TestSets {
      assertEquals(size(set5.filter(tw => tw.retweets == 20)), 2)
    }
  }

  test("union: set4c and set4d") {
    new TestSets {
      assertEquals(size(set4c.union(set4d)), 4)
    }
  }

  test("union: with empty set1") {
    new TestSets {
      assertEquals(size(set5.union(set1)), 4)
    }
  }

  test("union: with empty set2") {
    new TestSets {
      assertEquals(size(set1.union(set5)), 4)
    }
  }

  test("most retweeted: with empty") {
    new TestSets {
      try {
        set1.mostRetweeted
        fail("needs exception")
      }
      catch {
        case e: NoSuchElementException => ;
      }

    }
  }

  test("most retweeted: just works") {
    new TestSets {
      val setZ: TweetSet = set1.incl(c).incl(d)
      val mostTweet: Tweet = setZ.mostRetweeted

      assertEquals(mostTweet.retweets, 9)
    }
  }

  test("descending: set5") {
    new TestSets {
      val trends = set5.descendingByRetweet
      assert(!trends.isEmpty)
      assert(trends.head.user == "a" || trends.head.user == "b")
    }
  }


  test("descending: real") {
    new TestSets {
      val seta: TweetSet = set1.incl(new Tweet("a", "a body", 205))
      val setb: TweetSet = seta.incl(new Tweet("b", "b body", 321))

      val trends: TweetList = setb.descendingByRetweet

      assert(trends.head.user == "b")
    }
  }



  import scala.concurrent.duration._

  override val munitTimeout = 10.seconds
}
