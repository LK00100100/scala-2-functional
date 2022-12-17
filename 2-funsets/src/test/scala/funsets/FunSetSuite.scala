package funsets

import funsets.FunSets.{FunSet, singletonSet, union}

/**
 * This class is a test suite for the methods in object FunSets.
 *
 * To run this test suite, start "sbt" then run the "test" command.
 */
class FunSetSuite extends munit.FunSuite {

  def createSet(numbers: Int*): FunSet = {
    if (numbers.isEmpty)
      throw new IllegalArgumentException("needs numbers")

    numbers.head match {
      case x if numbers.tail == Nil => singletonSet(x)
      case x => union(singletonSet(x), createSet(numbers.tail: _*))
    }
  }

  import FunSets._

  test("contains is implemented") {
    assert(contains(_ => true, 100))
  }

  /**
   * When writing tests, one would often like to re-use certain values for multiple
   * tests. For instance, we would like to create an Int-set and have multiple test
   * about it.
   *
   * Instead of copy-pasting the code for creating the set into every test, we can
   * store it in the test class using a val:
   *
   * val s1 = singletonSet(1)
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
    val s1: FunSet = singletonSet(1)
    val s2: FunSet = singletonSet(2)
    val s3: FunSet = singletonSet(3)
  }

  /**
   * This test is currently disabled (by using @Ignore) because the method
   * "singletonSet" is not yet implemented and the test would fail.
   *
   * Once you finish your implementation of "singletonSet", remove the
   * .ignore annotation.
   */
  test("singleton set one contains one") {

    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3".
     */
    new TestSets {
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      assert(contains(s1, 1), "Singleton")
    }
  }

  test("union contains all elements of each set") {
    new TestSets {
      val s: FunSet = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }

  test("intersect contains intersect of each set") {
    new TestSets {
      val u1: FunSet = union(s1, s2)
      val u2: FunSet = union(s2, s3)
      val intersectSet: FunSet = intersect(u1, u2)

      assert(contains(intersectSet, 2), "intersect should have 2")
      assert(!contains(intersectSet, 1), "should not contain")
      assert(!contains(intersectSet, 3), "should not contain")
    }
  }

  test("diff should work") {
    new TestSets {
      val u1: FunSet = union(s1, s2)
      val u2: FunSet = union(s2, s3)
      val diffSet: FunSet = diff(u1, u2)

      print(FunSets.toString(diffSet))

      assert(contains(diffSet, 1), "diff should have")
      assert(!contains(diffSet, 2), "should not contain")
      assert(!contains(diffSet, 3), "should not contain")
    }
  }

  test("diff should work - real") {
    new TestSets {
      val u1: FunSet = createSet(1, 3, 4, 5, 7, 1000)
      val u2: FunSet = createSet(1, 2, 3, 4)
      val diffSet: FunSet = diff(u1, u2)

      print(FunSets.toString(diffSet))

      assert(contains(diffSet, 5), "diff should have")
      assert(contains(diffSet, 7), "diff should have")
      assert(contains(diffSet, 1000), "diff should have")
    }
  }

  test("diff should work - real 2") {
    new TestSets {
      val u1: FunSet = createSet(1, 2, 3, 4)
      val u2: FunSet = createSet(-1000, 0)
      val diffSet: FunSet = diff(u1, u2)

      // print(FunSets.toString(diffSet))

      assert(contains(diffSet, 1), "diff should have")
      assert(contains(diffSet, 2), "diff should have")
      assert(contains(diffSet, 3), "diff should have")
      assert(contains(diffSet, 4), "diff should have")
    }
  }

  test("forall should work - within bounds of -/+ 1000") {
    new TestSets {
      val u1: FunSet = union(s1, s2)
      val predicate: Int => Boolean = (x: Int) => x < 10
      val predicateBad: Int => Boolean = (x: Int) => x < 0

      assert(forall(u1, predicate), "all elements should have true predicate")
      assert(!forall(u1, predicateBad), "should have been false")
    }
  }


  test("forall should fail - real") {
    new TestSets {
      val u1: FunSet = createSet(1, 3, 4, 5, 7, 1000)

      val predicate: Int => Boolean = (x: Int) => x < 5

      assert(!forall(u1, predicate), "all elements shouldn't have true predicate")
    }
  }

  test("exists should work - within bounds of -/+ 1000") {
    new TestSets {
      val u1: FunSet = union(s1, s2)
      val predicate: Int => Boolean = (x: Int) => x == 2
      val predicateBad: Int => Boolean = (x: Int) => x == -10

      assert(exists(u1, predicate), "one element should have true predicate")
      assert(!exists(u1, predicateBad), "should have been false")
    }
  }

  test("map should work") {
    new TestSets {
      val u1: FunSet = union(s1, s2)
      val mapper: Int => Int = (x: Int) => x * 4
      val mappedSet: FunSet = map(u1, mapper)

      assert(contains(mappedSet, 4), "should have new mapped val")
      assert(contains(mappedSet, 8), "should have new mapped val")
      assert(!contains(mappedSet, 1), "shouldn't have new mapped val")
    }
  }


  import scala.concurrent.duration._

  override val munitTimeout: FiniteDuration = 10.seconds
}
