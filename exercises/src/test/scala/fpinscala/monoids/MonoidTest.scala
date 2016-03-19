package fpinscala.monoids

import fpinscala.testing.Gen
import org.scalatest.FunSuite

import Monoid._

/**
 * Monoid unit tests.
 */
class MonoidTest extends FunSuite {
  test("monoidLaws") {
    monoidLaws(intAddition, Gen.int).run
  }

  test("foldMapV") {
    
  }

  test("parFoldMap") {

  }

  test("wcMonoid") {
    assert(5 === count("lorem ipsum dolar sit amet, "))
  }
}
