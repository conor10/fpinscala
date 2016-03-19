package fpinscala.monads

import fpinscala.state.RNG.Simple
import fpinscala.state.{RNG, State}
import org.scalatest.FunSuite

/**
 * Monad unit tests.
 */
class MonadTest extends FunSuite {
  test("ReplicateM") {
    val n = 2
    val optionSomeReplicate = Monad.optionMonad.replicateM(n, Option("Some"))
    println(optionSomeReplicate)
    val optionNoneReplicate = Monad.optionMonad.replicateM(n, None)
    println(optionNoneReplicate)
    val listReplicate = Monad.listMonad.replicateM(n, List(1, 2, 3))
    println(listReplicate)
  }

  test("flatMap") {
    val l = List(1, 2, 3)
    val s = l.flatMap(x => Option(x))
    println(s)
  }

  test("stateMonad") {
    val simple = Simple(10L)
    val nonNegative = State(RNG.nonNegativeInt)
    val boolean = State(RNG.boolean)
    val replicated = Monad.stateMonad.replicateM(3, nonNegative)
    val map2 = Monad.stateMonad.map2(nonNegative, nonNegative)((x, y) => x - y)
    val sequence = Monad.stateMonad.sequence(List(nonNegative, boolean))

    println("replicated: " + replicated.run(simple))
    println("map2: " + map2.run(simple))
    println("sequence: " + sequence.run(simple))
  }
}
