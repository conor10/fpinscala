package fpinscala.parallelism

import java.util.concurrent._

object Par {
  type Par[A] = ExecutorService => Future[A]

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a) // `unit` is represented as a function that returns a
  // `UnitFuture`, which is a simple implementation of `Future` that just wraps a constant value.
  // It doesn't use the `ExecutorService` at all. It's always done and can't be cancelled. Its `get` method simply returns the value that we gave it.

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true

    def get(timeout: Long, units: TimeUnit) = get

    def isCancelled = false

    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = // `map2` doesn't evaluate the call to `f` in a separate logical thread,
  // in accord with our design choice of having `fork` be the sole function in the API for controlling parallelism.
  // We can always do `fork(map2(a,b)(f))` if we want the evaluation of `f` to occur in a separate thread.
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      UnitFuture(f(af.get, bf.get)) // This implementation of `map2` does _not_ respect timeouts, and eagerly waits for the returned futures.
      // This means that even if you have passed in "forked" arguments, using this map2 on them will make them wait.
      // It simply passes the `ExecutorService` on to both `Par` values, waits for the results of the Futures `af` and `bf`,
      // applies `f` to them, and wraps them in a `UnitFuture`. In order to respect timeouts, we'd need a new `Future` implementation
      // that records the amount of time spent evaluating `af`, then subtracts that time from the available time allocated for evaluating `bf`.
    }

  def fork[A](a: => Par[A]): Par[A] = // This is the simplest and most natural implementation of `fork`,
  // but there are some problems with it--for one, the outer `Callable` will block waiting for the "inner" task to complete.
  // Since this blocking occupies a thread in our thread pool, or whatever resource backs the `ExecutorService`,
  // this implies that we're losing out on some potential parallelism. Essentially, we're using two threads when one should suffice.
  // This is a symptom of a more serious problem with the implementation, and we will discuss this later in the chapter.
    es => es.submit(new Callable[A] {
      def call = a(es).get
    })

  def map[A, B](pa: Par[A])(f: A => B): Par[B] =
    map2(pa, unit(()))((a, _) => f(a))

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def asyncF[A, B](f: A => B): A => Par[B] = (x: A) => lazyUnit(f(x))

  def syncF[A, B](f: A => B): A => Par[B] = (x: A) => unit(f(x))

  def sortPar(parList: Par[List[Int]]) = map(parList)(_.sorted)

  def sequence[A](ps: List[Par[A]]): Par[List[A]] = {
    (es: ExecutorService) => {
      UnitFuture(loop(ps)(es))
    }

    def loop(ps: List[Par[A]])(es: ExecutorService): List[A] =
      ps match {
        case x :: xs => x(es).get :: loop(xs)(es)
        case _ => Nil
      }
  }

  // Solution in book
  def sequence_simple[A](l: List[Par[A]]): Par[List[A]] =
    l.foldRight[Par[List[A]]](unit(List()))((h,t) => map2(h,t)(_ :: _))

  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
    val l: List[Par[List[A]]] = as.map(
      asyncF((x: A) => if (f(x)) List(x) else Nil))
    map(sequence_simple(l))(_.flatten)
  }

  def equal[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean = 
    p(e).get == p2(e).get

  def delay[A](fa: => Par[A]): Par[A] = 
    es => fa(es)

  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    es => 
      if (run(es)(cond).get) t(es) // Notice we are blocking on the result of `cond`.
      else f(es)

  def choiceN[A](p: Par[Int])(ps: List[Par[A]]): Par[A] =
    es => {
      val n = run(es)(p).get
      ps(n)(es)
    }

  def choiceViaChoiceN[A](a: Par[Boolean])(ifTrue: Par[A], ifFalse: Par[A]): Par[A] =
    choiceN(map(a)((x: Boolean) => if (x) 0 else 1))(List(ifTrue, ifFalse))

  def choiceMap[K,V](p: Par[K])(ps: Map[K,Par[V]]): Par[V] =
    es => {
      val k = run(es)(p).get
      ps(k)(es)
    }

  def chooser[A,B](p: Par[A])(f: A => Par[B]): Par[B] =
    es => {
      val k = run(es)(p).get
      f(k)(es)
    }

  def choiceViaChooser[A](p: Par[Boolean])(f: Par[A], t: Par[A]): Par[A] =
    chooser(p)(if (_) t else f)

  def choiceNChooser[A](p: Par[Int])(choices: List[Par[A]]): Par[A] =
    chooser(p)(choices(_))

  def flatMap[A,B](p: Par[A])(f: A => Par[B]): Par[B] =
    chooser(p)(f)

  // Could be more concise, but more readable as per below
  // es => run(es)(run(es)(p).get())
  def join[A](p: Par[Par[A]]): Par[A] =
    es => {
      val k = run(es)(p).get
      run(es)(k)
    }

  def joinViaFlatMap[A](a: Par[Par[A]]): Par[A] =
    flatMap(a)(k => es => run(es)(k))

  def flatMapViaJoin[A,B](p: Par[A])(f: A => Par[B]): Par[B] =
    join(map(p)(f))

  /* Gives us infix syntax for `Par`. */
  implicit def toParOps[A](p: Par[A]): ParOps[A] = new ParOps(p)

  class ParOps[A](p: Par[A]) {


  }
}

object Examples {
  import Par._
  def sum(ints: IndexedSeq[Int]): Int = // `IndexedSeq` is a superclass of random-access sequences like `Vector` in the standard library.
  // Unlike lists, these sequences provide an efficient `splitAt` method for dividing them into two parts at a particular index.
    if (ints.size <= 1)
      ints.headOption getOrElse 0 // `headOption` is a method defined on all collections in Scala. We saw this function in chapter 3.
    else { 
      val (l,r) = ints.splitAt(ints.length/2) // Divide the sequence in half using the `splitAt` function.
      sum(l) + sum(r) // Recursively sum both halves and add the results together.
    }

}
