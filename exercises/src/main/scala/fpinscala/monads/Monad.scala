package fpinscala
package monads

import java.util.concurrent.ExecutorService

import parsing._
import testing._
import parallelism._
import state._
import parallelism.Par._

import scala.collection.immutable.Stream.cons
import scala.concurrent.Future

trait Functor[F[_]] {
  def map[A,B](fa: F[A])(f: A => B): F[B]

  def distribute[A,B](fab: F[(A, B)]): (F[A], F[B]) =
    (map(fab)(_._1), map(fab)(_._2))

  def codistribute[A,B](e: Either[F[A], F[B]]): F[Either[A, B]] = e match {
    case Left(fa) => map(fa)(Left(_))
    case Right(fb) => map(fb)(Right(_))
  }
}

object Functor {
  val listFunctor = new Functor[List] {
    def map[A,B](as: List[A])(f: A => B): List[B] = as map f
  }
}

trait Monad[M[_]] extends Functor[M] {
  def unit[A](a: => A): M[A]
  def flatMap[A,B](ma: M[A])(f: A => M[B]): M[B]

  def map[A,B](ma: M[A])(f: A => B): M[B] =
    flatMap(ma)(a => unit(f(a)))

  def map2[A,B,C](ma: M[A], mb: M[B])(f: (A, B) => C): M[C] =
    flatMap(ma)(a => map(mb)(b => f(a, b))) // A => map(M[B])(B => (A, B) => C)

  def sequence[A](lma: List[M[A]]): M[List[A]] =
    lma.foldRight(unit(List[A]()))((a, acc) => map2(a, acc)(_ :: _))

  def traverse[A,B](la: List[A])(f: A => M[B]): M[List[B]] =
    la.foldRight(unit(List[B]()))((a: A, b: M[List[B]]) => map2(f(a), b)(_ :: _))



//  def map[A,B](ma: M[A])(f: A => B): M[B] =
//    ???
//
//  def map2[A,B,C](ma: M[A], mb: M[B])(f: (A, B) => C): M[C] =
//    ???
//
//  def sequence[A](lma: List[M[A]]): M[List[A]] =
//    ???
//
//  def traverse[A,B](la: List[A])(f: A => M[B]): M[List[B]] =
//    ???


  def replicateM[A](n: Int, ma: M[A]): M[List[A]] =
    sequence(List.fill(n)(ma))

  // Ex11.5
  // In the list monad replicate will replicate all possible permutations of the list
  // In the option monad replicate will simply replicate the option value n times

  def compose[A,B,C](f: A => M[B], g: B => M[C]): A => M[C] =
    a => flatMap(f(a))(g)

  // Implement in terms of `compose`:
  def _flatMap[A,B](ma: M[A])(f: A => M[B]): M[B] = ???


  // Ex11.9
  // x.flatMap(f).flatMap(g).flatMap(h) == x.flatMap(a => f(a).flatMap(g)).flatMap(h)
  // x.flatMap(A => F[B]).flatMap(B => F[C]).flatMap(C => F[D] == x.flatMap(A => F[B]).flatMap(B => F[C]).flatMap(C => F[D])
  //
  // compose(compose(f, g), h) == compose(f, compose(g, h))
  // for f: A => F[B], g: B => F[C], h: C => F[D]:
  // compose(compose(A => F[B], B => F[C]), C => F[D]) == compose(A => F[B], compose(B => F[C], C => F[D])
  // compose(A => F[B], B => F[D]) == compose(A => F[B], B => F[D])
  // A => F[D] == A => F[D]
  //
  // => A => F[D] == x.flatMap(A => F[B]).flatMap(B => F[C]).flatMap(C => F[D])


  // def unit[A](a: => A): M[A]
  // def flatMap[A,B](ma: M[A])(f: A => M[B]): M[B]

  // Ex11.10
  // compose(f, unit) == compose(unit, f)
  // compose(A => F[B], A => F[A]) == compose(A => F[A], A => F[B])
  // ( A => F[B]) == ( A => F[B])
  // ( A => F[B]) == f
  //
  // flatMap(x)(unit) == x
  // flatMap(F[A])(A => F[A]) == F[A]
  //
  // flatMap(unit(y))(f) == f(y)
  // flatMap(A => F[A])(A => F[B]) == (A => F[B])
  // (A => F[B]) == f

  // Ex11.11
  // flatMap(Some(x))(a => Some(a)) == Some(x)
  // flatMap(x => Some(x))(a => Some(List(a)) == (x => Some(List(x))

  def join[A](mma: M[M[A]]): M[A] = flatMap(mma)(ma => ma)

  // Implement in terms of `join`:
  def __flatMap[A,B](ma: M[A])(f: A => M[B]): M[B] = join(map(ma)(f))

  // Ex11.14
  // Associative Law
  // join(map(mma)(join)) == (join(join(mma)))
  // join(map(M[M[A]])(join)) == join(join(M[M[A]]))
  // join(M[M[A]]) == join(M[M[A]])

  // Identify Law
  // join(map(x)(unit)) == x  // right identity
  // join(unit(x)) == x  // left identity

  // Ex11.15
  // Associative Laws implications for Par & Parser are that the
  // order in which executions/parsing takes place does not effect
  // the produced result

  // Ex11.16
  // The identity laws
  // Lifting these types via unit, then applying flatmap flattens them
  // back to their original values.

  // Ex11.18
  // ReplicateM with the state monad creates a list of M random
  // numbers with the generator => (List(v1, ..., vM), RNG)
  // map2 will apply a function to the numbers generated from
  // each RNG instance passed in.
  // Sequence will generate a sequence of values in a list

  // Ex11.19
  // We expect the associativity & identity laws to hold.
}

object Monad {
  val genMonad = new Monad[Gen] {
    def unit[A](a: => A): Gen[A] = Gen.unit(a)
    override def flatMap[A,B](ma: Gen[A])(f: A => Gen[B]): Gen[B] =
      ma flatMap f
  }

  val parMonad: Monad[Par] = new Monad[Par] {
    override def unit[A](a: => A): Par[A] = Par.unit(a)

    override def flatMap[A, B](ma: Par[A])(f: (A) => Par[B]): Par[B] =
      es => f(ma(es).get)(es)
  }

  // TODO: Complete parsers & come back to this
  def parserMonad[P[+_]](p: Parsers[P]): Monad[P] = new Monad[P] {
    override def unit[A](a: => A): P[A] = ???
    override def flatMap[A, B](ma: P[A])(f: (A) => P[B]): P[B] = ???
  }

  val optionMonad: Monad[Option] = new Monad[Option] {
    override def unit[A](a: => A): Option[A] = Option(a)

    override def flatMap[A, B](ma: Option[A])(f: (A) => Option[B]): Option[B] =
      ma match {
        case Some(x) => f(x)
        case None => None
      }
  }

  val streamMonad: Monad[Stream] = new Monad[Stream] {
    override def unit[A](a: => A): Stream[A] = cons(a, Stream.empty)

    override def flatMap[A, B](ma: Stream[A])(f: (A) => Stream[B]): Stream[B] =
      ma.flatMap(f)
  }

  val listMonad: Monad[List] = new Monad[List] {
    override def unit[A](a: => A): List[A] = List(a)
    override def flatMap[A, B](ma: List[A])(f: (A) => List[B]): List[B] = ma.flatMap(f)
  }

  def stateMonad[S] = new Monad[({type f[x] = State[S,x]})#f] {
    def unit[A](a: => A): State[S, A] = State(s => (a, s))

    def flatMap[A, B](st: State[S, A])(f: A => State[S, B]): State[S, B] =
      st flatMap f
  }

  val idMonad: Monad[Id] = new Monad[Id] {
    override def unit[A](a: => A): Id[A] = Id(a)
    override def flatMap[A, B](ma: Id[A])(f: A => Id[B]): Id[B] = ma.flatMap(f)
  }

  def readerMonad[R] = ???
}

case class Id[A](value: A) {
  def map[B](f: A => B): Id[B] = Id(f(value))
  def flatMap[B](f: A => Id[B]): Id[B] = f(value)
}

case class Reader[R, A](run: R => A)

object Reader {
  def readerMonad[R] = new Monad[({type f[x] = Reader[R,x]})#f] {
    def unit[A](a: => A): Reader[R,A] = Reader(_ => a)
    override def flatMap[A,B](st: Reader[R,A])(f: A => Reader[R,B]): Reader[R,B] =
      Reader(r => f(st.run(r)).run(r))
      // Reader should run st, but also pass r to the result of running st
  }
}

