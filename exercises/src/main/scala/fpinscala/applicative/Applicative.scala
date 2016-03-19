package fpinscala
package applicative

import monads.Functor
import state._
import State._
import scala.{Option => _, Either => _, Left => _, Right => _, _}
import fpinscala.errorhandling.{Either, Right, Left}
import StateUtil._ // defined at bottom of this file
import monoids._

trait Applicative[F[_]] extends Functor[F] {

  // apply & unit
  def map2[A,B,C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    apply(apply(unit(f.curried))(fa))(fb)
    // Equivalent to
//    apply(map(fa)(f.curried))(fb)


  def map[A,B](fa: F[A])(f: A => B): F[B] =
  //    map2(fa, unit(()))((a, _) => f(a))
    apply(unit(f))(fa)


  // map2 & unit
  def apply[A,B](fab: F[A => B])(fa: F[A]): F[B] =
    map2(fab, fa)((ab, a) => ab(a))
//    map(fab)((f: (A => B)) => map(fa)(f))

  def unit[A](a: => A): F[A] = ???


  def sequence[A](fas: List[F[A]]): F[List[A]] =
    traverse(fas)(a => a)

  def traverse[A,B](as: List[A])(f: A => F[B]): F[List[B]] =
    as.foldRight(unit(List[B]()))((a, acc) => map2(f(a), acc)(_ :: _))

  def replicateM[A](n: Int, fa: F[A]): F[List[A]] =
    sequence(List.fill(n)(fa))

  // This is named product in the book
  def factor[A,B](fa: F[A], fb: F[B]): F[(A,B)] =
    map2(fa, fb)((a, b) => (a, b))

  // using unit & applied
  def map3[A,B,C,D](fa: F[A], fb: F[B],
                    fc: F[C])(f: (A, B, C) => D): F[D] =
    f.curried((ab: A => B) => apply(unit(ab))(fa))((bc: B => C) => apply(unit(bc))(fb))((cd: C => D) => apply(unit(cd))(fc))

  def map4[A,B,C,D,E](fa: F[A], fb: F[B], fc: F[C],
                      fd: F[D])(f: (A, B, C, D) => E): F[E] =
    f.curried((ab: A => B) => apply(unit(ab))(fa))((bc: B => C) => apply(unit(bc))(fb))((cd: C => D) => apply(unit(cd))(fc))((de: D => E) => apply(unit(de))(fd))

  def product[G[_]](G: Applicative[G]): Applicative[({type f[x] = (F[x], G[x])})#f] = {
    val self = this
    new Applicative[({type f[x] = (F[x], G[x])})#f] {
      override def unit[A] (a: => A): (F[A], G[A]) = (self.unit(a), G.unit(a))
      override def map2[A,B,C] (fa: (F[A], G[A]), gb: (F[B], G[B]) ) (f: (A, B) => C): (F[C], G[C]) =
        apply(map(fa)(f.curried))(gb)
    }
  }

  def compose[G[_]](G: Applicative[G]): Applicative[({type f[x] = F[G[x]]})#f] = {
    val self = this
    new Applicative[({type f[x] = F[G[x]]})#f] {
      override def unit[A] (a: => A): (F[G[A]]) = self.unit(G.unit(a))
      override def map2[A,B,C](fa: F[G[A]], gb: F[G[B]])(f: (A, B) => C): F[G[C]] = {
        apply(map(fa)(f.curried))(gb)
      }
    }
  }

  def sequenceMap[K,V](ofa: Map[K,F[V]]): F[Map[K,V]] =
    ofa.foldRight(unit(Map[K,V]()))((kfv: (K,F[V]), fmkv: F[Map[K,V]]) => map2(kfv._2, fmkv)((v, mkv) => mkv + (kfv._1 -> v)))
}

case class Tree[+A](head: A, tail: List[Tree[A]])

trait Monad[F[_]] extends Applicative[F] {
  def flatMap[A,B](ma: F[A])(f: A => F[B]): F[B] = join(map(ma)(f))

  def join[A](mma: F[F[A]]): F[A] = flatMap(mma)(ma => ma)

  def compose[A,B,C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => flatMap(f(a))(g)

  override def apply[A,B](mf: F[A => B])(ma: F[A]): F[B] =
    flatMap(mf)(f => map(ma)(a => f(a)))

  def compose[G[_]](G: Monad[G]): Monad[({type f[x] = F[G[x]]})#f] = {
    val self = this
    new Monad[({type f[x] = F[G[x]]})#f] {
      override def unit[A](a: => A): (F[G[A]]) = self.unit(G.unit(a))
      override def flatMap[A,B](fga: F[G[A]])(f: A => F[G[B]]): F[G[B]] = {
        self.flatMap(fga)(f)
      }
    }
  }
}



object Monad {
  def eitherMonad[E] = new Monad[({type f[x] = Either[E, x]})#f] {
    override def unit[A](a: => A): Either[E, A] = a match {
      case e: E => Left(e)
      case a: A => Right(a)
    }
    override def flatMap[A,B](e: Either[E, A])(f: A => Either[E,B]): Either[E,B] = e match {
      case Left(x) => Left(x)
      case Right(x) => f(x)
    }
  }

  def stateMonad[S] = new Monad[({type f[x] = State[S, x]})#f] {
    override def unit[A](a: => A): State[S, A] = State(s => (a, s))
    override def flatMap[A,B](st: State[S, A])(f: A => State[S, B]): State[S, B] =
      st flatMap f
  }

  def composeM[F[_],N[_]](implicit F: Monad[F], N: Monad[N], T: Traverse[N]):
    Monad[({type f[x] = F[N[x]]})#f] = ???
}

sealed trait Validation[+E, +A]

case class Failure[E](head: E, tail: Vector[E])
  extends Validation[E, Nothing]

case class Success[A](a: A) extends Validation[Nothing, A]


object Applicative {

  val streamApplicative = new Applicative[Stream] {

    override def unit[A](a: => A): Stream[A] =
      Stream.continually(a) // The infinite, constant stream

    override def map2[A,B,C](a: Stream[A], b: Stream[B])( // Combine elements pointwise
                    f: (A,B) => C): Stream[C] =
      a zip b map f.tupled
  }

  def validationApplicative[E] = new Applicative[({type f[x] = Validation[E,x]})#f] {
    override def unit[A](a: => A): Validation[E, A] = a match {
      case a: A => Success(a)
      case a: E => Failure(a, Vector())
    }

    override def map2[A, B, C](a: Validation[E,A], b: Validation[E,B])(f: (A, B) => C): Validation[E,C] =
      a match {
        case Success(as) => b match {
          case Success(bs) => unit(f(as, bs))
          case fb: Failure[E] => fb
        }
        case fa: Failure[E] => b match {
          case Success(bs) => fa
          case fb: Failure[E] => Failure(fa.head, fa.tail ++ (fb.head +: fb.tail))
        }
      }
  }

  type Const[A, B] = A

  implicit def monoidApplicative[M](M: Monoid[M]) =
    new Applicative[({ type f[x] = Const[M, x] })#f] {
      def unit[A](a: => A): M = M.zero
      override def apply[A,B](m1: M)(m2: M): M = M.op(m1, m2)
    }
}

trait Traverse[F[_]] extends Functor[F] with Foldable[F] {
  def traverse[G[_]:Applicative,A,B](fa: F[A])(f: A => G[B]): G[F[B]] =
    sequence(map(fa)(f))
  def sequence[G[_]:Applicative,A](fma: F[G[A]]): G[F[A]] =
    traverse(fma)(ma => ma)

  type Id[A] = A
  val idMonad = new Monad[Id] {
    def unit[A](a: => A) = a
    override def flatMap[A,B](a: A)(f: A => B): B = f(a)
  }

  def map[A,B](fa: F[A])(f: A => B): F[B] =
    traverse[Id, A, B](fa)(f)(idMonad)

  import Applicative._

  override def foldMap[A,B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
    traverse[({type f[x] = Const[B,x]})#f,A,Nothing](
      as)(f)(monoidApplicative(mb))

  def traverseS[S,A,B](fa: F[A])(f: A => State[S, B]): State[S, F[B]] =
    traverse[({type f[x] = State[S,x]})#f,A,B](fa)(f)(Monad.stateMonad)

  def mapAccum[S,A,B](fa: F[A], s: S)(f: (A, S) => (B, S)): (F[B], S) =
    traverseS(fa)((a: A) => (for {
      s1 <- get[S]
      (b, s2) = f(a, s1)
      _  <- set(s2)
    } yield b)).run(s)

  override def toList[A](fa: F[A]): List[A] =
    mapAccum(fa, List[A]())((a, s) => ((), a :: s))._2.reverse

  def zipWithIndex[A](fa: F[A]): F[(A, Int)] =
    mapAccum(fa, 0)((a, s) => ((a, s), s + 1))._1

  // F is in effect a series of state transitions
  def reverse[A](fa: F[A]): F[A] =
    mapAccum(fa, toList(fa).reverse)((a, s) => (s.head, s.tail))._1

  override def foldLeft[A,B](fa: F[A])(z: B)(f: (B, A) => B): B =
    mapAccum(fa, z)((a, s) => ((), f(s, a)))._2

  def fuse[G[_],H[_],A,B](fa: F[A])(f: A => G[B], g: A => H[B])
                         (implicit G: Applicative[G], H: Applicative[H]): (G[F[B]], H[F[B]]) =
    traverse[({type f[x] = (G[x], H[x])})#f, A, B](fa)(a => (f(a), g(a)))

  // def traverse[G[_]:Applicative,A,B](fa: F[A])(f: A => G[B]): G[F[B]]
  // def product[G[_]](G: Applicative[G]): Applicative[({type f[x] = (F[x], G[x])})#f]
  // Answer also adds (G.product(H)) to the end - not clear why


  def compose[G[_]](implicit G: Traverse[G]): Traverse[({type f[x] = F[G[x]]})#f] = {
      val self = this
      new Traverse[( {type f[x] = F[G[x]]}) #f] {
        override def traverse[M[_]: Applicative, A, B] (fa: F[G[A]]) (f: A => M[B] ) = ???
      }
    }
  }

object Traverse {
  val listTraverse = new Traverse[List] {
    override def traverse[M[_],A,B](as: List[A])(f: A => M[B])(implicit M: Applicative[M]): M[List[B]] =
      as.foldRight(M.unit(List[B]()))((a, acc) => M.map2(f(a), acc)(_ :: _))
  }

  val optionTraverse = new Traverse[Option] {
    override def traverse[M[_],A,B](opt: Option[A])(f: A => M[B])(implicit M: Applicative[M]): M[Option[B]] =
      opt match {
        case Some(a) => M.map(f(a))(Option(_))
        case None => M.unit(None)
      }
  }

  // case class Tree[+A](head: A, tail: List[Tree[A]])
  // M[Tree[B]] = M[Tree(B, List[M[Tree[B]]])
  // override def apply[A,B](mf: F[A => B])(ma: F[A]): F[B] =
  val treeTraverse = new Traverse[Tree] {
    override def traverse[M[_],A,B](t: Tree[A])(f: A => M[B])(implicit M: Applicative[M]): M[Tree[B]] = {
      M.map2(f(t.head), listTraverse.traverse(t.tail)(traverse(_)(f)))(Tree(_, _))
    }
  }
}

// The `get` and `set` functions on `State` are used above,
// but aren't in the `exercises` subproject, so we include
// them here
object StateUtil {

  def get[S]: State[S, S] =
    State(s => (s, s))

  def set[S](s: S): State[S, Unit] =
    State(_ => ((), s))
}
