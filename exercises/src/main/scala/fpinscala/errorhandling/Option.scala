package fpinscala.errorhandling


import scala.{Option => _, Some => _, Either => _, _} // hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case Some(x) => Some(f(x))
    case _ => None
  }

  def getOrElse[B>:A](default: => B): B = this match {
    case Some(x) => x
    case _ => default
  }

  def flatMap[B](f: A => Option[B]): Option[B] = map(f)getOrElse(None)

  def orElseMatch[B>:A](ob: => Option[B]): Option[B] = this match {
    case Some(x) => this
    case _ => ob
  }

  def orElse[B>:A](ob: => Option[B]): Option[B] = map(Some(_)).getOrElse(ob)

  def filter(f: A => Boolean): Option[A] =
    flatMap(x => if (f(x)) Some(x) else None)
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def failingFn(i: Int): Int = {
    val y: Int = throw new Exception("fail!") // `val y: Int = ...` declares `y` as having type `Int`, and sets it equal to the right hand side of the `=`.
    try {
      val x = 42 + 5
      x + y
    }
    catch { case e: Exception => 43 } // A `catch` block is just a pattern matching block like the ones we've seen. `case e: Exception` is a pattern that matches any `Exception`, and it binds this value to the identifier `e`. The match returns the value 43.
  }

  def failingFn2(i: Int): Int = {
    try {
      val x = 42 + 5
      x + ((throw new Exception("fail!")): Int) // A thrown Exception can be given any type; here we're annotating it with the type `Int`
    }
    catch { case e: Exception => 43 }
  }

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs) flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))
  }

  def map2Match[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    (a, b) match {
      case (Some(x), Some(y)) => Some(f(x, y))
      case (_, _) => None
    }

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a.flatMap(x => b.map(y => f(x, y)))


  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    def loop(a: List[Option[A]], acc: List[A]) : Option[List[A]] = {
      a match {
//        case Some(x) :: Nil => Some(acc :+ x)
        case Some(x) :: xs => loop(xs, acc :+ x)
        case Nil => Some(acc)
        case _ => None
      }
    }
    loop(a, List())
  }

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    def loop(a: List[A], acc: List[B]): Option[List[B]] = {
      a match {
//        case x :: Nil => f(x) match {
//          case Some(x) => Some(acc :+ x)
//          case _ => None
//        }
        case x :: xs => f(x) match {
          case Some(x) => loop(xs, acc :+ x)
          case _ => None
        }
        case Nil => Some(acc)
        case _ => None
      }
    }

    loop(a, List())
  }

  def traverseMap[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    a match {
      case Nil => Some(Nil)
      case x :: xs => map2(f(x), traverseMap(xs)(f))(_::_)
    }
  }

  // Sequence in terms of traverse
  def seqTraverse[A](a: List[Option[A]]): Option[List[A]] = {
    traverse(a)((x) => x)
  }

  def lift[A,B](f: A => B): Option[A] => Option[B] = _ map f
}

