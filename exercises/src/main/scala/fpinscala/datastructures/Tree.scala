package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  def size[A](t: Tree[A]): Int = {
    def go(t: Tree[A], acc: Int): Int = t match {
      case Leaf(x) => acc + 1
      case Branch(left, right) => 1 + go(left, acc) + go(right, acc)
    }

    go(t, 0)
  }

  def max(t: Tree[Int]): Int = {
    def go(t: Tree[Int], acc: Int): Int = t match {
      case Leaf(x: Int) => x.max(acc)
      case Branch(left, right) => go(left, acc).max(go(right, acc))
    }

    go(t, Int.MinValue)
  }

  def depth[A](t: Tree[A]): Int = {
    def go(t: Tree[A], acc: Int): Int = t match {
      case Leaf(x) => acc + 1
      case Branch(left, right) => acc.max(go(left, acc + 1)).max(go(right, acc + 1))
    }

    go(t, 0)
  }

  def map[A,B](t: Tree[A])(f: (A => B)): Tree[B] = t match {
    case Leaf(x) => Leaf(f(x))
    case Branch(left, right) => Branch(map(left)(f), map(right)(f))
  }

  def fold[A,B](t: Tree[A])(l: A => B)(b: (B,B) => B): B =
    t match {
      case Leaf(x) => l(x)
      case Branch(left, right) => b(fold(left)(l)(b), fold(right)(l)(b))
    }

  def sizeFold[A](t: Tree[A]): Int = {
    fold(t)((x) => 1)((x, y) => 1 + x + y)
  }

  def maxFold(t: Tree[Int]): Int = {
    fold(t)((x: Int) => x)((x, y) => x.max(y))
  }

  def depthFold[A](t: Tree[A]): Int = {
    fold(t)((x) => 1)((x, y) => (x + 1).max(y + 1))
  }

  def mapFold[A,B](t: Tree[A])(f: (A => B)): Tree[B] = {
    fold(t)(x => Leaf(f(x)): Tree[B])((x, y) => Branch(x, y))
  }

}