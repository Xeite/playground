sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + size(l) + size(r)
  }

  def sizeViaFold[A](t: Tree[A]): Int =
    fold(t)(_ => 1)(1 + _ + _)

  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(n) => n
    case Branch(l, r) => maximum(l) max maximum(r)
  }

  def maximumViaFold(t: Tree[Int]): Int =
    fold(t)(n => n)(_ max _)

  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 0
    case Branch(l, r) => 1 + (depth(l) max depth(r))
  }

  def depthViaFold[A](t: Tree[A]): Int =
    fold(t)(n => 0)((l, r) => 1 + (l max r))

  def map[A](t: Tree[A])(f: A => A): Tree[A] = t match {
    case Leaf(n) => Leaf(f(n))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  def mapViaFold[A](t: Tree[A])(f: A => A): Tree[A] =
    fold(t)(n => Leaf(f(n)): Tree[A])((l, r) => Branch(l, r))

  def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = t match {
    case Leaf(n) => f(n)
    case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
  }

}

val t = Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))
Tree.map(t)(_ + 1)
Tree.size(t)
Tree.sizeViaFold(t)
Tree.maximum(t)
Tree.maximumViaFold(t)
Tree.depth(t)
Tree.depthViaFold(t)

