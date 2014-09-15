sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

def size[A](tree: Tree[A]): Int =
  tree match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + size(l) + size(r)
  }

def maximum(tree: Tree[Int]): Int =
  tree match {
    case Leaf(x) => x
    case Branch(l, r) => maximum(l) max maximum(r)
  }

def depth[A](tree: Tree[A]): Int =
  tree match {
    case Leaf(_) => 1
    case Branch(l, r) => depth(l) max depth(r)
  }

def map[A, B](tree: Tree[A])(transform: (A => B)): Tree[B] =
  tree match {
    case Leaf(x) => Leaf(transform(x))
    case Branch(l, r) => Branch(map(l)(transform), map(r)(transform))
  }

