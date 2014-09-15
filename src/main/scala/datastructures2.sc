object datastructures2 {
	sealed trait Tree[+A]
	case class Leaf[A](value: A) extends Tree[A]
	case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
	
	def fold[A, B](tree: Tree[A])(baseTransform: A => B)(branchTransform: (B, B) => B): B = {
	  def go(tree: Tree[A]): B =
	    tree match {
	      case Leaf(x) => baseTransform(x)
	      case Branch(l, r) => branchTransform(go(l), go(r))
	    }
	
	  go(tree)
	}

	def size[A](tree: Tree[A]) =
	  fold(tree)(_ => 1)(_ + _ + 1)
	
	def maximum(tree: Tree[Int]) =
	  fold(tree)(x => x)(_ max _)
	
	def depth[A](tree: Tree[A]) =
	  fold(tree)(_ => 0)((l, r) => (l max r) + 1)

	def map[A, B](tree: Tree[A])(transform: (A => B)): Tree[B] =
	  fold(tree)(x => Leaf(transform(x)): Tree[B])(Branch(_, _))
}