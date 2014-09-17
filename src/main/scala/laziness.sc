object laziness {
  sealed trait Stream[+A] {
    def toList: List[A] =
      this match {
        case Empty => Nil
        case Cons(hd, tl) => hd() :: (tl()).toList
      }

    def take(n: Int): Stream[A] =
      this match {
        case Cons(hd, tl) if (n == 1) => Stream.cons(hd(), Stream.empty)
        case Cons(hd, tl) => Stream.cons(hd(), tl().take(n - 1))
        case _ => Stream.empty
      }

    def drop(n: Int): Stream[A] = {
      def go(s: Stream[A], n: Int): Stream[A] =
        if (n <= 0) s
        else s match {
          case Cons(hd, tl) => go(tl(), n - 1)
          case _ => Stream.empty
        }

      go(this, n)
    }
  }

  case object Empty extends Stream[Nothing]
  case class Cons[+A](head: () => A, tail: () => Stream[A]) extends Stream[A]

  object Stream {
    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
      lazy val head = hd
      lazy val tail = tl

      Cons(() => head, () => tail)
    }

    def empty[A]: Stream[A] = Empty

    def apply[A](as: A*): Stream[A] =
      if (as.isEmpty) empty[A] else cons(as.head, apply(as.tail: _*))
  }

  Stream.cons(1, Stream.cons(2, throw new Exception("Foo"))).drop(1).take(1).toList
}
