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

    def foldRight[B](initial: => B)(f: (A, => B) => B): B =
      this match {
        case Cons(h, t) => f(h(), t().foldRight(initial)(f))
        case _ => initial
      }

    def exists(p: A => Boolean): Boolean =
      this.foldRight(false)((a, x) => p(a) || x)

    def forAll(p: A => Boolean): Boolean =
      this.foldRight(true)((a, x) => p(a) && x)

    def takeWhile(p: A => Boolean): Stream[A] =
      this.foldRight(Stream.empty[A])((x, acc) => {
        if (p(x)) Stream.cons(x, acc) else Stream.empty
      })

    def headOption: Option[A] =
      this.foldRight(None: Option[A])((h, _) => Some(h))

    def map[B](f: A => B): Stream[B] =
      this.foldRight(Stream.empty[B])((x, acc) => {
        Stream.cons(f(x), acc)
      })

    def append[B >: A](as: => Stream[B]) =
      this.foldRight(as)((h, acc) => Stream.cons(h, acc))

    def filter(f: A => Boolean): Stream[A] =
      foldRight(Stream.empty[A])((x, acc) => {
        if (f(x)) Stream.cons(x, acc) else acc
      })

    def flatMap[B](f: A => Stream[B]): Stream[B] =
      this.foldRight(Stream.empty[B])((x, acc) => acc append f(x))
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
}
