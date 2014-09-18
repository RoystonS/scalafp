object laziness {
  sealed trait Stream[+A] {
    def toList: List[A] =
      this match {
        case Empty => Nil
        case Cons(hd, tl) => hd() :: tl().toList
      }

    def take(n: Int): Stream[A] =
      this match {
        case Cons(hd, tl) if n == 1 => Stream.cons(hd(), Stream.empty)
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
      this.foldRight(Stream.empty[B])((x, acc) => Stream.cons(f(x), acc))

    def mapViaUnfold[B](f: A => B): Stream[B] =
      Stream.unfold(this) { case Cons(hd, tl) => Some(f(hd()), tl())}

    def takeViaUnfold(n: Int) = Stream.unfold((this, n)) {
      case (Cons(hd, tl), r) if r > 0 => Some(hd(), (tl(), r - 1))
      case _ => None
    }

    def takeWhileViaUnfold(p: A => Boolean) = Stream.unfold(this) {
      case (Cons(hd, tl)) if p(hd()) => Some(hd(), tl())
      case _ => None
    }

    def append[B >: A](as: => Stream[B]) =
      this.foldRight(as)((h, acc) => Stream.cons(h, acc))

    def filter(f: A => Boolean): Stream[A] =
      foldRight(Stream.empty[A])((x, acc) => {
        if (f(x)) Stream.cons(x, acc) else acc
      })

    def flatMap[B](f: A => Stream[B]): Stream[B] =
      this.foldRight(Stream.empty[B])((x, acc) => acc append f(x))

    def zipWith[B, C](bs: Stream[B])(f: (A, B) => C): Stream[C] = {
      def step(state: (Stream[A], Stream[B])): Option[(C, (Stream[A], Stream[B]))] = {
        state match {
          case (Empty, _) => None
          case (_, Empty) => None
          case (Cons(a, ax), Cons(b, bx)) => Some(f(a(), b()), (ax(), bx()))
        }
      }

      Stream.unfold(this, bs)(step)
    }

    def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] = {
      def step(state: (Stream[A], Stream[B])): Option[((Option[A], Option[B]), (Stream[A], Stream[B]))] = {

        state match {
          case (Empty, Empty) => None
          case (Empty, Cons(h, t)) => Some((None, Some(h())), (Empty, t()))
          case (Cons(h, t), Empty) => Some((Some(h()), None), (t(), Empty))
          case (Cons(a, ax), Cons(b, bx)) => Some((Some(a()), Some(b())), (ax(), bx()))
        }
      }

      Stream.unfold((this, s2))(step)
    }

    def startsWith[A](s2: Stream[A]): Boolean = {
      zipAll(s2).takeWhile(x => !x._2.isEmpty) forAll {
        case (h1, h2) => h1 == h2
      }
    }

    def tails: Stream[Stream[A]] =
      Stream.unfold(this) {
        case Empty => None
        case str => Some((str, str drop 1))
      } append Stream(Stream.empty)

    def scanRight[B](z: B)(f: (A,=>B) => B): Stream[B] =
      foldRight((z, Stream(z)))((a,p) => {
        val b2 = f(a,p._1)
        (b2, Stream.cons(b2,p._2))
      })._2


    def hasSubsequence[B >: A](s2: Stream[B]): Boolean =
      tails exists (_ startsWith s2)
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

    def unfold[A, State](initialState: State)(f: State => Option[(A, State)]): Stream[A] = {
      f(initialState) match {
        case Some((newValue, newState)) => cons(newValue, unfold(newState)(f))
        case None => empty
      }
    }

    def fibs(): Stream[Int] = unfold((0, 1)) { case (f0, f1) => Some((f0, (f1, f0 + f1))) }
    def from(n: Int): Stream[Int] = unfold(n) { x => Some((x, x + 1)) }
    def constant[A](a: A): Stream[A] = unfold(a) { x => Some(x, x) }
    def ones() = constant(1)
  }
}
