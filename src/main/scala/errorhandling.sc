object errorhandling {

  sealed trait Option[+A] {

    def flatMap[B](f: A => Option[B]): Option[B] = this match {
      case None => None
      case Some(a) => f(a)
    }

    def map[B](f: A => B): Option[B] = this flatMap (x => Some(f(x)))

    def getOrElse[B >: A](default: => B): B = this match {
      case None => default
      case Some(a) => a
    }

    def orElse[B >: A](default: => Option[B]): Option[B] = {
      this map (x => Some(x)) getOrElse default
    }

    def filter(f: A => Boolean): Option[A] =
      this flatMap (x => if (f(x)) Some(x) else None)
  }

  case class Some[+A](get: A) extends Option[A]

  case object None extends Option[Nothing]

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs) flatMap (m => mean(xs.map(x => math.pow(x - m, 2))))

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    for {
      va <- a
      vb <- b
    } yield f(va, vb)

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a match {
      case Nil => Some(Nil)
      case (a1 :: as) => {
        val z = map2(f(a1), traverse(as)(f))

        map2(f(a1), traverse(as)(f))(_ :: _)
      }
    }

  def sequence[A](a: List[Option[A]]): Option[List[A]] = traverse(a)(x => x)

  sealed trait Either[+E, +A] {
    def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] =
      this match {
        case Right(a) => f(a)
        case Left(e) => Left(e)
      }

    def map[B](f: A => B): Either[E, B] = this flatMap (a => Right(f(a)))

    def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] =
      this match {
        case Right(a) => Right(a)
        case Left(e) => b
      }

    def map2[EE >: E, B, C](b: => Either[EE, B])(f: (A, B) => C): Either[EE, C] =
      for {
        va <- this
        vb <- b
      } yield f(va, vb)
  }

  def traverseEithers[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    as match {
      case Nil => Right(Nil)
      case (a1 :: as) => {
        f(a1).map2(traverseEithers(as)(f))(_ :: _)
      }
    }

  def sequenceEithers[E, A](es: List[Either[E, A]]): Either[E, List[A]] = traverseEithers(es)(x => x)

  case class Left[+E](value: E) extends Either[E, Nothing]
  case class Right[+A](value: A) extends Either[Nothing, A]
}