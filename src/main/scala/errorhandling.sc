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
    mean(xs) flatMap (m => mean(xs.map(x => math.pow(x -m, 2))))

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a flatMap (va => b map (vb => f(va, vb)))

  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    a match {
      case Nil => Some(Nil)
      case (a1 :: as) => a1 flatMap (q => (sequence(as) map (q :: _)))
    }
}