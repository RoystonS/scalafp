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
}