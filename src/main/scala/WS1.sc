object WS1 {
  import scala.annotation.tailrec

  def partial1[A, B, C](a: A, f: (A, B) => C): B => C =
    (b) => f(a, b)                                //> partial1: [A, B, C](a: A, f: (A, B) => C)B => C

  def curry[A, B, C](f: (A, B) => C): A => (B => C) =
    a => b => f(a, b)                             //> curry: [A, B, C](f: (A, B) => C)A => (B => C)

  def uncurry[A, B, C](f: A => B => C): (A, B) => C =
    (a, b) => f(a)(b)                             //> uncurry: [A, B, C](f: A => (B => C))(A, B) => C

  def compose[A, B, C](f: B => C, g: A => B): A => C =
    x => f(g(x))                                  //> compose: [A, B, C](f: B => C, g: A => B)A => C

  def dropWhile[A](l: List[A], pred: A => Boolean): List[A] = {
    l match {
      case x :: xs if (pred(x)) => dropWhile(l, pred)
      case _ => l
    }
  }                                               //> dropWhile: [A](l: List[A], pred: A => Boolean)List[A]

  val xs = List(1, 2, 3, 4, 5, 6)                 //> xs  : List[Int] = List(1, 2, 3, 4, 5, 6)\

  val q = dropWhile(xs, (x: Int) => x < 4)

  def append[A](a1: List[A], a2: List[A]): List[A] = {
    a1 match {
      case Nil => a2
      case x :: xs => x :: append(xs, a2)
    }
  }

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case x :: xs => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1
    case 0.0 :: _ => 0
    case x :: xs => x * product(xs)
  }

  def fold[T, U](l: List[T])(init: U)(f: (U, T) => U): U = {
    def go(acc: U, l: List[T]): U =
      l match {
        case Nil => acc
        case x :: xs => go(f(acc, x), xs)
      }

    go(init, l)
  }
}