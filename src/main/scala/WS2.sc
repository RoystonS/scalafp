import scala.annotation.tailrec

@tailrec
def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = {
  l match {
    case Nil => z
    case x :: xs => foldLeft(xs, f(z, x))(f)
  }
}

def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
  as match {
    case Nil => z
    case x :: xs => f(x, foldRight(xs, z)(f))
  }
}

def reverse1[A](l:List[A]) = foldRight(l, Nil: List[A])((h, acc) => h :: acc)
def reverse2[A](l: List[A]): List[A] = foldLeft(l, List[A]())((acc, h) => h :: acc)

def appendViaFold[A](l1: List[A], l2: List[A]): List[A] =
  foldRight(l1, l2)((h, xs) => h :: xs)


def concatenate[A](l: List[List[A]]): List[A] = {
  foldRight(l, List[A]())(appendViaFold)
}

def map[A, B](l: List[A])(transform: (A => B)): List[B] =
  foldRight(l, Nil:List[B])((h, t) => transform(h) :: t)

def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = {
  concatenate(map(as)(f))
}

def filter[A](l: List[A], predicate: (A => Boolean)): List[A] =
  foldRight(l, Nil: List[A])((h, t) => if (predicate(h)) h :: t else t)

def filterViaFlatMap[A](l: List[A], predicate: (A => Boolean)): List[A] =
  flatMap(l)(x => if (predicate(x)) List(x) else Nil)

def addPairwise(as: List[Int], bs: List[Int]): List[Int] =
  (as, bs) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (a::as, b::bs) => {
      (a+b) :: addPairwise(as, bs)
    }
  }

val x = addPairwise(List(1,2,3), List(3,4,5))

