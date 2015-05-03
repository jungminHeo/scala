sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A] (head: A, tail: List[A]) extends List[A]

object List {
        def sum(ints: List[Int]): Int = ints match {
        case Nil =>  0
        case Cons(x, xs) => x +sum(xs)
        }

	def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B ): B = as match {
		case Nil => z
		case Cons(x, xs) => f(x, foldRight(xs, z)(f))
	}

	def length[A](as: List[A]): Int = as match {
		case Nil => 0
		case Cons(x,xs) => 1 + length(xs) 
	}

	def length2[A](as: List[A]): Int = 
		foldRight(as, 0)((x,y) => y+1)

	def sum2(ns: List[Int]) =
		foldRight(ns, 0)((x,y) => x + y)

	def product2(ns: List[Double]) =
		foldRight(ns, 1.0)(_*_)

	def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = as match {
		case Nil => z
		case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
	}

	def sum3(ints: List[Int]): Int = 
		foldLeft(ints, 0)(_+_)

        def product(ds: List[Double]): Double = ds match {
                case Nil => 1.0
                case Cons(0.0, _) => 0.0
                case Cons(x, xs) => x * product(xs)
        }

        def tail(ints: List[Int]): List[Int] = ints match {
                case Nil => Nil
                case Cons(x, y) => y
        }

        def setHead(ints: List[Int], subHead: Int): List[Int] = ints match {
                case Nil => Nil
                case Cons(x, y) => Cons(subHead, y)
        }

        def drop[A](l: List[A], n: Int): List[A] = l match {
                case Nil => Nil
                case Cons(x, y) if (n > 0) => drop(y, n-1)
                case _ => l
        }

        def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
                case Nil => Nil
                case Cons(x, y) if f(x) => dropWhile(y, f)
                case _ => l

        }

        def init[A](l: List[A]): List[A] = l match {
                case Nil => Nil
                case Cons(_, Nil) => Nil        
                case Cons(x,y) => Cons(x, init(y))
        }

        def apply[A](as: A*): List[A] = 
                if (as.isEmpty) Nil
                else Cons (as.head, apply(as.tail: _*))

}

object TestMain {
        def main(args: Array[String]): Unit = {

                // 3.1
                val x = Cons(1, Cons(2, Cons(3, Cons(4, Cons(5, Nil))))) match {
                        case Cons(x, Cons(2, Cons(4, _))) => x
                        case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
                }

                println(x)     

                val y = Cons(1, Cons(2, Cons(3, Cons(4, Cons(5, Nil)))))
                val z = List.tail(y)

                val xs: List[Int] = List(1,2,3,4,5)

                // 3.2
                println(z)

                // 3.3
                println(List.setHead(y, 10))

                // 3.4
                println(List.drop(y, 6))

                // 3.5
                println(List.dropWhile(xs, (x: Int) => x <= 5 ))

                // 3.6
                println(List.init(y))

		//3.8
		println(List.foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_)))

		//3.9
		println(List.length(xs))
		println(List.length2(xs))

		//3.11
		println(List.sum3(xs))
        }
}

