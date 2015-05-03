sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A] (head: A, tail: List[A]) extends List[A]

object List {
        def sum(ints: List[Int]): Int = ints match {
        case Nil =>  0
        case Cons(x, xs) => x +sum(xs)
        }

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
        }
}

