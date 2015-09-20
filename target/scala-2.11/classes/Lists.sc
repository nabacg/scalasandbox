import java.util.NoSuchElementException

val l = List(List(true, false), List(3))
val l2 = List(1, 2, 3)

trait MyList[T] {
  def isEmpty : Boolean
  def head: T
  def tail : MyList[T]
}

class Cons[T](val head: T, val tail: MyList[T]) extends MyList[T] {
  def isEmpty = false
}

class Nil[T] extends MyList[T] {
  def isEmpty = true
  def head : Nothing = throw new NoSuchElementException("Nil.head")
  def tail : Nothing = throw new NoSuchElementException("Nil.tail")
}

def singleton[T](elem: T) = new Cons[T](elem, new Nil[T])

singleton[Int](1)
singleton(23)
singleton[Boolean](true)

def nth[T](n : Int, xs : MyList[T]) : T = {
  if (xs.isEmpty) throw new ArrayIndexOutOfBoundsException("Index out of bound")
  n match {
    case 0 => xs.head
    case _ => nth(n-1, xs.tail)
  }
/*
  def loop[T](i: Int, ys: MyList[T]): T =
    if (i == n) ys.head
    else if (ys.tail.isEmpty) throw new IndexOutOfBoundsException("out of bound")
    else loop(i + 1, ys.tail)
  loop(0, xs)
  */
}

val xs = 1.to(10).reverse.map(i => new Cons(i, new Nil())).reduce((l, i) => new Cons(i.head, l))

nth(3, xs)
nth(8, xs)
nth(12, xs)
