val m1 = Map("a"-> 12,"b" -> 144, "answer to life" -> 42)

m1("a")
m1.get("answer to life")
//m1("c")

val s1 = Seq(1, 2, 3,5)
s1(2)
s1(1)

val arr1 = 1.to(100).filter(i => i % 5 == 0 && i % 3 == 0).toArray
arr1(3)

arr1.take(3)

1 until 5
6 to 1 by -2

val s = "Hello World"
s map (c => c.toUpper)

(1 to 10) flatMap (x => (20 to 25) map (y => (x,y)) )

def scalarProduct(xs: Vector[Double], ys: Vector[Double]) : Double =
//  ((xs zip ys) map {case (x, y) => x*y}).sum
  xs.zip(ys).map({case (x,y) => x*y}).sum

arr1.reduce((a, b) => a + b)

arr1.map(a => a+10)
arr1.map(_+1)

def isPrime(n : Int) : Boolean = (2 until n).forall(i => n % i != 0)

isPrime(1)
isPrime(2)
isPrime(4)



//arr1.foldLeft((a:Int, i:Int) => a + i)


def myMap[T, S](xs: List[T], f : T => S) : List[S] = xs match {
  case Nil => Nil
  case h :: tail => f(h) :: myMap(tail, f)
}

myMap(arr1.toList, (i : Int) => i + 4)

def myFlatMap[T,S](xs : List[T], f : T => List[S]) : List[S] = xs match {
  case Nil => Nil
  case h :: tail => f(h) ++ myFlatMap(tail, f)
}

myFlatMap(arr1.toList, (i : Int) => i.to(i+5).toList)

var sM = collection.mutable.Set[Int](1,2,3)
sM.add(23)
sM += 33
sM

val sIm = collection.immutable.Set[Int](1,2,3)
