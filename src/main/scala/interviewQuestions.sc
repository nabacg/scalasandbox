import java.io.File

import scala.io.Source

val a = 1.to(10).toList
val b = a.filter(i => i % 2 == 0).map(i => i*3)
val c = for (i <- a if i % 2 == 0) yield i * 3

val opts = List(None, Some(23), None, Some(233))
val optsV = opts.flatMap(o => o)
val optsVExp = opts.flatMap(o => o.toList)
val optsV2 = for (Some(i) <- opts) yield i
val optsV3 = for (i <- opts; v <- i) yield v



//def, val, var

val test1 : () => Int = {
  val r = util.Random.nextInt
  () => r
}

//same result as val will keep value of expr
test1()
test1()


def testDef : () => Int = {
  val r = util.Random.nextInt
  () => r
}
//different results as def will re-eval on each call
testDef()
testDef()

val xs = List(None, Some(12), Some(3), None, Some(23))
xs.flatMap(i => i)

class RichFile (val from : File ) {
  def read = Source.fromFile(from.getPath).mkString


}


object RichFile {
  implicit def file2RichFile(from : File) : RichFile = new RichFile(from)
}

import RichFile._
val f = new File("/Users/cab/Dev/Haskell/functorsSandbox.hs").read
case class Delimiter(left: String, right: String)
class OrderedDelim(val d: Delimiter) extends Ordered[Delimiter] {
  def compare(other :Delimiter) = if(d.left.length > d.left.length) 1 else -1
}
def quote(what: String)(implicit delims: Delimiter) = delims.left + what + delims.right
object ExclamationMarkDelims {
  implicit val delimiter = Delimiter("!! ", " !!")
}
quote("TEST")(Delimiter("<< ", " >>"))

import ExclamationMarkDelims.delimiter
quote("AA")
def smaller[T](a: T, b: T)(implicit order: T => Ordered[T]) =
  if (a < b) a else b
smaller(2,34)
smaller(34, -2)
implicit val dToOrder: Delimiter =>  Ordered[Delimiter] = d => new OrderedDelim(d)
smaller(Delimiter("!", "!"), Delimiter("!!", "!!"))

class Pair[T: Ordering](val first: T, val second : T) {
  def smaller(implicit ord: Ordering[T]) =
    if (ord.compare(first, second) < 0) first else second

  def smallerAlt =
    if(implicitly[Ordering[T]].compare(first, second) < 0) first else second

  def smallerAltTwo = {
    import Ordered._
    if(first < second) first else second
  }
}

new Pair(23, 2).smaller

