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