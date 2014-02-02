import org.scalatest.FunSuite

/**
 * @author Jan Paw 
 *         Date: 2/1/14
 */
class Test extends FunSuite {
  test("SOR") {
    val A: Seq[Double] = List.fill(4)(1.0)
    val B: Seq[Double] = List.fill(2)(1.0)

    val sor = SOR(A, B)
    sor(1.1) foreach {a => println(a)}
  }
}
