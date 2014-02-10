import org.scalatest.{ShouldMatchers, FunSuite}

/**
 * @author Jan Paw 
 *         Date: 2/1/14
 */
class Test extends FunSuite with ShouldMatchers {
  val ω: Double = 1.5
  val ε: Double = 0.00001

  test("SOR") {
    val A: Seq[Double] = Seq(2.0, -1.0, 0.0, -1.0, 2.0, -1.0, 0.0, -1.0, 2.0)
    val b: Seq[Double] = Seq(1.0, 0.0, 1.0)
    val sor: SOR = new SOR(A, b)

    sor(ω, ε) foreach {
      v => v should equal(v +- ε)
    }
  }
}
