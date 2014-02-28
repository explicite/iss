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

  test("FEM") {
    val mes: FEM = FEM(0.0, 0.08, 300.0, 100.0, Seq((200, 1800), (1000, 3000)), 700, 7800, 12E-6, 25, 10)
    val data = mes(ω, ε)
    data(0) foreach {
      v => println(v)
    }
  }
}
