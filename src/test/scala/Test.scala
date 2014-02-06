import org.scalatest.FunSuite

/**
 * @author Jan Paw 
 *         Date: 2/1/14
 */
class Test extends FunSuite {
  test("MES") {
    val mes: MES = MES(0.0, 0.08, 300.0, 100.0, Seq((200, 1800), (1000, 3000)), 700, 7800, 25, 10)
    mes(1) foreach {
      a => println(a)
    }
  }
}
