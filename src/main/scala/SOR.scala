import scala.math.sqrt
import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

/**
 * Successive over-relaxation equation system solver
 *
 * @param A matrix
 * @param b vector
 *
 * @author Jan Paw
 *         Date: 2/1/14
 */
case class SOR(A: Seq[Double], b: Seq[Double]) {
  var x1: ArrayBuffer[Double] = ArrayBuffer.fill(b.length)(0.0)
  var x2: ArrayBuffer[Double] = ArrayBuffer.fill(b.length)(0.0)

  /**
   * Resolve linear system
   *
   * @param ω iteration factor [1,2] When ω, then become Gauss-Seidel method
   * @param ε accuracy rate
   *
   * @return solution vector
   */
  def apply(ω: Double, ε: Double): ArrayBuffer[Double] = {
    def convergence: Boolean = norm(subtract(x2, x1)) <= ε

    def norm(v: Seq[Double]): Double = sqrt(v.par.foldLeft(0.0)((acc, c) => acc + (c * c)))

    def subtract(v1: Seq[Double], v2: Seq[Double]): Seq[Double] = (v1, v2).zipped.map(_ - _)

    def repeat(body: => Unit) = new {
      @tailrec
      def until(condition: => Boolean) {
        body
        if (condition) () else until(condition)
      }
    }

    repeat {
      x1 = x2.clone()

      for (i <- 0 until b.length) {

        var strictLowerTriangular: Double = 0.0
        var strictUpperTriangular: Double = 0.0

        for (j <- 0 until i)
          strictLowerTriangular += (A(i + j * b.length) * x2(j))

        for (j <- i + 1 until b.length)
          strictUpperTriangular += (A(i + j * b.length) * x1(j))

        x2(i) = (ω / A(i + i * b.length)) * (b(i) - strictLowerTriangular - strictUpperTriangular) + ((1 - ω) * x1(i))
      }
    } until convergence

    x2
  }
}
