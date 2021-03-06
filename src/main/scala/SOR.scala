import scala.collection.mutable.ArrayBuffer
import BLAS._

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

  def triangular(x: Seq[Double], i: Int): Double = {
    def iteration(j: Int, acc: Double): Double = {
      if (j < i)
        iteration(j + 1, acc + (A(i + j * b.length) * x(j)))
      else
        acc
    }

    iteration(0, 0d)
  }

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

    repeat {
      x1 = x2.clone()

      for (i <- 0 until b.length) {
        x2(i) = (ω / A(i + i * b.length)) * (b(i) - triangular(x2, i) - triangular(x1, i)) + ((1 - ω) * x1(i))
      }
    } until convergence

    x2
  }
}
