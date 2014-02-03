import scala.math.sqrt
import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

/**
 * Successive over-relaxation equation system solver
 *
 * @param A stiffness matrix
 * @param B loads vector
 *
 * @author Jan Paw
 *         Date: 2/1/14
 */
case class SOR(A: Seq[Double], B: Seq[Double]) {
  var x1: ArrayBuffer[Double] = ArrayBuffer.fill(B.length)(0.0)
  var x2: ArrayBuffer[Double] = ArrayBuffer.fill(B.length)(0.0)
  var x3: ArrayBuffer[Double] = ArrayBuffer.fill(B.length)(0.0)

  def apply(ω: Double): ArrayBuffer[Double] = {
    var strictLowerTriangular: Double = 0.0
    var strictUpperTriangular: Double = 0.0

    repeat {

      x3 = x1.clone()
      x1 = x2.clone()

      for (i <- 0 until B.length) {

        strictLowerTriangular = 0.0
        strictUpperTriangular = 0.0

        for (j <- 0 until i)
          strictLowerTriangular += (A(i + j * B.length) * x2(j))

        for (j <- i + 1 until B.length)
          strictUpperTriangular += (A(i + j * B.length) * x1(j))

        x2(i) = (ω / A(i + i * B.length)) * (B(i) - strictLowerTriangular - strictUpperTriangular) + ((1 - ω) * x1(i))
      }
    } until convergence

    x2
  }

  def convergence: Boolean = vectorNorm(substractVectors(x2, x1)) < vectorNorm(substractVectors(x1, x3))

  def vectorNorm(v: Seq[Double]): Double = sqrt(v.par.foldLeft(0.0)((acc, c) => acc + (c * c)))

  def substractVectors(v1: Seq[Double], v2: Seq[Double]): Seq[Double] = (v1, v2).zipped.map(_ - _)

  private def repeat(body: => Unit) = new {
    @tailrec
    def until(condition: => Boolean) {
      body
      if (condition) () else until(condition)
    }
  }
}

