import scala.collection.mutable.ArrayBuffer
import BLAS._
import scala.collection.parallel.ForkJoinTaskSupport

/**
 * Jacobi equation system solver
 *
 * @param A matrix
 * @param b vector
 *
 * @author Jan Paw
 *         Date: 3/2/14
 */
case class Jacobi(A: Seq[Double], b: Seq[Double]) {
  var x1: ArrayBuffer[Double] = ArrayBuffer.fill(b.length)(0.0)
  var x2: ArrayBuffer[Double] = ArrayBuffer.fill(b.length)(0.0)
  val iterator = (0 until b.length).toList.par
  iterator.tasksupport = new ForkJoinTaskSupport(new scala.concurrent.forkjoin.ForkJoinPool(Runtime.getRuntime().availableProcessors()))

  /**
   * Resolve linear system
   *
   * @param ε accuracy rate
   *
   * @return solution vector
   */
  def apply(ε: Double): ArrayBuffer[Double] = {

    def convergence: Boolean = norm(subtract(x2, x1)) <= ε

    repeat {
      x1 = x2.clone()

      iterator.foreach({
        i =>
          var sum: Double = 0.0

          for (j <- 0 until b.length)
            if (i != j)
              sum += A(i + j * b.length) * x1(j)

          x2(i) = (b(i) - sum) / A(i + i * b.length)
      })

    } until convergence

    x2
  }
}
