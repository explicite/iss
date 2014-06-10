import scala.annotation.tailrec
import scala.math._

/**
 * @author Jan Paw 
 *         Date: 3/2/14
 */
object BLAS {
  def norm(v: Seq[Double]): Double = sqrt(v.par.foldLeft(0.0)((acc, c) => acc + (c * c)))

  def subtract(v1: Seq[Double], v2: Seq[Double]): Seq[Double] = (v1, v2).zipped.map(_ - _)

  def repeat(body: => Unit) = new {
    @tailrec
    def until(condition: => Boolean) {
      body
      if (condition) () else until(condition)
    }
  }
}
