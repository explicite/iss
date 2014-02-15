import java.awt.Color
import scala.collection.mutable.ArrayBuffer
import scala.swing.Component
import scala.math.floor

/**
 * @author Jan Paw 
 *         Date: 2/15/14
 */
class ConcentricCirclesCanvas extends Component {
  var circles = ArrayBuffer[Double]()
  var min = 0.0
  var max = 0.0

  override protected def paintComponent(g: _root_.scala.swing.Graphics2D) {
    g.clearRect(0, 0, size.width, size.height)

    for (circle <- 0 until circles.length) {
      g.setColor(circles(circle))
      val radius: Int = (size.width / 2 * (circle.toDouble / circles.length.toDouble)).toInt
      g.drawOval(size.width - radius, size.height - radius, radius * 2, radius * 2)
    }
  }

  def paint(c: Seq[Double]) {
    circles = c.asInstanceOf[ArrayBuffer[Double]]
    min = circles.min
    max = circles.max
    repaint()
  }

  implicit def Double2Color(d: Double): Color = {
    val s = 1.0
    val v = 1

    val h = (360.0 / (min - max)) * d / 60
    val i = floor(h)

    val f = h - i
    val p = (v * (1 - s) * 255).toInt
    val q = (v * (1 - s * f) * 255).toInt
    val t = (v * (1 - s * (1 - f)) * 255).toInt

    i match {
      case 0 => new Color(v, t, p)
      case 1 => new Color(q, v, p)
      case 2 => new Color(p, v, t)
      case 3 => new Color(p, q, v)
      case 4 => new Color(t, p, v)
      case _ => new Color(v, p, q)
    }
  }
}
