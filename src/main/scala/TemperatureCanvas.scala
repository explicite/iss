import java.awt.{RenderingHints, Color}
import scala.collection.mutable.ArrayBuffer
import scala.swing.Component

/**
 * @author Jan Paw 
 *         Date: 2/15/14
 */
class TemperatureCanvas extends Component {
  var circles: ArrayBuffer[Double] = ArrayBuffer(0.0)

  override protected def paintComponent(g: _root_.scala.swing.Graphics2D) {
    g.clearRect(0, 0, size.width, size.height)

    val maxRadius = if (size.width > size.height) size.height else size.width

    for (h <- 0 until size.height) {
      g.setColor(Color.getHSBColor(h * 250f / size.height / 360f, 1f, 1f))
      g.drawLine(0, h, 20, h)
      if (h % (size.height / 20) == 0) {
        g.drawString(s"${(((size.height - h.toDouble) / size.height.toDouble * (circles.max - circles.min)) + circles.min).toInt}[°C]", 30, h + 10)
      }
    }

    g.setRenderingHint(
      RenderingHints.KEY_ANTIALIASING,
      RenderingHints.VALUE_ANTIALIAS_ON
    )

    for (circle <- circles.length - 1 to 0 by -1) {
      g.setColor(circle)
      val radius: Int = (maxRadius * (circle.toDouble / circles.length.toDouble)).toInt
      g.fillOval((size.width / 2) - radius, size.height - radius, radius * 2, radius * 2)
    }
  }

  def draw(c: Seq[Double]) {
    circles = c.asInstanceOf[ArrayBuffer[Double]]
    repaint()
  }

  implicit def Circle2Color(c: Int): Color = Color.getHSBColor(((circles.max - circles(c)) * 250f / (circles.max - circles.min) / 360f).toFloat, 1f, 1f)

}
