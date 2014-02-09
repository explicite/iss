import scala.collection.mutable.ArrayBuffer
import scala.swing._
import javax.swing.UIManager


/**
 * @author Jan 
 *         Date: 2/9/14
 */
object App extends SwingApplication {
  val interRadius: TextField = 0.0
  val outerRadius: TextField = 0.0
  val αAir: TextField = 0.0
  val t: TextField = 0.0
  val columnName = Seq("temperature", "time")
  val rowData = Array(Array("0.0", "0.0"))

  val data = ArrayBuffer(
    Array("Time [s]", "Temp [K]"),
    Array("0.0", "0.0")
  )

  val table: Table = new Table(data.tail.toArray map (_.toArray[Any]), data.head)

  val panel = new FlowPanel() {
    contents ++= table :: Nil
  }

  val scrollPane = new ScrollPane(panel)

  def top = new MainFrame {
    title = "Heating the pipe"
    contents = scrollPane
  }

  override def startup(args: Array[String]): Unit = {
    UIManager.setLookAndFeel(
      UIManager.getSystemLookAndFeelClassName)
    top.visible = true
  }

  def textField(d: Double) = new TextField {
    text = d.toString
    columns = 5
    horizontalAlignment = Alignment.Left
  }

  implicit def TextField2Double(f: TextField): Double = f.text.toDouble

  implicit def Double2TextField(d: Double): TextField = textField(d)
}
