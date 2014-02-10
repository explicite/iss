import scala.swing._
import scala.swing.event.ButtonClicked
import scala.swing.Orientation.Vertical
import javax.swing.{BorderFactory, UIManager}
import scalax.chart.Charting._
import scalax.chart.XYChart

/**
 * @author Jan 
 *         Date: 2/9/14
 */
object App extends SwingApplication {
  val interRadius: TextField = 0.0
  val interRadiusLabel: Label = "inter radius [m]"

  val outerRadius: TextField = 0.08
  val outerRadiusLabel: Label = "outer radius [m]"

  val αAir: TextField = 300.0
  val αAirLabel: Label = "α air [W/m2*K]"

  val t: TextField = 100.0
  val tLabel: Label = "begin temperature [K]"

  val c: TextField = 700.0
  val cLabel: Label = "heat factor [J/kg*K]"

  val ρ: TextField = 7800.0
  val ρLabel: Label = "density [kg/m3]"

  val λ: TextField = 25.0
  val λLabel: Label = "conductivity [W/m*K]"

  val numberOfNodes: TextField = 50
  val numberOfNodesLabel: Label = "number of nodes"

  val materialParameters = new GridPanel(8, 2) {
    contents ++= interRadius :: interRadiusLabel ::
      outerRadius :: outerRadiusLabel ::
      αAir :: αAirLabel ::
      t :: tLabel ::
      c :: cLabel ::
      ρ :: ρLabel ::
      λ :: λLabel ::
      numberOfNodes :: numberOfNodesLabel :: Nil

    border = BorderFactory.createCompoundBorder(
      BorderFactory.createTitledBorder("Material parameters"),
      BorderFactory.createEmptyBorder(5, 5, 5, 5)
    )
  }

  val firstStopLabel: Label = "first stop"
  val firstStopTime: TextField = 1800.0
  val firstStopTemperature: TextField = 200.0

  val secondStopLabel: Label = "second stop"
  val secondStopTime: TextField = 3000.0
  val secondStopTemperature: TextField = 1000.0

  val stopsParameters = new GridPanel(2, 3) {
    contents ++= firstStopLabel :: firstStopTemperature :: firstStopTime ::
      secondStopLabel :: secondStopTemperature :: secondStopTime :: Nil

    border = BorderFactory.createCompoundBorder(
      BorderFactory.createTitledBorder("Heating stops (temperature/time)"),
      BorderFactory.createEmptyBorder(5, 5, 5, 5)
    )
  }

  val ε: TextField = 1.5
  val εLabel: Label = "ε"

  val ω: TextField = 0.00001
  val ωLabel: Label = "ω"

  val SORParameters = new GridPanel(2, 2) {
    contents ++= ε :: εLabel :: ω :: ωLabel :: Nil

    border = BorderFactory.createCompoundBorder(
      BorderFactory.createTitledBorder("SOR"),
      BorderFactory.createEmptyBorder(5, 5, 5, 5)
    )
  }

  val compute = new Button("compute")

  lazy val menu = new BoxPanel(Vertical) {
    contents ++= materialParameters :: stopsParameters :: SORParameters :: compute :: Nil
  }

  val panel = new FlowPanel() {
    contents ++= menu :: Nil
  }

  val scrollPane = new ScrollPane(panel)

  def top = new MainFrame {
    title = "Heating the pipe"
    contents = scrollPane

    listenTo(compute)

    reactions += {
      case ButtonClicked(`compute`) =>
        val mes = MES(interRadius,
          outerRadius,
          αAir,
          t,
          Seq((firstStopTemperature, firstStopTime), (secondStopTemperature, secondStopTime)),
          c,
          ρ,
          λ,
          numberOfNodes.toInt)

        val data = mes(ω, ε)

    }
  }

  def textField(d: Double) = new TextField {
    text = d.toString
    columns = 5
    horizontalAlignment = Alignment.Left
  }

  implicit def TextField2Double(f: TextField): Double = f.text.toDouble

  implicit def Double2TextField(d: Double): TextField = textField(d)

  implicit def String2Label(s: String): Label = new Label(s)

  override def startup(args: Array[String]): Unit = {
    UIManager.setLookAndFeel(
      UIManager.getSystemLookAndFeelClassName)
    top.visible = true
  }

  def resourceFromClassloader(path: String): java.net.URL =
    this.getClass.getResource(path)

  def resourceFromUserDirectory(path: String): java.io.File =
    new java.io.File(util.Properties.userDir, path)
}
