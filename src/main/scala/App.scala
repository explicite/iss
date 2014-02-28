import javax.swing.{UIManager, BorderFactory}
import scala.collection.mutable
import scala.swing.event.{ValueChanged, ButtonClicked}
import scala.swing.Orientation.Vertical
import scalax.chart.Charting._
import scalax.chart.XYChart
import scala.swing._

/**
 * @author Jan Paw
 *         Date: 2/9/14
 */
object App extends SwingApplication {
  var data = mutable.Buffer[Seq[Double]]()

  val interRadius: TextField = 0.0
  val interRadiusLabel: Label = "minimal radius [m]"

  val outerRadius: TextField = 0.08
  val outerRadiusLabel: Label = "maximal radius [m]"

  val αAir: TextField = 300.0
  val αAirLabel: Label = "α air [W/m2*K]"

  val t: TextField = 100.0
  val tLabel: Label = "begin temperature [°C]"

  val c: TextField = 700.0
  val cLabel: Label = "heat factor [J/kg*K]"

  val ρ: TextField = 7800.0
  val ρLabel: Label = "density [kg/m3]"

  val β: TextField = 12E-6
  val βLabel: Label = "linear expansion [1/K]"

  val λ: TextField = 58.0
  val λLabel: Label = "conductivity [W/m*K]"

  val numberOfNodes: TextField = 50
  val numberOfNodesLabel: Label = "number of nodes"

  lazy val materialParameters = new GridPanel(9, 2) {
    contents ++= interRadius :: interRadiusLabel ::
      outerRadius :: outerRadiusLabel ::
      αAir :: αAirLabel ::
      t :: tLabel ::
      c :: cLabel ::
      ρ :: ρLabel ::
      β :: βLabel ::
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

  lazy val stopsParameters = new GridPanel(2, 3) {
    contents ++= firstStopLabel :: firstStopTemperature :: firstStopTime ::
      secondStopLabel :: secondStopTemperature :: secondStopTime :: Nil

    border = BorderFactory.createCompoundBorder(
      BorderFactory.createTitledBorder("Heating stops [°C]|[s]"),
      BorderFactory.createEmptyBorder(5, 5, 5, 5)
    )
  }

  val ε: TextField = new TextField("1.5")
  val εLabel: Label = "ε"

  val ω: TextField = new TextField("0.00001")
  val ωLabel: Label = "ω"

  lazy val SORParameters = new GridPanel(2, 2) {
    contents ++= ε :: εLabel :: ω :: ωLabel :: Nil

    border = BorderFactory.createCompoundBorder(
      BorderFactory.createTitledBorder("SOR"),
      BorderFactory.createEmptyBorder(5, 5, 5, 5)
    )
  }

  val timeLabel: Label = "0"

  lazy val compute = new Button("compute")

  lazy val simulation = new GridPanel(1, 2) {
    contents ++= timeLabel :: compute :: Nil

    border = BorderFactory.createCompoundBorder(
      BorderFactory.createTitledBorder("Simulation"),
      BorderFactory.createEmptyBorder(5, 5, 5, 5)
    )
  }

  lazy val slider = new Slider {
    enabled = false
    value = 0
  }

  lazy val menu = new BoxPanel(Vertical) {
    contents ++= materialParameters :: stopsParameters :: SORParameters :: simulation :: slider :: Nil
  }

  val chartData = Seq((0, 0)).toXYSeriesCollection("default")

  val chart: XYChart = XYLineChart(chartData, title = "temperature in points", domainAxisLabel = "time", rangeAxisLabel = "temperature")

  val canvas = new TemperatureCanvas {
    preferredSize = new Dimension(150, 250)
  }

  val visualization = new BoxPanel(Vertical) {
    contents ++= chart.toPanel :: canvas :: Nil
  }

  lazy val panel = new FlowPanel() {
    contents ++= menu :: visualization :: Nil
  }

  lazy val scrollPane = new ScrollPane(panel)

  def top = new MainFrame {
    title = "Heating transfer in cylinder"
    contents = scrollPane

    listenTo(compute)
    listenTo(slider)

    reactions += {
      case ButtonClicked(`compute`) =>
        val mes = FEM(interRadius,
          outerRadius,
          αAir,
          t,
          Seq((firstStopTemperature, firstStopTime), (secondStopTemperature, secondStopTime)),
          c,
          ρ,
          β,
          λ,
          numberOfNodes.toInt)

        data = mes(ε, ω)

        chartData.removeAllSeries()
        chartData.addSeries((for (i <- 0 until data.length) yield (i, data(i).head)).toXYSeries("inter"))
        chartData.addSeries((for (i <- 0 until data.length) yield (i, data(i)(data(i).length / 2))).toXYSeries("middle"))
        chartData.addSeries((for (i <- 0 until data.length) yield (i, data(i).last)).toXYSeries("outer"))

        canvas.draw(data(((slider.value / 100.0) * (data.length - 1)).toInt))
        slider.enabled = true

      case ValueChanged(`slider`) =>
        canvas.draw(data(((slider.value / 100.0) * (data.length - 1)).toInt))
        timeLabel.text = ((slider.value / 100.0) * (data.length - 1)).toInt.toString
    }
  }

  def textField(d: Double) = new TextField {
    text = d.toString
    columns = 5
    horizontalAlignment = Alignment.Left
  }

  override def startup(args: Array[String]) {
    UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName)
    top.visible = true
  }

  def resourceFromClassloader(path: String): java.net.URL =
    this.getClass.getResource(path)

  def resourceFromUserDirectory(path: String): java.io.File =
    new java.io.File(util.Properties.userDir, path)

  implicit def TextField2Double(f: TextField): Double = f.text.toDouble

  implicit def Double2TextField(d: Double): TextField = textField(d)

  implicit def String2Label(s: String): Label = new Label(s)

}
