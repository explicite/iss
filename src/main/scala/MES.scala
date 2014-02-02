import scala.collection.mutable.ArrayBuffer

/**
 * @param interRadius pipe inter radius [m]
 * @param outerRadius pipe outer radius [m]
 * @param α air convection heat transfer coefficient [W/m2*K]
 * @param t begin temperature [K]
 * @param stops temperature stops(stop temperature[K], stop time[s])
 * @param c heat factor [J/kg*K]
 * @param ρ material density [kg/m3]
 * @param λ thermal conductivity [W/m*K]
 * @param non number of nodes
 *
 * @author Jan Paw
 *         Date: 2/2/14
 */
case class MES(interRadius: Double, outerRadius: Double, α: Double, t: Double, stops: Seq[(Double, Double)], c: Double, ρ: Double, λ: Double, non: Int) {

  val NumberOfElements: Int = non - 1

  //integration points in the local coordinate system
  val E: Seq[Double] = Seq(-0.5773502692, 0.5773502692)

  //importance of integration
  val W: Seq[Double] = Seq(1.0, 1.0)

  //first shape function
  val N1: Seq[Double] = Seq(0.5 * (1.0 - E(0)), 0.5 * (1.0 - E(0)))

  //second shape function
  val N2: Seq[Double] = Seq(0.5 * (1.0 + E(0)), 0.5 * (1.0 + E(0)))

  val tEnd: Double = stops.foldLeft(0.0)((acc, c) => acc + c._2)

  val σRadius: Double = (outerRadius - interRadius) / (NumberOfElements - 1)

  val σTime: Double = tEnd / numberOfIterations

  val numberOfIterations: Int = ((tEnd / (σRadius * σRadius) / (0.5 * (λ / (c * ρ)))) + 1).toInt

  val NodeCoordinates: Seq[Int] = List.range(0, NumberOfElements)

  var NodeTemperature: ArrayBuffer[Double] = ArrayBuffer.fill(NumberOfElements)(t)

  //lower diagonal stiffness matrix
  var aC: ArrayBuffer[Double] = ArrayBuffer.fill(NumberOfElements)(0.0)

  //diagonal stiffness matrix
  var aD: ArrayBuffer[Double] = ArrayBuffer.fill(NumberOfElements)(0.0)

  //upper diagonal stiffness matrix
  var aE: ArrayBuffer[Double] = ArrayBuffer.fill(NumberOfElements)(0.0)

  //load vector
  var aB: ArrayBuffer[Double] = ArrayBuffer.fill(NumberOfElements)(0.0)
  
}
