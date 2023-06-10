import scala.compiletime.ops.double
1 + 1

val x = 42

x * x

val y = 24

y * x

def house(facade: Double, window: Double): Double = 
    val door = 2*1
    val subtractedArea = door + window * 2
    facade - subtractedArea

house(10, 2)

def marathonDuration(speed: Double): Double =
    val distance = 42.195
    val duration = distance / speed
    duration * 60

marathonDuration(12)

def showPrice(paintingArea: Double, paintPrice: Double): String =
    val price = paintingArea * paintPrice
    if price > 100 then
        "This is too expensive"
    else
        price.toString

showPrice(15, 6)

// Case class
case class square(width: Int):
    val area = width * width

case class circle(radius: Int):
    val area = radius * radius * 3.14

// Sealed traits
// Match expression
// val subscribeEffectiveScala = 
//     Subscribe(Channel("effective-scala"))

case class Experience(duration: Int, definition: Double, network: Network)

enum Network:
    case Fixed, Mobile

val lowQuality = 0.3 // MB/s
val highQuality = 0.6 // MB/s

val thirtyMinutes = 30 * 60 // seconds

val highQualityAndMobile = 
    Experience(thirtyMinutes, highQuality, Network.Mobile)

val lowQualityAndFixed = 
    Experience(thirtyMinutes, lowQuality, Network.Fixed)

val dataCenterEnergy = 0.000072
val kgCO2PerKwh = 0.5

def networkEnergy(network: Network): Double = network match
    case Network.Fixed  => 0.00043
    case Network.Mobile => 0.00088 

def footprint(experience: Experience): Double =
    val megabytes = experience.duration * experience.definition
    val energy = dataCenterEnergy + networkEnergy(experience.network)
    energy * megabytes * 0.5

footprint(lowQualityAndFixed)
footprint(highQualityAndMobile)