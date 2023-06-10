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


// First Business Logic
// Example 1
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

// Example 2
case class Card(shape: Shape, number: Number, color: Color, shading: Shading)

enum Shape:
    case Diamond, Squiggle, Oval

enum Color:
    case Red, Green, Purple

enum Shading:
    case Open, Striped, Solid

enum Number:
    case One, Two, Three

val deck = List(
    Card(Shape.Diamond, Number.One, Color.Purple, Shading.Striped),
    Card(Shape.Squiggle, Number.Two, Color.Red, Shading.Open),
    Card(Shape.Oval, Number.Three, Color.Green, Shading.Solid)
)

def isValidSet(card1: Card, card2: Card, card3: Card): Boolean =
    checkShapeProperty(card1, card2, card3) &&
    checkNumberProperty(card1, card2, card3) &&
    checkColorProperty(card1, card2, card3) &&
    checkShadingProperty(card1, card2, card3)

def checkShapeProperty(card1: Card, card2: Card, card3: Card): Boolean = 
    def allSame = 
        card1.shape == card2.shape && card1.shape == card3.shape
    def allDifferent =
        card1.shape != card2.shape &&
        card1.shape != card3.shape &&
        card2.shape != card3.shape
    allSame || allDifferent


def checkNumberProperty(card1: Card, card2: Card, card3: Card): Boolean = 
    def allSame = 
        card1.number == card2.number&& card1.number == card3.number
    def allDifferent =
        card1.number != card2.number &&
        card1.number != card3.number &&
        card2.number != card3.number
    allSame || allDifferent

def checkShadingProperty(card1: Card, card2: Card, card3: Card): Boolean = 
    def allSame = 
        card1.shading == card2.shading && card1.shading == card3.shading
    def allDifferent =
        card1.shading != card2.shading &&
        card1.shading != card3.shading &&
        card2.shading != card3.shading
    allSame || allDifferent

def checkColorProperty(card1: Card, card2: Card, card3: Card): Boolean = 
    def allSame = 
        card1.color == card2.color && card1.color == card3.color
    def allDifferent =
        card1.color != card2.color &&
        card1.color != card3.color &&
        card2.color != card3.color
    allSame || allDifferent

isValidSet(
    Card(Shape.Diamond, Number.One, Color.Purple, Shading.Striped),
    Card(Shape.Squiggle, Number.Two, Color.Green, Shading.Solid),
    Card(Shape.Oval, Number.Three, Color.Red, Shading.Open)
)


