import Settings._
import indigo._

object Assets {
  val imageFiles  = Set[String] (
    "circle", "slider-end", "slider-mid", "slider-pointer", "walker", "wall")
  val buttonFiles = Set[String] ()
  val textFiles   = Set[String] ("plan")
  val jsonFiles   = Set[String] ()
  val audioFiles  = Set[String] ()

  def assets (baseUrl: String): Set[AssetType] =
    imageFiles.map   (file => AssetType.Image (AssetName(file), AssetPath(baseUrl + s"assets/$file.png"))) ++
      buttonFiles.map (file => AssetType.Image (AssetName(file), AssetPath(baseUrl + s"assets/$file.png"))) ++
      textFiles.map   (file => AssetType.Text  (AssetName(file), AssetPath(baseUrl + s"assets/$file.txt"))) ++
      audioFiles.map  (file => AssetType.Audio (AssetName(file), AssetPath(baseUrl + s"assets/$file.mp3"))) ++
      jsonFiles.map   (file => AssetType.Text  (AssetName(file), AssetPath(baseUrl + s"assets/$file.json")))

  val materials: Map[String, Material.Textured] =
    imageFiles.map (image => (image, Material.Textured (AssetName (image)))).toMap

  def graphic (asset: String, width: Int, height: Int): Graphic =
    Graphic (0, 0, width, height, 2, materials (asset))

  val planSpecs = AssetName ("plan")

  val circle = graphic ("circle", cellSize, cellSize).withRef (cellMiddle, cellMiddle)
  val tightCircle = circle.scaleBy (tightScale, tightScale)
  val looseCircle = circle.scaleBy (looseScale, looseScale)
  val walker1 = graphic ("walker", cellSize, cellSize).withRef (cellMiddle, cellMiddle)
  val walker2 = walker1.flipHorizontal (true)

  case class GraphicSheet (asset: String) {
    val sheet = Graphic (0, 0, cellSize, cellSize, 2, materials (asset))
    def instance (row: Int, column: Int): Graphic =
      sheet.withCrop (cellSize * column, cellSize * row, cellSize, cellSize)
  }

  val wallSheet = GraphicSheet ("wall")
  val midWall = wallSheet.instance (0, 0)
  val wallLeft = wallSheet.instance (0, 1)
  val wallDown = wallSheet.instance (0, 2)
  val wallRight = wallSheet.instance (0, 3)
  val wallUp = wallSheet.instance (0, 4)
  val wallBendBottomLeft = wallSheet.instance (1, 0)
  val wallBendBottomRight = wallSheet.instance (1, 1)
  val wallBendTopRight =  wallSheet.instance (1, 2)
  val wallBendTopLeft = wallSheet.instance (1, 3)

  val sliderLeft = graphic ("slider-end", 16, 16)
  val sliderRight = sliderLeft.flipHorizontal (true)
  val sliderPointer = graphic ("slider-pointer", 16, 16)

  def wallGraphic (angle: WallAngle): Graphic = angle match {
    case MidWall => midWall
    case LeftWall => wallLeft
    case RightWall => wallRight
    case UpWall => wallUp
    case DownWall => wallDown
    case BottomLeftDiagonal => wallBendBottomLeft
    case TopLeftDiagonal => wallBendTopLeft
    case BottomRightDiagonal => wallBendBottomRight
    case TopRightDiagonal => wallBendTopRight
  }
}
