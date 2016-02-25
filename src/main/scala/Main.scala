case class Station(name: String, nameKana: String, nameKanji: String, line: String)
case class Distance(from: String, to: String, line: String, distance: Double, time: Double)
case class Line(name: String, nameKanji: String)
case class Path(name: String, minDistance: Double, path: List[String])

object Eki {
  def nameToKanjiName(name: String, stations: List[Station]): Option[String] =
    stations.find(_.name == name) map { _.nameKanji }

  def distanceBetween(from: String, to: String, distances: List[Distance]): Option[Double] = {
    { distances.find(_.from == from) filter { _.to == to } map { _.distance } }.orElse
    { distances.find(_.to == from) filter { _.from == to } map { _.distance } }
  }

  def makeStationList(stations: List[Station]): List[Path] =
    stations.map { s => Path(s.nameKanji, Double.MaxValue, Nil) }

  def initialize(name: String, paths: List[Path]): List[Path] =
    paths.map { n => if (n.name == name) Path(name, 0, List(name)) else n }

  def sort(stations: List[Station]): List[Station] =
    stations.sortBy(_.nameKana)

  def update(p: Path, ps: List[Path], distances: List[Distance]): List[Path] = {
    def update1(p1: Path, p2: Path): Path =
      (
        for {
          dis <- distanceBetween(p1.name, p2.name, distances)
          min = p2.minDistance
          if dis < min
        } yield Path(p2.name, dis, p2.name :: p1.path)
      ).getOrElse(p2)

    ps map { update1(p, _) }
  }

  def removeNearest(paths: List[Path], distances: List[Distance]): (Path, List[Path]) = {
    val p = paths.minBy(_.minDistance)
    (p, paths.filter { _ != p })
  }

  def dijkstra(paths: List[Path], distances: List[Distance]): List[Path] = {
    def loop(ps1: List[Path], ps2: List[Path]): List[Path] = {
      if (ps2.isEmpty) ps1
      else {
        val (p, ps) = removeNearest(ps2, distances)
        loop(p :: ps1, update(p, ps, distances))
      }
    }

    val (p, ps) = removeNearest(paths, distances)
    loop(List(p), update(p, ps, distances))
  }

  def start(s: String, e: String): Option[Path] = {
    import TokyoMetro._
    val paths = initialize(s, makeStationList(stations))
    val results = dijkstra(paths, distances)
    results.find { _.name == e }
  }
}

object Main {
  def main(args: Array[String]): Unit = {
  }
}
