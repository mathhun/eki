case class Station(name: String, nameKana: String, nameKanji: String, line: String)
case class Distance(from: String, to: String, line: String, distance: Double, time: Double)
case class Line(name: String, nameKanji: String)
case class Path(name: String, minDistance: Double, path: List[String])

abstract class MyTree[A <% Ordered[A], B] {
  def insert(label: A, data: B): MyTree[A, B]
  def isEmpty: Boolean
}
case class Node[A <% Ordered[A], B](left: MyTree[A, B], label: A, values: List[B], right: MyTree[A, B]) extends MyTree[A, B] {
  def insert(lab: A, data: B): MyTree[A, B] =
    if (lab < label) {
      val l = if (left.isEmpty) MyTree.single(lab, data) else left.insert(lab, data)
      Node(l, label, values, right)
    } else if (label < lab) {
      val r = if (right.isEmpty) MyTree.single(lab, data) else right.insert(lab, data)
      Node(left, label, values, r)
    } else {
      Node(left, label, data :: values, right)
    }

  def isEmpty = false
}
case class Empty[A <% Ordered[A], B]() extends MyTree[A, B] {
  def insert(label: A, data: B): MyTree[A, B] = MyTree.single(label, data)
  def isEmpty = true
}
object MyTree {
  def single[A <% Ordered[A], B](label: A, data: B) = Node(Empty[A, B](), label, List(data), Empty[A, B]())
  def insert[A, B](tree: MyTree[A, B], label: A, data: B): MyTree[A, B] = tree.insert(label, data)
}

object STree {
  type ToDistance = (String, Double)
  type FromToDistance = (String, ToDistance)

  def empty: MyTree[String, ToDistance] = Empty[String, ToDistance]()

  def assoc(name: String, distances: List[ToDistance]): Option[Double] =
    distances find (_._1 == name) map (_._2)

  def fromDistance(d: Distance): (FromToDistance, FromToDistance) =
    (
      (d.from, (d.to, d.distance)),
      (d.to, (d.from, d.distance))
    )

  def insert(tree: MyTree[String, ToDistance], distance: Distance): MyTree[String, ToDistance] = {
    val ((l0, d0), (l1, d1)) = fromDistance(distance)
    tree.insert(l0, d0).insert(l1, d1)
  }
}

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

    loop(Nil, paths)
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
