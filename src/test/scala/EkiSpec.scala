import org.scalatest._
import org.scalatest.FunSpec

class EkiSpec extends FunSpec with Matchers {
  describe("Eki") {
    it("ayase") {
      Eki.start("代々木上原", "綾瀬") should be (Some(Path("綾瀬", 2.5, List(
        "綾瀬", "北千住", "町屋", "西日暮里", "千駄木", "根津", "湯島",
        "新御茶ノ水", "大手町", "二重橋前", "日比谷", "霞ヶ関", "国会議事堂前",
        "赤坂", "乃木坂", "表参道", "明治神宮前", "代々木公園", "代々木上原"
      ))))
    }

    it("shibuya") {
      pending
      //Eki.start("綾瀬", "渋谷") should be (None)
    }
  }
}

class Tree extends FunSpec with Matchers {
  describe("assoc") {
    it("success") {
      STree.assoc("後楽園", List(("新大塚", 1.2), ("後楽園", 1.8))) should be (Some(1.8))
    }
    it("failure") {
      STree.assoc("池袋", List(("新大塚", 1.2), ("後楽園", 1.8))) should be (None)
    }
  }
  describe("insert") {
    it("") {
      val tree0 = STree.empty
      val dist0 = Distance("a", "b", "line", 1.8, 3)
      val tree1 = STree.insert(tree0, dist0)

      tree1 should be (
        Node(
          STree.empty,
          "a",List(("b",1.8)),
          Node(
            STree.empty,
            "b",List(("a",1.8)),
            STree.empty))
      )

      val dist1 = Distance("b", "c", "line", 2.1, 4)
      val tree2 = STree.insert(tree1, dist1)

      println(tree2)
      tree2 should be (
        Node(
          STree.empty,
          "a",List(("b",1.8)),
          Node(
            STree.empty,
            "b",List(("c",2.1), ("a",1.8)),
            Node(
              STree.empty,
              "c",List(("b",2.1)),
              STree.empty)))
      )
    }
  }
}
