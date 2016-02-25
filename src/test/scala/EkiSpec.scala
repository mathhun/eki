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
