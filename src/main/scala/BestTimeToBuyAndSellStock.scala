import scala.annotation.tailrec

class BestTimeToBuyAndSellStock {
  def maxProfit(StockPricesArray: Array[Int]): Int = {
    @tailrec
    def maxProfitHelper(StockPricesArray: List[Int], maxProfit: Int, prevBought: Int): Int = {
      StockPricesArray match {
        case Nil => maxProfit
        case head :: tail => if (head > prevBought) {
          maxProfitHelper(tail, Math.max(maxProfit, head - prevBought), prevBought)
        } else {
          maxProfitHelper(tail, maxProfit, head)
        }
      }
    }

    val priceList = StockPricesArray.toList
    maxProfitHelper(priceList, 0, priceList.head)
  }
}