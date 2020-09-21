package computationalcognitivescience.fribble

import scalaz.Memo

object Levenshtein {
  def levenshtein[A](x: List[A], y: List[A]): Int = {
    lazy val m: ((Int, Int)) => Int = Memo.mutableHashMapMemo[(Int, Int), Int] {
      case (0, j) => j
      case (i, 0) => i
      case (i, j) =>
        if (x(i) == y(j)) {
          // same value is diagonal value
          m((i - 1, j - 1))
        } else {
          // diff value is min(left, diag, up) +1
          val left = m((i - 1, j)) + 1
          val diag = m((i - 1, j - 1)) + 1
          lazy val up = m((i, j - 1)) + 1
          if (left < diag) left
          else if (diag <= up) diag
          else up
        }
    }
    m(x.length - 1, y.length - 1)
  }


}