package reductions

import scala.annotation._
import org.scalameter._
import common._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime ms")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime ms")
    println(s"speedup: ${seqtime / fjtime}")
  }
}

object ParallelParenthesesBalancing {

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def balance(chars: Array[Char]): Boolean = {
    @tailrec
    def balanceChars(subChar: Array[Char], noOfOpenedP: Int, noOfClosedP: Int): Boolean = {
      //TODO double check this ... i had a bug that is supposed to be fixed bad test coverage from the previous assignment
      if (noOfOpenedP != noOfClosedP && subChar.isEmpty)
        false
      else if (noOfClosedP == noOfOpenedP && subChar.isEmpty)
        true
      else if (subChar.head.equals('('))
        balanceChars(subChar.tail, noOfOpenedP + 1, noOfClosedP)
      else if (subChar.head.equals(')') && noOfOpenedP <1)
        false
      else if(subChar.head.equals(')'))
        balanceChars(subChar.tail, noOfOpenedP, noOfClosedP + 1)
      else
        balanceChars(subChar.tail, noOfOpenedP, noOfClosedP)
    }
    balanceChars(chars,0,0)

  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    def traverse(idx: Int, until: Int, arg1: Int, arg2: Int) : (Int, Int) = {
      if(idx>= until) (arg1, arg2)
      else{
        chars(idx) match {
          case ')' => traverse(idx+1,until, arg1-1, arg2+1)
          case '(' => traverse(idx+1, until, arg1+1, arg2-1)
          case _ => traverse(idx+1, until, arg1, arg2)

        }
      }
    }

    def reduce(from: Int, until: Int) : (Int,Int)= {
      if (until - from <= threshold) traverse(from, until, 0, 0)
      else {
        val mid = (from + until) / 2
        val ((l1, r1), (l2,r2)) = parallel(reduce(from, mid), reduce(mid, until))

        (l1+l2,r1+r2)
      }
    }

    reduce(0, chars.length) == (0,0)
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
