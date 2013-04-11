package trochee.parser

import virtualization.lms.common.{RangeOps, Base}
import trochee.kernels.{Constant, Global, KernelOps}
import spire.algebra._
import spire.implicits._
import spire.syntax._
import spire.math._

/**
 * 
 * @author dlwh
 */
trait InsideKernels extends ParserCommon { self: Base with KernelOps with RangeOps =>

  def insideTermBinaries = kernel8("inside_term_binaries"){ (insideBots: Rep[Array[ParseCell] with Global],
                                                              insideTops: Rep[Array[ParseCell] with Global],
                                                              posTags: Rep[Array[TermCell] with Global],
                                                              offsets: Rep[Array[Int] with Global],
                                                              lengths: Rep[Array[Int] with Global],
                                                              lengthOffsets: Rep[Array[Int] with Global],
                                                              spanLength: Rep[Int],
                                                              rules: Rep[RuleCell with Global]) =>
    val sentence = globalId(0)
    val begin = globalId(1)
    val gram = globalId(2)
    val end = begin + spanLength
    val length = lengths(sentence)

    if (end <= length) {
      val out = NewArray[Real](numSyms)
      Arrays.fill(out, numSyms, zero)

      val sentOffset = offsets(sentence)
      val lengthOff = lengthOffsets(sentence)
      val left = CELL(insideTops, sentOffset, begin, end-1)
      val right = CELL(insideTops, sentOffset, begin+1, end)
      val leftTerm = posTags(lengthOff + begin)
      val rightTerm = posTags(lengthOff + (end-1))

      doLeftTermUpdates(out, gram,  leftTerm, right, rules)
      doRightTermUpdates(out, gram, left, rightTerm, rules)
      if(spanLength == 2)
        doBothTermUpdates(out, gram, leftTerm, rightTerm, rules)
      else unit()
      writeOut(CELL(insideBots, sentOffset, begin, end), out)

    } else unit() // needed because scala is silly
  }


  protected def doLeftTermUpdates(out: Rep[Array[Real]],
                                  grammar: Rep[Int],
                                  leftTerm: Rep[TermCell],
                                  right: Rep[ParseCell],
                                  rules: Rep[RuleCell]):Rep[Unit]


  protected def doBothTermUpdates(out: Rep[Array[Real]],
                                  grammar: Rep[Int],
                                  leftTerm: Rep[TermCell],
                                  rightTerm: Rep[TermCell],
                                  rules: Rep[RuleCell]):Rep[Unit]

  protected def doRightTermUpdates(out: Rep[Array[Real]],
                                  grammar: Rep[Int],
                                  left: Rep[ParseCell],
                                  rightTerm: Rep[TermCell],
                                  rules: Rep[RuleCell]):Rep[Unit]

  protected def doNTRuleUpdates(out: Rep[Array[Real]],
                                left: Rep[ParseCell],
                                right: Rep[ParseCell],
                                grammar: Rep[Int],
                                rulePartition: IndexedSeq[(Rule[Int], Int)],
                                rules: Rep[RuleCell]):Rep[Unit]


  def insideUnaries = kernel6("inside_unaries"){ (insideBots: Rep[Array[ParseCell] with Global],
                                                  insideTops: Rep[Array[ParseCell] with Global],
                                                  offsets: Rep[Array[Int] with Global],
                                                  lengths: Rep[Array[Int] with Global],
                                                  spanLength: Rep[Int],
                                                  rules: Rep[RuleCell with Global]) =>
    val sentence = globalId(0)
    val begin = globalId(1)
    val grammar = globalId(2)
    val end = begin + spanLength
    val length = lengths(sentence)

    if (end <= length) {
      val sentOffset = offsets(sentence)
      val top = CELL(insideTops, sentOffset, begin, end)
      val bot = CELL(insideBots, sentOffset, begin, end)
      doInsideUnaries(top, bot, grammar, rules)
    } else unit() // needed because scala is silly
  }

  def insideTermUnaries = kernel6("inside_unaries"){ (insidePos: Rep[Array[TermCell] with Global],
                                                      insideTops: Rep[Array[ParseCell] with Global],
                                                      offsets: Rep[Array[Int] with Global],
                                                      lengths: Rep[Array[Int] with Global],
                                                      lengthOffsets: Rep[Array[Int] with Global],
                                                      rules: Rep[RuleCell with Global]) =>
    val sentence = globalId(0)
    val begin = globalId(1)
    val grammar = globalId(2)
    val end = begin + 1
    val length = lengths(sentence)
    val lengthOff = lengthOffsets(sentence)

    if (end <= length) {
      val sentOffset = offsets(sentence)
      val top = CELL(insideTops, sentOffset, begin, end)
      val bot = insidePos(lengthOff + begin)
      doInsideTermUnaries(top, bot, grammar, rules)
    } else unit() // needed because scala is silly
  }

  protected def doInsideUnaries(top: Rep[ParseCell],
                                bot: Rep[ParseCell],
                                grammar: Rep[Int],
                                rules: Rep[RuleCell]):Rep[Unit]

  protected def doInsideTermUnaries(top: Rep[ParseCell],
                                    bot: Rep[TermCell],
                                    grammar: Rep[Int],
                                    rules: Rep[RuleCell]):Rep[Unit]


  def insideNonterms(partitionId: Int, rulePartition: IndexedSeq[(Rule[Int], Int)]) = {
    kernel6("inside_nonterms_"+partitionId){ (insideBots: Rep[Array[ParseCell] with Global],
                                              insideTops: Rep[Array[ParseCell] with Global],
                                              offsets: Rep[Array[Int] with Global],
                                              lengths: Rep[Array[Int] with Global],
                                              spanLength: Rep[Int],
                                              rules: Rep[RuleCell with Global]) =>
      val sentence = globalId(0)
      val begin = globalId(1)
      val gram = globalId(2)
      val end = begin + spanLength
      val length = lengths(sentence)

      if (end <= length) {
        val out = NewArray[Real](numSyms)
        Arrays.fill(out, numSyms, zero)

        val sentOffset = offsets(sentence)

        for(split <- (begin + unit(1)) until end) {
          val left = CELL(insideTops, sentOffset, begin, split)
          val right = CELL(insideTops, sentOffset, split, end)
          doNTRuleUpdates(out, left, right, gram, rulePartition, rules)

          unit()
        }

        writeOut(CELL(insideBots, sentOffset, begin, end), out)

      } else unit() // needed because scala is silly
    }
  }


  def writeOut(out: Rep[ParseCell], in: Rep[Array[Real]]):Rep[Unit]
}
