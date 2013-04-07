package trochee.parser

import trochee.kernels.{KernelOps, KernelOpsExp}
import virtualization.lms.common._
import reflect.SourceContext
import trochee.codegen._
import virtualization.lms.internal.{Expressions, FatExpressions, Effects}
import virtualization.lms.common.RangeOpsExp
import virtualization.lms.common.RangeOps
import virtualization.lms.common.Base
import trochee.basic.{SpireOpsExp, GenSpireOps}

/**
 * 
 * @author dlwh
 */
abstract class ParserKernels extends InsideKernels with KernelOps with RangeOps { this: Base with KernelOps with RangeOps =>


}

object NullGrammar extends ParserKernels with InliningInsideKernels with ParserCommonExp with KernelOpsExp with RangeOpsExp with IfThenElseExp with App with SpireOpsExp { self =>
  val codegen = new OpenCLKernelCodegen with OpenCLKernelGenArrayOps with OpenCLParserGen {
    val IR: self.type = self
  }
  println(codegen.mkKernel(insideUnaries))
  protected def doLeftTermUpdates(out: Rep[Array[Real]], grammar: Rep[Int], leftTerm: Rep[TermCell], right: Rep[ParseCell], rules: Rep[Array[RuleCell]]): Rep[Unit] = unit()

  protected def doBothTermUpdates(out: Rep[Array[Real]], grammar: Rep[Int], leftTerm: Rep[TermCell], rightTerm: Rep[TermCell], rules: Rep[Array[RuleCell]]): Rep[Unit] = unit()

  protected def doRightTermUpdates(out: Rep[Array[Real]], grammar: Rep[Int], left: Rep[ParseCell], rightTerm: Rep[TermCell], rules: Rep[Array[RuleCell]]): Rep[Unit] = unit()

  protected def doNTRuleUpdates(out: Rep[Array[Real]], left: Rep[ParseCell], right: Rep[ParseCell], grammar: Rep[Int], rulePartition: IndexedSeq[(Rule[Int], Int)], rules: Rep[Array[RuleCell]]): Rep[Unit] = unit()

  def writeOut(out: Rep[ParseCell], in: Rep[Array[Real]]): Rep[Unit] = unit()

  type Real = Float

  def manifestReal: Manifest[Real] = manifest[Float]

  def rigRepReal: Numeric[NullGrammar.Real] = implicitly


  def grammar: Grammar {type Real = self.Real} = new Grammar {
    type Real = self.Real

    def numSyms: Int = 5

    def ruleScores: Array[Real] = Array(1.0f,2.0f,3.0f)

    def leftTermRules: IndexedSeq[(BinaryRule[Int], Int)] = ???

    def rightTermRules: IndexedSeq[(BinaryRule[Int], Int)] = ???

    def nontermRules: IndexedSeq[(BinaryRule[Int], Int)] = ???

    def bothTermRules: IndexedSeq[(BinaryRule[Int], Int)] = ???

    def unaryRules: IndexedSeq[(UnaryRule[Int], Int)] = Array(UnaryRule(0,0) -> 0, UnaryRule(0,1) -> 1, UnaryRule(1,0) -> 2)
  }
}

trait OpenCLParserGen extends OpenCLKernelCodegen with GenSpireOps {
  val IR: Expressions with Effects with FatExpressions with trochee.kernels.KernelOpsExp with ParserCommonExp with IfThenElseExp with SpireOpsExp
  import IR._
  lazy val typeMaps = Map[Class[_],String](manifestParseCell.erasure -> "parse_cell" ,
    manifestTermCell.erasure -> "term_cell",
    manifestRuleCell.erasure -> "rule_cell")
  override def remap[A](m: Manifest[A]) : String = {
    typeMaps.getOrElse(m.erasure, super.remap(m))
  }


}
