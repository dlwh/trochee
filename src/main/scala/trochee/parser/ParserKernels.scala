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
abstract class ParserKernels extends InsideKernels with KernelOps with RangeOps { this: Base =>


}

object NullGrammar extends ParserKernels with InliningInsideKernels with ParserCommonExp with KernelOpsExp with RangeOpsExp with IfThenElseExp with App with SpireOpsExp { self =>
  val codegen = new OpenCLKernelCodegen with OpenCLKernelGenArrayOps with OpenCLParserGen {
    val IR: self.type = self
  }
  println(codegen.mkKernel(insideUnaries))
  protected def doLeftTermUpdates(out: Rep[Array[Real]], grammar: Rep[Int], leftTerm: Rep[TermCell], right: Rep[ParseCell], rules: Rep[RuleCell]): Rep[Unit] = unit()

  protected def doBothTermUpdates(out: Rep[Array[Real]], grammar: Rep[Int], leftTerm: Rep[TermCell], rightTerm: Rep[TermCell], rules: Rep[RuleCell]): Rep[Unit] = unit()

  protected def doRightTermUpdates(out: Rep[Array[Real]], grammar: Rep[Int], left: Rep[ParseCell], rightTerm: Rep[TermCell], rules: Rep[RuleCell]): Rep[Unit] = unit()

  protected def doNTRuleUpdates(out: Rep[Array[Real]], left: Rep[ParseCell], right: Rep[ParseCell], grammar: Rep[Int], rulePartition: IndexedSeq[(Rule[Int], Int)], rules: Rep[RuleCell]): Rep[Unit] = unit()

  def writeOut(out: Rep[ParseCell], in: Rep[Array[Real]]): Rep[Unit] = unit()

  type Real = Float

  def manifestReal: Manifest[Real] = manifest[Float]

  def rigRepReal: Numeric[NullGrammar.Real] = implicitly


  val grammar = Grammar.parseFile(new java.io.File("src/main/resources/trochee/parser/demo.grammar.txt"))
}

trait OpenCLParserGen extends OpenCLKernelCodegen with GenSpireOps {
  val IR: Expressions with Effects with FatExpressions with trochee.kernels.KernelOpsExp with ParserCommonExp with IfThenElseExp with SpireOpsExp
  import IR._
  lazy val typeMaps = Map[Class[_],String](manifestParseCell.erasure -> "parse_cell" ,
    manifestTermCell.erasure -> "term_cell",
    manifestRuleCell.erasure -> "rule_cell*")
  override def remap[A](m: Manifest[A]) : String = {
    typeMaps.getOrElse(m.erasure, super.remap(m))
  }

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) {
    rhs match {
      case Mad(a,b,c) =>
        cacheAndEmit(sym, s"mad(${quote(a)}, ${quote(b)}, ${quote(c)})")
      case _ => super.emitNode(sym, rhs)
    }

  }
}
