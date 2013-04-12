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

  type Real = Float

  def manifestReal: Manifest[Real] = manifest[Float]

  def rigRepReal: Numeric[NullGrammar.Real] = implicitly


  val grammar = Grammar.parseFile(new java.io.File("src/main/resources/trochee/parser/demo.grammar.txt"))

  println(codegen.mkKernel(insideUnaries))
}

trait OpenCLParserGen extends OpenCLKernelCodegen with GenSpireOps {
  val IR: Expressions with Effects with FatExpressions with trochee.kernels.KernelOpsExp with ParserCommonExp with IfThenElseExp with SpireOpsExp
  import IR._
  lazy val typeMaps = Map[Class[_],String](manifestParseChart.erasure -> "PARSE_CELL" ,
//    manifestTermCell.erasure -> "term_cell",
    manifestRuleCell.erasure -> "rule_cell*")
  override def remap[A](m: Manifest[A]) : String = {
    typeMaps.getOrElse(m.erasure, super.remap(m))
  }

  override def quote(x: Exp[Any]) = x match {
    case RuleDeref(cell, rule, grammar) => preferNoLocal(cell) + s"->rules[${quote(rule)}][${quote(grammar)}]"
    case _ => super.quote(x)
  }

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) {
    rhs match {
      case Mad(a,b,c) =>
        cacheAndEmit(sym, s"mad(${quote(a)}, ${quote(b)}, ${quote(c)})")
      case app@CellApply(NTCell(cell, off, begin, end, gram), symsym) =>
        cacheAndEmit(addPos(sym, app), s"${quote(cell)}[(${quote(symsym)} * CHART_SIZE + ${quote(off)} + TRIANGULAR_INDEX(${quote(begin)}, ${quote(end)}))*NUM_GRAMMARS + ${quote(gram)}]")
      case MadUpdate(acc, index, a, b) =>
        val local = acc.local(index)
        if(!acc.declared(index)) {
          acc.declare(index)
          cacheAndEmit(sym, s"float $local = mad($local, ${quote(a)}, ${quote(b)})")
        } else {
          cacheAndEmit(sym, s"$local = mad($local, ${quote(a)}, ${quote(b)})")
        }

      case WriteOutput(NTCell(cell, off, begin, end, gram), acc) =>
        cacheAndEmit(sym, s"XXX")
      case _ => super.emitNode(sym, rhs)
    }

  }
}
