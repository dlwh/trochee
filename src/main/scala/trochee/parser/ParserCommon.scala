package trochee.parser

import virtualization.lms.common._
import trochee.kernels.{KernelOpsExp, KernelOps}
import spire.algebra.Rig
import reflect.SourceContext
import virtualization.lms.common.RangeOpsExp
import virtualization.lms.common.RangeOps
import virtualization.lms.common.Base
import virtualization.lms.common.IfThenElseExp
import trochee.util.CStructExp
import trochee.basic.{SpireOps, RingOps}
import scala.virtualization.lms.util.OverloadHack
import scala.collection.mutable.ArrayBuffer
import scala.virtualization.lms.internal.Effects

/**
 * 
 * @author dlwh
 */
trait ParserCommon extends ExtraBase with SpireOps with OverloadHack { self: Base with KernelOps with RangeOps =>
  type Real
  implicit def manifestReal: Manifest[Real]
  implicit def rigRepReal: Numeric[Real]
  def zero: Rep[Real] = unit[Real](implicitly[Numeric[Real]].zero)
  def mad(a: Rep[Real], b: Rep[Real], c: Rep[Real]):Rep[Real]

  type ParseChart
  implicit def manifestParseChart: Manifest[ParseChart]
  def infix_update(chart: Rep[ParseChart], offset: Rep[Int], begin: Rep[Int], end: Rep[Int], gram: Rep[Int], acc: Accumulator): Rep[Unit]
  def infix_apply(chart: Rep[ParseChart], offset: Rep[Int], begin: Rep[Int], end: Rep[Int], gram: Rep[Int]):ParseCell

  implicit class RichParseChart(chart: Rep[ParseChart]) {
    def apply(offset: Rep[Int], begin: Rep[Int], end: Rep[Int], gram: Rep[Int]) = infix_apply(chart, offset, begin, end, gram)
    def update(offset: Rep[Int], begin: Rep[Int], end: Rep[Int], gram: Rep[Int], acc: Accumulator) = infix_update(chart, offset, begin, end, gram, acc)
  }

  type ParseCell
  def infix_apply(cell: ParseCell, sym: Rep[Int])(implicit pos: SourceContext):Rep[Real]

  implicit class RichParseCell(cell: ParseCell) {
    def apply(sym: Rep[Int])(implicit pos: SourceContext) = infix_apply(cell, sym)(pos)
  }



  type Accumulator
  def nontermAccumulator: Accumulator
  def infix_mad(acc: Accumulator, sym: Rep[Int], score1: Rep[Real], score2: Rep[Real]):Rep[Unit]


  /*
  typedef struct {
    float syms[NUM_SYMS][NUM_GRAMMARS];
  } parse_cell;
 */

  type TermChart
  implicit def manifestTermChart: Manifest[TermChart]
  def infix_update(chart: Rep[TermChart], offset: Rep[Int], pos: Rep[Int], gram: Rep[Int], acc: Accumulator): Rep[Unit]
  def infix_apply(chart: Rep[TermChart], offset: Rep[Int], pos: Rep[Int], gram: Rep[Int]):ParseCell

  implicit class RichTermChart(chart: Rep[TermChart]) {
    def apply(offset: Rep[Int], begin: Rep[Int],  gram: Rep[Int]) = infix_apply(chart, offset, begin, gram)
    def update(offset: Rep[Int], begin: Rep[Int], gram: Rep[Int], acc: Accumulator) = infix_update(chart, offset, begin, gram, acc)
  }
  

  type RuleCell
  implicit def manifestRuleCell: Manifest[RuleCell]
  def infix_rules(cell: Rep[RuleCell], r: Rep[Int], g: Rep[Int]):Rep[Real]

  def CELL[T:Manifest](arr: Rep[Array[T]], offset: Rep[Int], begin: Rep[Int], end: Rep[Int]): Rep[T] = {
    val triangularIndex = end * (end - 1) / 2 + begin
    arr(offset + triangularIndex)
  }

  def grammar: Grammar[String, Real]

  def numSyms: Int = grammar.numSyms
}

trait ParserCommonExp extends ParserCommon with BaseFatExp with CStructExp with Effects { self: Base with KernelOpsExp with RangeOpsExp with IfThenElseExp =>


  sealed trait ParseCell
  final case class NTCell(chart: Rep[ParseChart], offset: Rep[Int], begin: Rep[Int], end: Rep[Int], gram: Rep[Int]) extends ParseCell
  final case class TCell(chart: Rep[TermChart], offset: Rep[Int], pos: Rep[Int], gram: Rep[Int]) extends ParseCell


  def manifestParseChart: Manifest[ParseChart] = implicitly
  def manifestRuleCell: Manifest[RuleCell] = implicitly
  def manifestTermChart: Manifest[TermChart] = implicitly


  trait RuleCell
  trait ParseChart
  trait TermChart
  case class Accumulator(size: Int, prefix: String="out")  {
    val outSyms = ArrayBuffer[Int]()
    val _declared = collection.mutable.BitSet()
    def declared(id: Rep[Int]) = _declared(coerce(id))
    def declare(id: Rep[Int]) = _declared += (coerce(id))
    def local(id: Rep[Int]) = s"${prefix}${coerce(id)}"
  }
    
  def infix_rules(cell: Rep[RuleCell], r: Rep[Int], g: Rep[Int]):Rep[Real] = RuleDeref(cell, r, g)

  case class RuleDeref(cell: Rep[RuleCell], r: Rep[Int], g: Rep[Int]) extends Exp[Real]


  case class Mad(a: Rep[Real], b: Rep[Real], c: Rep[Real]) extends Def[Real]
  case class CellApply(cell: ParseCell, sym: Rep[Int])(implicit val pos: SourceContext) extends Def[Real]
  case class MadUpdate(acc: Accumulator, sym: Rep[Int], score1: Rep[Real], score2: Rep[Real]) extends Def[Unit]
  case class WriteOutput(cell: ParseCell, acc: Accumulator) extends Def[Unit]

  def mad(a: Rep[Real], b: Rep[Real], c: Rep[Real]): Rep[Real] = Mad(a,b,c)

  def infix_update(chart: Rep[ParseChart], offset: Rep[Int], begin: Rep[Int], end: Rep[Int], gram: Rep[Int], acc: Accumulator): Rep[Unit] = reflectWrite(chart)(WriteOutput(NTCell(chart, offset, begin, end, gram), acc))

  def infix_apply(chart: Rep[ParseChart], offset: Rep[Int], begin: Rep[Int], end: Rep[Int], gram: Rep[Int]): ParseCell = NTCell(chart, offset, begin, end, gram)

  def infix_apply(cell: ParseCell, sym: Rep[Int])(implicit pos: SourceContext): Rep[Real] = CellApply(cell, sym)(pos)

  def nontermAccumulator: Accumulator = Accumulator(grammar.numNonTerminals)

  def infix_mad(acc: Accumulator, sym: Rep[Int], score1: Rep[Real], score2: Rep[Real]): Rep[Unit] = {
    acc.outSyms += coerce(sym)
    reflectEffect(MadUpdate(acc, sym, score1, score2))
  }

  private def coerce(sym: Rep[Int]) = sym match { case Const(x) => x } 


  def infix_update(chart: Rep[TermChart], offset: Rep[Int], pos: Rep[Int], gram: Rep[Int], acc: Accumulator): Rep[Unit] = reflectEffect(WriteOutput(TCell(chart, offset, pos, gram), acc), infix_andAlso(Simple(), Write(List(chart.asInstanceOf[Sym[Any]])) ))

  def infix_apply(chart: Rep[TermChart], offset: Rep[Int], pos: Rep[Int], gram: Rep[Int]) = TCell(chart, offset, pos, gram)
}
