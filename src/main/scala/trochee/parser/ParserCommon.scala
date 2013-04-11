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

/**
 * 
 * @author dlwh
 */
trait ParserCommon extends ExtraBase with SpireOps { self: Base with KernelOps with RangeOps =>
  type Real
  implicit def manifestReal: Manifest[Real]
  implicit def rigRepReal: Numeric[Real]
  def zero: Rep[Real] = unit[Real](implicitly[Numeric[Real]].zero)
  def mad(a: Rep[Real], b: Rep[Real], c: Rep[Real]):Rep[Real]

  /*
  typedef struct {
    float syms[NUM_SYMS][NUM_GRAMMARS];
  } parse_cell;
 */

  type ParseCell
  implicit def manifestParseCell: Manifest[ParseCell]
  def infix_syms(cell: Rep[ParseCell])(implicit dummy: Manifest[Unit]):Rep[Array[Array[Real]]]
  def infix_syms(cell: Rep[ParseCell], i: Rep[Int])(implicit dummy: Manifest[Unit]):Rep[Array[Real]] = infix_syms(cell) apply (i)


  type TermCell
  implicit def manifestTermCell: Manifest[TermCell]
  def infix_syms(cell: Rep[TermCell]):Rep[Array[Array[Real]]]
  def infix_syms(cell: Rep[TermCell], i: Rep[Int]):Rep[Array[Real]] = infix_syms(cell) apply (i)

  type RuleCell
  implicit def manifestRuleCell: Manifest[RuleCell]
  def infix_rules(cell: Rep[RuleCell]):Rep[Array[Array[Real]]]
  def infix_rules(cell: Rep[RuleCell], i: Rep[Int]):Rep[Array[Real]] = infix_rules(cell) apply (i)

  def CELL[T:Manifest](arr: Rep[Array[T]], offset: Rep[Int], begin: Rep[Int], end: Rep[Int]): Rep[T] = {
    val triangularIndex = end * (end - 1) / 2 + begin
    arr(offset + triangularIndex)
  }

  def grammar: Grammar[String, Real]

  def numSyms: Int = grammar.numSyms
}

trait ParserCommonExp extends ParserCommon with BaseFatExp with CStructExp { self: Base with KernelOpsExp with RangeOpsExp with IfThenElseExp =>
  trait ParseCell
  def manifestParseCell = implicitly
  override def infix_syms(cell: Rep[ParseCell])(implicit dummy: Manifest[Unit]):Rep[Array[Array[Real]]] = FieldDeref(cell, "syms")

  trait TermCell

  def manifestTermCell: Manifest[TermCell] = implicitly

  def infix_syms(cell: Rep[TermCell]): Rep[Array[Array[Real]]] = FieldDeref[TermCell, Array[Array[Real]]](cell, "syms")

  trait RuleCell

  def manifestRuleCell: Manifest[RuleCell] = implicitly

  def infix_rules(cell: Rep[RuleCell]): Rep[Array[Array[Real]]] = FieldPointerDeref[RuleCell, Array[Array[Real]]](cell, "rules")



  def arrays_fill[T: Manifest](x: Rep[Array[T]], n: Rep[T])(implicit pos: SourceContext): Rep[Unit] = ???

  case class Mad(a: Rep[Real], b: Rep[Real], c: Rep[Real]) extends Def[Real]

  def mad(a: Rep[Real], b: Rep[Real], c: Rep[Real]): Rep[Real] = Mad(a,b,c)
}