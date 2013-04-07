package trochee.basic

import virtualization.lms.common.{BaseFatExp, Variables, BaseExp, Base}
import virtualization.lms.internal.{Effects, FatExpressions, Expressions, GenericCodegen}
import trochee.util.NiceNamesGen
import reflect.SourceContext

/**
 * 
 * @author dlwh
 */
trait SpireOps { this: Base with Variables =>
  implicit def numericToNumericOps[T:Numeric:Manifest](n: T)(implicit pos: SourceContext) = new NumericOpsCls(unit(n))(implicitly, implicitly, pos)
  implicit def repNumericToNumericOps[T:Numeric:Manifest](n: Rep[T])(implicit pos: SourceContext) = new NumericOpsCls(n)(implicitly, implicitly, pos)
  implicit def varNumericToNumericOps[T:Numeric:Manifest](n: Var[T])(implicit pos: SourceContext)  = new NumericOpsCls(readVar(n))(implicitly, implicitly, pos)

  class NumericOpsCls[T:Numeric:Manifest](lhs: Rep[T])(implicit pos: SourceContext) {
    def +[A](rhs: A)(implicit c: A => T, pos: SourceContext) = numeric_plus(lhs,unit(c(rhs)))(implicitly, implicitly, pos)
    def +(rhs: Rep[T])(implicit pos: SourceContext) = numeric_plus(lhs,rhs)(implicitly, implicitly, pos)
    def -(rhs: Rep[T])(implicit pos: SourceContext) = numeric_minus(lhs,rhs)(implicitly, implicitly, pos)
    def *(rhs: Rep[T])(implicit pos: SourceContext) = numeric_times(lhs,rhs)(implicitly, implicitly, pos)
    def /(rhs: Rep[T])(implicit pos: SourceContext) = numeric_divide(lhs,rhs)(implicitly, implicitly, pos)
  }

  //def infix_+[T:Numeric:Manifest](lhs: Rep[T], rhs: Rep[T]) = numeric_plus(lhs,rhs)
  //def infix_-[T:Numeric:Manifest](lhs: Rep[T], rhs: Rep[T]) = numeric_minus(lhs,rhs)
  //def infix_*[T:Numeric:Manifest](lhs: Rep[T], rhs: Rep[T]) = numeric_times(lhs,rhs)

  def numeric_plus[T:Numeric:Manifest](lhs: Rep[T], rhs: Rep[T])(implicit pos: SourceContext): Rep[T]
  def numeric_minus[T:Numeric:Manifest](lhs: Rep[T], rhs: Rep[T])(implicit pos: SourceContext): Rep[T]
  def numeric_times[T:Numeric:Manifest](lhs: Rep[T], rhs: Rep[T])(implicit pos: SourceContext): Rep[T]
  def numeric_divide[T:Numeric:Manifest](lhs: Rep[T], rhs: Rep[T])(implicit pos: SourceContext): Rep[T]

}

trait SpireOpsExp extends SpireOps { this: BaseExp with BaseFatExp with Variables =>
  abstract class DefMN[A:Manifest:Numeric] extends Def[A] {
    def mev = manifest[A]
    def aev = implicitly[Numeric[A]]
    def pos: SourceContext
  }


  case class NumericPlus[T:Numeric:Manifest](lhs: Exp[T], rhs: Exp[T])(implicit val pos: SourceContext) extends DefMN[T]
  case class NumericMinus[T:Numeric:Manifest](lhs: Exp[T], rhs: Exp[T])(implicit val pos: SourceContext) extends DefMN[T]
  case class NumericTimes[T:Numeric:Manifest](lhs: Exp[T], rhs: Exp[T])(implicit val pos: SourceContext) extends DefMN[T]
  case class NumericDivide[T:Numeric:Manifest](lhs: Exp[T], rhs: Exp[T])(implicit val pos: SourceContext) extends DefMN[T]

  def numeric_plus[T:Numeric:Manifest](lhs: Exp[T], rhs: Exp[T])(implicit pos: SourceContext) : Exp[T] = NumericPlus(lhs, rhs)(implicitly, implicitly, pos)
  def numeric_minus[T:Numeric:Manifest](lhs: Exp[T], rhs: Exp[T])(implicit pos: SourceContext) : Exp[T] = NumericMinus(lhs, rhs)(implicitly, implicitly, pos)
  def numeric_times[T:Numeric:Manifest](lhs: Exp[T], rhs: Exp[T])(implicit pos: SourceContext) : Exp[T] = NumericTimes(lhs, rhs)(implicitly, implicitly, pos)
  def numeric_divide[T:Numeric:Manifest](lhs: Exp[T], rhs: Exp[T])(implicit pos: SourceContext) : Exp[T] = NumericDivide(lhs, rhs)(implicitly, implicitly, pos)

  /*
  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = (e match {
    case e@NumericPlus(l,r) => numeric_plus(f(l), f(r))(e.aev.asInstanceOf[Numeric[A]], mtype(e.mev), pos)
    case e@NumericMinus(l,r) => numeric_minus(f(l), f(r))(e.aev.asInstanceOf[Numeric[A]], mtype(e.mev), pos)
    case e@NumericTimes(l,r) => numeric_times(f(l), f(r))(e.aev.asInstanceOf[Numeric[A]], mtype(e.mev), pos)
    case e@NumericDivide(l,r) => numeric_divide(f(l), f(r))(e.aev.asInstanceOf[Numeric[A]], mtype(e.mev), pos)
    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]]
  */
}

trait GenSpireOps extends NiceNamesGen { this: GenericCodegen =>
  val IR: Expressions with Effects with Variables with FatExpressions with SpireOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case op @ NumericPlus(lhs, rhs) =>
      cacheAndEmit(sym.withPos(List(op.pos)), quote(lhs) + "+" +quote(rhs))
    case op @ NumericMinus(lhs, rhs) =>
      cacheAndEmit(sym.withPos(List(op.pos)), quote(lhs) + "-" +quote(rhs))
    case op @ NumericTimes(lhs, rhs) =>
      cacheAndEmit(sym.withPos(List(op.pos)), quote(lhs) + "*" +quote(rhs))
    case op @ NumericDivide(lhs, rhs) =>
      cacheAndEmit(sym.withPos(List(op.pos)), quote(lhs) + "/" +quote(rhs))
    case _ => super.emitNode(sym, rhs)
  }
}