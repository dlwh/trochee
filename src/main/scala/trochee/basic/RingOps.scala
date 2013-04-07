package trochee.basic

import spire.algebra.Ring
import reflect.SourceContext
import virtualization.lms.common.{BaseExp, Base}


/**
 *
 * @author dlwh
 */
trait RingOps extends RigOps { this: Base =>
  implicit def liftRing[T:Ring:Manifest]:Ring[Rep[T]] = new Ring[Rep[T]] {
    def plus(x: Rep[T], y: Rep[T]): Rep[T] = numeric_plus(x,y)

    def one: Rep[T] = unit(implicitly[Ring[T]].one)

    def times(x: Rep[T], y: Rep[T]): Rep[T] = numeric_times(x,y)

    def zero: Rep[T] = unit(implicitly[Ring[T]].zero)

    def negate(x: Rep[T]): Rep[T] = numeric_negate(x)

    override def minus(x: Rep[T], y: Rep[T]): Rep[T] = numeric_minus(x,y)

    override def fromInt(n: Int): Rep[T] = unit(implicitly[Ring[T]].fromInt(n))
  }

  def numeric_negate[T:Ring:Manifest](lhs: Rep[T])(implicit pos: SourceContext) : Rep[T]
  def numeric_minus[T:Ring:Manifest](lhs: Rep[T], rhs: Rep[T])(implicit pos: SourceContext) : Rep[T]


}


/**
 *
 * @author dlwh
 */
trait RingOpsExp extends RingOps with RigOpsExp { this: BaseExp =>

  case class RingNegate[T:Ring:Manifest](lhs: Exp[T])(implicit val pos: SourceContext) extends Def[T]
  case class RingMinus[T:Ring:Manifest](lhs: Exp[T], rhs: Exp[T])(implicit val pos: SourceContext) extends Def[T]

  def numeric_negate[T:Ring:Manifest](lhs: Exp[T])(implicit pos: SourceContext) : Exp[T] = RingNegate(lhs)(implicitly, implicitly, pos)
  def numeric_minus[T:Ring:Manifest](lhs: Exp[T], rhs: Exp[T])(implicit pos: SourceContext) : Exp[T] = RingMinus(lhs, rhs)(implicitly, implicitly, pos)


}
