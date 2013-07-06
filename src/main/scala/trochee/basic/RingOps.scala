package trochee.basic

import scala.reflect.{ClassTag, SourceContext}
import virtualization.lms.common.{BaseExp, Base}
import breeze.math.Ring
import breeze.storage.DefaultArrayValue


/**
 *
 * @author dlwh
 */
trait RingOps  { this: Base with ExtraBase =>
  implicit def liftRing[T:Ring:Manifest]:Ring[Rep[T]] = new Ring[Rep[T]] {
    def +(x: Rep[T], y: Rep[T]): Rep[T] = numeric_plus(x,y)

    def one: Rep[T] = unit(implicitly[Ring[T]].one)

    def *(x: Rep[T], y: Rep[T]): Rep[T] = numeric_times(x,y)

    def zero: Rep[T] = unit(implicitly[Ring[T]].zero)

    override def negate(x: Rep[T]): Rep[T] = numeric_negate(x)
    override def -(x: Rep[T], y: Rep[T]): Rep[T] = numeric_minus(x,y)

    def ==(a: Rep[T], b: Rep[T]): Boolean = a == b

    def norm(a: Rep[T]): Double = ???

    def isNaN(a: Rep[T]): Boolean = ???

    def !=(a: Rep[T], b: Rep[T]): Boolean = ???

    def manifest: ClassTag[Rep[T]] = ???

    def defaultArrayValue: DefaultArrayValue[Rep[T]] = DefaultArrayValue(zero)
  }

  implicit class liftRep[T](rep: Rep[T])(implicit ring: Ring[T], man: Manifest[T]) {
    def +(other: Rep[T])(implicit pos: SourceContext) = numeric_plus(rep, other)(ring, man, pos)
    def -(other: Rep[T])(implicit pos: SourceContext) = numeric_minus(rep, other)(ring, man, pos)
  }

  def numeric_plus[T:Ring:Manifest](lhs: Rep[T], rhs: Rep[T])(implicit pos: SourceContext) : Rep[T]
  def numeric_times[T:Ring:Manifest](lhs: Rep[T], rhs: Rep[T])(implicit pos: SourceContext) : Rep[T]
  def numeric_negate[T:Ring:Manifest](lhs: Rep[T])(implicit pos: SourceContext) : Rep[T]
  def numeric_minus[T:Ring:Manifest](lhs: Rep[T], rhs: Rep[T])(implicit pos: SourceContext) : Rep[T]


}


/**
 *
 * @author dlwh
 */
trait RingOpsExp extends RingOps { this: BaseExp with ExtraBaseExp =>

  case class RingPlus[T:Ring:Manifest](lhs: Exp[T], rhs: Exp[T])(implicit val pos: SourceContext) extends Def[T]
  case class RingTimes[T:Ring:Manifest](lhs: Exp[T], rhs: Exp[T])(implicit val pos: SourceContext) extends Def[T]
  case class RingNegate[T:Ring:Manifest](lhs: Exp[T])(implicit val pos: SourceContext) extends Def[T]
  case class RingMinus[T:Ring:Manifest](lhs: Exp[T], rhs: Exp[T])(implicit val pos: SourceContext) extends Def[T]

  def numeric_plus[T:Ring:Manifest](lhs: Rep[T], rhs: Rep[T])(implicit pos: SourceContext) : Rep[T] = RingPlus(lhs, rhs)(implicitly, implicitly, pos)
  def numeric_times[T:Ring:Manifest](lhs: Rep[T], rhs: Rep[T])(implicit pos: SourceContext) : Rep[T] = RingTimes(lhs, rhs)(implicitly, implicitly, pos)
  def numeric_negate[T:Ring:Manifest](lhs: Exp[T])(implicit pos: SourceContext) : Exp[T] = RingNegate(lhs)(implicitly, implicitly, pos)
  def numeric_minus[T:Ring:Manifest](lhs: Exp[T], rhs: Exp[T])(implicit pos: SourceContext) : Exp[T] = RingMinus(lhs, rhs)(implicitly, implicitly, pos)


}
