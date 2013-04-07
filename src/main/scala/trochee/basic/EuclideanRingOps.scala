package trochee.basic

import spire.algebra.{Ring, EuclideanRing}
import reflect.SourceContext
import virtualization.lms.common.{BaseExp, Base}


/**
 *
 * @author dlwh
 */
trait EuclideanRingOps extends RingOps { this: Base =>
  implicit def liftEuclideanRing[T:EuclideanRing:Manifest]:EuclideanRing[Rep[T]] = new EuclideanRing[Rep[T]] {
    def times(x: Rep[T], y: Rep[T]): Rep[T] = numeric_times(x,y)

    def quot(x: Rep[T], y: Rep[T]): Rep[T] = numeric_divide(x,y)

    def plus(x: Rep[T], y: Rep[T]) = numeric_plus(x, y)

    def one: Rep[T] = unit(implicitly[EuclideanRing[T]].one)
    def zero: Rep[T] = unit(implicitly[EuclideanRing[T]].zero)

    def mod(a: Rep[T], b: Rep[T]): Rep[T] = ???

    def gcd(a: Rep[T], b: Rep[T]): Rep[T] =  ???

    def negate(x: Rep[T]): Rep[T] = numeric_negate(x)
    override def minus(x: Rep[T], y: Rep[T]): Rep[T] = numeric_minus(x,y)
    override def fromInt(n: Int): Rep[T] = unit(implicitly[Ring[T]].fromInt(n))
  }

  def numeric_divide[T:EuclideanRing:Manifest](lhs: Rep[T], rhs: Rep[T])(implicit pos: SourceContext) : Rep[T] //= DivideOp(lhs, rhs)(implicitly, implicitly, pos)

//  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = (e match {
//    case e@EuclideanRingPlus(l,r) => numeric_plus(f(l), f(r))(e.aev.asInstanceOf[EuclideanRing[A]], mtype(e.mev), pos)
//    case e@EuclideanRingMinus(l,r) => numeric_minus(f(l), f(r))(e.aev.asInstanceOf[EuclideanRing[A]], mtype(e.mev), pos)
//    case e@EuclideanRingTimes(l,r) => numeric_times(f(l), f(r))(e.aev.asInstanceOf[EuclideanRing[A]], mtype(e.mev), pos)
//    case e@EuclideanRingDivide(l,r) => numeric_divide(f(l), f(r))(e.aev.asInstanceOf[EuclideanRing[A]], mtype(e.mev), pos)
//    case _ => super.mirror(e,f)
//  }).asInstanceOf[Exp[A]]


}

trait EuclideanRingOpsExp extends EuclideanRingOps with RingOpsExp { this: BaseExp =>


  case class DivideOp[T:Manifest](lhs: Exp[T], rhs: Exp[T])(implicit val pos: SourceContext) extends Def[T]
  def numeric_divide[T:EuclideanRing:Manifest](lhs: Rep[T], rhs: Rep[T])(implicit pos: SourceContext) : Rep[T] = DivideOp(lhs, rhs)(implicitly, pos)

//  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = (e match {
//    case e@EuclideanRingPlus(l,r) => numeric_plus(f(l), f(r))(e.aev.asInstanceOf[EuclideanRing[A]], mtype(e.mev), pos)
//    case e@EuclideanRingMinus(l,r) => numeric_minus(f(l), f(r))(e.aev.asInstanceOf[EuclideanRing[A]], mtype(e.mev), pos)
//    case e@EuclideanRingTimes(l,r) => numeric_times(f(l), f(r))(e.aev.asInstanceOf[EuclideanRing[A]], mtype(e.mev), pos)
//    case e@EuclideanRingDivide(l,r) => numeric_divide(f(l), f(r))(e.aev.asInstanceOf[EuclideanRing[A]], mtype(e.mev), pos)
//    case _ => super.mirror(e,f)
//  }).asInstanceOf[Exp[A]]


}

