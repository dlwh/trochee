package trochee.basic

import spire.algebra.Rig
import reflect.SourceContext
import virtualization.lms.common.{BaseExp, Base}


/**
 *
 * @author dlwh
 */
trait RigOps { this: Base =>
  implicit def liftRig[T:Rig:Manifest]:Rig[Rep[T]] = new Rig[Rep[T]] {
    def plus(x: Rep[T], y: Rep[T]): Rep[T] = numeric_plus(x,y)

    def one: Rep[T] = unit(implicitly[Rig[T]].one)

    def times(x: Rep[T], y: Rep[T]): Rep[T] = numeric_times(x,y)

    def zero: Rep[T] = unit(implicitly[Rig[T]].zero)
  }

  def numeric_plus[T:Rig:Manifest](lhs: Rep[T], rhs: Rep[T])(implicit pos: SourceContext) : Rep[T]
//  def numeric_minus[T:Rig:Manifest](lhs: Exp[T], rhs: Exp[T])(implicit pos: SourceContext) : Exp[T] = RigMinus(lhs, rhs)(implicitly, implicitly, pos)
  def numeric_times[T:Rig:Manifest](lhs: Rep[T], rhs: Rep[T])(implicit pos: SourceContext) : Rep[T]
//  def numeric_divide[T:Rig:Manifest](lhs: Exp[T], rhs: Exp[T])(implicit pos: SourceContext) : Exp[T] = RigDivide(lhs, rhs)(implicitly, implicitly, pos)

//  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = (e match {
//    case e@RigPlus(l,r) => numeric_plus(f(l), f(r))(e.aev.asInstanceOf[Rig[A]], mtype(e.mev), pos)
//    case e@RigMinus(l,r) => numeric_minus(f(l), f(r))(e.aev.asInstanceOf[Rig[A]], mtype(e.mev), pos)
//    case e@RigTimes(l,r) => numeric_times(f(l), f(r))(e.aev.asInstanceOf[Rig[A]], mtype(e.mev), pos)
//    case e@RigDivide(l,r) => numeric_divide(f(l), f(r))(e.aev.asInstanceOf[Rig[A]], mtype(e.mev), pos)
//    case _ => super.mirror(e,f)
//  }).asInstanceOf[Exp[A]]


}

/**
 * 
 * @author dlwh
 */
trait RigOpsExp extends RigOps { this: BaseExp =>


  case class RigPlus[T:Rig:Manifest](lhs: Exp[T], rhs: Exp[T])(implicit val pos: SourceContext) extends Def[T]
//  case class RigMinus[T:Rig:Manifest](lhs: Exp[T], rhs: Exp[T])(implicit pos: SourceContext) extends Exp[T]
  case class RigTimes[T:Rig:Manifest](lhs: Exp[T], rhs: Exp[T])(implicit val pos: SourceContext) extends Def[T]
//  case class RigDivide[T:Rig:Manifest](lhs: Exp[T], rhs: Exp[T])(implicit pos: SourceContext) extends Exp[T]

  def numeric_plus[T:Rig:Manifest](lhs: Exp[T], rhs: Exp[T])(implicit pos: SourceContext) : Exp[T] = RigPlus(lhs, rhs)(implicitly, implicitly, pos)
//  def numeric_minus[T:Rig:Manifest](lhs: Exp[T], rhs: Exp[T])(implicit pos: SourceContext) : Exp[T] = RigMinus(lhs, rhs)(implicitly, implicitly, pos)
  def numeric_times[T:Rig:Manifest](lhs: Exp[T], rhs: Exp[T])(implicit pos: SourceContext) : Exp[T] = RigTimes(lhs, rhs)(implicitly, implicitly, pos)
//  def numeric_divide[T:Rig:Manifest](lhs: Exp[T], rhs: Exp[T])(implicit pos: SourceContext) : Exp[T] = RigDivide(lhs, rhs)(implicitly, implicitly, pos)

//  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = (e match {
//    case e@RigPlus(l,r) => numeric_plus(f(l), f(r))(e.aev.asInstanceOf[Rig[A]], mtype(e.mev), pos)
//    case e@RigMinus(l,r) => numeric_minus(f(l), f(r))(e.aev.asInstanceOf[Rig[A]], mtype(e.mev), pos)
//    case e@RigTimes(l,r) => numeric_times(f(l), f(r))(e.aev.asInstanceOf[Rig[A]], mtype(e.mev), pos)
//    case e@RigDivide(l,r) => numeric_divide(f(l), f(r))(e.aev.asInstanceOf[Rig[A]], mtype(e.mev), pos)
//    case _ => super.mirror(e,f)
//  }).asInstanceOf[Exp[A]]


}
