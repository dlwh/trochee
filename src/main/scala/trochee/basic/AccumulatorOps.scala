package trochee.basic

import scala.virtualization.lms.common.{BaseExp, Variables, Base}
import scala.reflect.SourceContext
import scala.virtualization.lms.internal.Effects

/**
 *
 *
 * @author dlwh
 */
trait AccumulatorOps extends SpireOps { this : Base with Variables =>
  type Real
  implicit def manifestReal: Manifest[Real]
  implicit def rigRepReal: Numeric[Real]
  def zero: Rep[Real] = unit[Real](implicitly[Numeric[Real]].zero)
  def mad(a: Rep[Real], b: Rep[Real], c: Rep[Real]):Rep[Real]

  type Accumulator
  def infix_mad(acc: Accumulator, sym: Rep[Int], score1: Rep[Real], score2: Rep[Real]):Rep[Unit]

  def accumulator(size: Int)(implicit pos: SourceContext):Accumulator
}


trait ArrayAccumulatorOps extends AccumulatorOps { this: Base with Variables =>
  type Rep[+X] <: X

  type Accumulator = Array[Real]
  def accumulator(size: Int)(implicit pos: SourceContext) = new Array[Real](size)

  def infix_mad(acc: Accumulator, sym: Rep[Int], score1: Rep[Real], score2: Rep[Real]):Rep[Unit] = {
    acc(sym.asInstanceOf[Int]) = (score1 * score2).asInstanceOf[Real]
  }


}


trait AccumulatorOpsExp extends AccumulatorOps { this: BaseExp with Variables with Effects =>
  case class Accumulator(size: Int, prefix: String="out")  {
    val outSyms = collection.mutable.Set[Int]()
    val declared = collection.mutable.Set[Int]()
  }

  def coerce(sym: Rep[Int]) = sym match { case Const(x) => x }

  case class MadUpdate(acc: Accumulator, sym: Int, score1: Rep[Real], score2: Rep[Real]) extends Def[Unit]

  def infix_mad(acc: Accumulator, sym: Rep[Int], score1: Rep[Real], score2: Rep[Real]): Rep[Unit] = {
    acc.outSyms += coerce(sym)
    reflectEffect(MadUpdate(acc, coerce(sym), score1, score2))
  }


  def accumulator(size: Int)(implicit pos: SourceContext) = new Accumulator(size, pos.assignedVariable.getOrElse("out"))
}
