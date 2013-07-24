package trochee.basic.collection

import scala.virtualization.lms.common.{BaseExp, Base}
import scala.reflect.SourceContext

/**
 * TODO
 *
 * @author dlwh
 **/
trait BitSetOps { this: Base =>
  trait BitSet

  def newBitSet()(implicit pos: SourceContext):Rep[BitSet]

  implicit class richBitSet(bs: Rep[BitSet]) {
    def +=(k: Rep[Int])(implicit pos: SourceContext):Rep[Unit] = infixBitSet_+=(bs, k)(pos)
    def apply(k: Rep[Int])(implicit pos: SourceContext):Rep[Boolean] = infixBitSet_apply(bs, k)(pos)
  }


  def infixBitSet_+=(bs: Rep[BitSet], i: Rep[Int])(implicit pos: SourceContext):Rep[Unit]
  def infixBitSet_apply(bs: Rep[BitSet], i: Rep[Int])(implicit pos: SourceContext):Rep[Boolean]
}

trait BitSetOpsExp extends BitSetOps { this: BaseExp =>
  def infixBitSet_+=(bs: Rep[BitSet], i: Rep[Int])(implicit pos: SourceContext): Rep[Unit] = BitSet_+=(bs, i)(pos)
  def infixBitSet_apply(bs: Rep[BitSet], i: Rep[Int])(implicit pos: SourceContext): Rep[Boolean] = BitSet_apply(bs, i)(pos)
  def newBitSet()(implicit pos: SourceContext): Rep[BitSet] = new NewBitSet()(pos)

  case class BitSet_+=(lhs: Rep[BitSet], rhs: Rep[Int])(implicit val pos: SourceContext) extends Def[Unit]
  case class BitSet_apply(lhs: Rep[BitSet], rhs: Rep[Int])(implicit val pos: SourceContext) extends Def[Boolean]
  case class NewBitSet()(implicit val pos: SourceContext) extends Def[BitSet]
}
