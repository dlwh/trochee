package trochee.breeze

import breeze.linalg.DenseVector
import scala.virtualization.lms.common.{BaseExp, Base}
import trochee.basic.SimpleFieldsExp
import scala.reflect.SourceContext
import breeze.math.Semiring
import scala.virtualization.lms.internal.Effects

/**
 * TODO
 *
 * @author dlwh
 **/
trait DenseVectorOps { this : Base =>

  implicit class enrichDV[T:Manifest](vv: Rep[DenseVector[T]]) {
  def data(implicit pos: SourceContext):Rep[Array[T]] = {println("??" + pos);  enrichDV_data(vv)(implicitly, pos)}
  def length(implicit pos: SourceContext):Rep[Int] = enrichDV_length(vv)(implicitly, pos)
  def stride(implicit pos: SourceContext):Rep[Int] = enrichDV_stride(vv)(implicitly, pos)
  def offset(implicit pos: SourceContext):Rep[Int] = enrichDV_offset(vv)(implicitly, pos)
    
  }

  def enrichDV_data[T:Manifest](dv: Rep[DenseVector[T]])(implicit pos: SourceContext):Rep[Array[T]]
  def enrichDV_length[T:Manifest](dv: Rep[DenseVector[T]])(implicit pos: SourceContext):Rep[Int]
  def enrichDV_stride[T:Manifest](dv: Rep[DenseVector[T]])(implicit pos: SourceContext):Rep[Int]
  def enrichDV_offset[T:Manifest](dv: Rep[DenseVector[T]])(implicit pos: SourceContext):Rep[Int]
  def newDenseVector[T:Manifest](length: Rep[Int])(implicit pos: SourceContext):Rep[DenseVector[T]]

  object repDenseVector {
    def zeros[T:Manifest:Semiring](len: Rep[Int])(implicit pos: SourceContext):Rep[DenseVector[T]] = repDenseVector_zeros[T](len)(implicitly, implicitly, pos)
  }

  def repDenseVector_zeros[T:Manifest:Semiring](len: Rep[Int])(implicit pos: SourceContext):Rep[DenseVector[T]]
}

/**
 * TODO
 *
 * @author dlwh
 **/
trait DenseVectorOpsExp extends DenseVectorOps { this : BaseExp with SimpleFieldsExp with Effects =>
  def enrichDV_data[T:Manifest](dv: Rep[DenseVector[T]])(implicit pos: SourceContext):Rep[Array[T]] = { println("ZZZ" + pos); reflectMutable(FieldDeref[DenseVector[T], Array[T]](dv, "data")(implicitly, implicitly, pos))}
  def enrichDV_length[T:Manifest](dv: Rep[DenseVector[T]])(implicit pos: SourceContext):Rep[Int] = FieldDeref[DenseVector[T], Int](dv, "length")(implicitly[Manifest[DenseVector[T]]], implicitly, pos)
  def enrichDV_stride[T:Manifest](dv: Rep[DenseVector[T]])(implicit pos: SourceContext):Rep[Int] = FieldDeref[DenseVector[T], Int](dv, "stride")(implicitly[Manifest[DenseVector[T]]], implicitly, pos)
  def enrichDV_offset[T:Manifest](dv: Rep[DenseVector[T]])(implicit pos: SourceContext):Rep[Int]= FieldDeref[DenseVector[T], Int](dv, "offset")(implicitly[Manifest[DenseVector[T]]], implicitly, pos)
  def newDenseVector[T:Manifest](length: Rep[Int])(implicit pos: SourceContext):Rep[DenseVector[T]] = NewObject[DenseVector[T]](length)(implicitly, pos)
  def repDenseVector_zeros[T:Manifest:Semiring](len: Rep[Int])(implicit pos: SourceContext):Rep[DenseVector[T]] = StaticMethodInvocation[DenseVector[T]]("breeze.linalg.DenseVector", "zeros", IndexedSeq(manifest[T]), len)(implicitly, pos)
}
