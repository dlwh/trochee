package trochee.breeze

import breeze.linalg.DenseVector
import scala.virtualization.lms.common.{BaseExp, Base}
import trochee.basic.SimpleFieldsExp
import scala.reflect.SourceContext

/**
 * TODO
 *
 * @author dlwh
 **/
trait DenseVectorOps { this : Base =>
  def infix_data[T:Manifest](dv: Rep[DenseVector[T]])(implicit pos: SourceContext):Rep[Array[T]]
  def infix_length[T:Manifest](dv: Rep[DenseVector[T]])(implicit pos: SourceContext):Rep[Int]
  def infix_stride[T:Manifest](dv: Rep[DenseVector[T]])(implicit pos: SourceContext):Rep[Int]
  def infix_offset[T:Manifest](dv: Rep[DenseVector[T]])(implicit pos: SourceContext):Rep[Int]
  def newDenseVector[T:Manifest](length: Rep[Int])(implicit pos: SourceContext):Rep[DenseVector[T]]
}

/**
 * TODO
 *
 * @author dlwh
 **/
trait DenseVectorOpsExp extends DenseVectorOps { this : BaseExp with SimpleFieldsExp =>
  def infix_data[T:Manifest](dv: Rep[DenseVector[T]])(implicit pos: SourceContext):Rep[Array[T]] = FieldDeref[DenseVector[T], Array[T]](dv, "data")(implicitly, implicitly, pos)
  def infix_length[T:Manifest](dv: Rep[DenseVector[T]])(implicit pos: SourceContext):Rep[Int] = FieldDeref[DenseVector[T], Int](dv, "length")(implicitly[Manifest[DenseVector[T]]], implicitly, pos)
  def infix_stride[T:Manifest](dv: Rep[DenseVector[T]])(implicit pos: SourceContext):Rep[Int] = FieldDeref[DenseVector[T], Int](dv, "stride")(implicitly[Manifest[DenseVector[T]]], implicitly, pos)
  def infix_offset[T:Manifest](dv: Rep[DenseVector[T]])(implicit pos: SourceContext):Rep[Int]= FieldDeref[DenseVector[T], Int](dv, "offset")(implicitly[Manifest[DenseVector[T]]], implicitly, pos)
  def newDenseVector[T:Manifest](length: Rep[Int])(implicit pos: SourceContext):Rep[DenseVector[T]] = NewObject[DenseVector[T]](length)(implicitly, pos)
}