package trochee.breeze

import breeze.linalg.{VectorBuilder, HashVector}
import scala.virtualization.lms.common._
import scala.reflect.SourceContext
import trochee.basic.SimpleFieldsExp
import scala.virtualization.lms.internal.Effects

/**
 * TODO
 *
 * @author dlwh
 **/
trait HashVectorOps extends SparseVectorOps { this : Base =>

  implicit class enrichHV[T:Manifest](vv: Rep[HashVector[T]]) {
    def data(implicit pos: SourceContext):Rep[Array[T]] = enrichHV_data(vv)(implicitly, pos)
    def index(implicit pos: SourceContext):Rep[Array[Int]] = enrichHV_index(vv)(implicitly, pos)
    def length(implicit pos: SourceContext):Rep[Int] = enrichHV_length(vv)(implicitly, pos)
    def activeSize(implicit pos: SourceContext):Rep[Int] = enrichHV_activeSize(vv)(implicitly, pos)
    def iterableSize(implicit pos: SourceContext):Rep[Int] = enrichHV_iterableSize(vv)(implicitly, pos)
    def isActive(i: Rep[Int])(implicit pos: SourceContext):Rep[Boolean] = enrichHV_isActive(vv, i)(implicitly, pos)
    def apply(i: Rep[Int])(implicit pos: SourceContext):Rep[T] = enrichHV_apply(vv, i)(implicitly, pos)
    def use(index: Rep[Array[Int]], data: Rep[Array[T]], activeSize: Rep[Int])(implicit pos: SourceContext):Rep[Unit] = enrichHV_use(vv, index, data, activeSize)(implicitly, pos)
  }

  def enrichHV_data[T:Manifest](dv: Rep[HashVector[T]])(implicit pos: SourceContext):Rep[Array[T]]
  def enrichHV_index[T:Manifest](dv: Rep[HashVector[T]])(implicit pos: SourceContext):Rep[Array[Int]]
  def enrichHV_length[T:Manifest](dv: Rep[HashVector[T]])(implicit pos: SourceContext):Rep[Int]
  def enrichHV_activeSize[T:Manifest](dv: Rep[HashVector[T]])(implicit pos: SourceContext):Rep[Int]
  def enrichHV_iterableSize[T:Manifest](dv: Rep[HashVector[T]])(implicit pos: SourceContext):Rep[Int]
  def newHashVectorBuilder[T:Manifest](length: Rep[Int], initialNonZero: Rep[Int] = unit(4))(implicit pos: SourceContext):Rep[VectorBuilder[T]]
  def enrichHV_isActive[T:Manifest](dv: Rep[HashVector[T]], i: Rep[Int])(implicit pos: SourceContext):Rep[Boolean]
  def enrichHV_apply[T:Manifest](dv: Rep[HashVector[T]], i: Rep[Int])(implicit pos: SourceContext):Rep[T]
  def enrichHV_use[T:Manifest](dv: Rep[HashVector[T]], index: Rep[Array[Int]], data: Rep[Array[T]], i: Rep[Int])(implicit pos: SourceContext):Rep[Unit]

  implicit class enrichHVBuilder[T:Manifest](vv: Rep[VectorBuilder[T]]) {
    def toHashVector(implicit pos: SourceContext):Rep[HashVector[T]] = enrichHVBuilder_toHashVector(vv)(implicitly, pos)
  }
  def enrichHVBuilder_add[T:Manifest](dv: Rep[VectorBuilder[T]], i: Rep[Int], v: Rep[T])(implicit pos: SourceContext):Rep[Unit]
  def enrichHVBuilder_length[T:Manifest](dv: Rep[VectorBuilder[T]])(implicit pos: SourceContext):Rep[Int]
  def enrichHVBuilder_toHashVector[T:Manifest](dv: Rep[VectorBuilder[T]])(implicit pos: SourceContext):Rep[HashVector[T]]

  /*
  object repHashVector {
    def zeros[T:Manifest:Semiring](len: Rep[Int])(implicit pos: SourceContext):Rep[HashVector[T]] = repHashVector_zeros[T](len)(implicitly, implicitly, pos)
  }

  def repHashVector_zeros[T:Manifest:Semiring](len: Rep[Int])(implicit pos: SourceContext):Rep[HashVector[T]]
  */
}






/**
 * TODO
 *
 * @author dlwh
 **/
trait HashVectorOpsExp extends HashVectorOps { this : BaseExp with SimpleFieldsExp with Effects =>
  def enrichHV_data[T:Manifest](dv: Rep[HashVector[T]])(implicit pos: SourceContext):Rep[Array[T]] = reflectMutable(FieldDeref[HashVector[T], Array[T]](dv, "data")(implicitly, implicitly, pos))
  def enrichHV_index[T:Manifest](dv: Rep[HashVector[T]])(implicit pos: SourceContext):Rep[Array[Int]] = reflectMutable(FieldDeref[HashVector[T], Array[Int]](dv, "index")(implicitly, implicitly, pos))
  def enrichHV_length[T:Manifest](dv: Rep[HashVector[T]])(implicit pos: SourceContext):Rep[Int] = FieldDeref[HashVector[T], Int](dv, "length")(implicitly[Manifest[HashVector[T]]], implicitly, pos)
  def enrichHV_activeSize[T:Manifest](dv: Rep[HashVector[T]])(implicit pos: SourceContext):Rep[Int] = FieldDeref[HashVector[T], Int](dv, "activeSize")(implicitly[Manifest[HashVector[T]]], implicitly, pos)
  def enrichHV_iterableSize[T:Manifest](dv: Rep[HashVector[T]])(implicit pos: SourceContext):Rep[Int] = FieldDeref[HashVector[T], Int](dv, "iterableSize")(implicitly[Manifest[HashVector[T]]], implicitly, pos)
  def newHashVectorBuilder[T:Manifest](length: Rep[Int], initialNonZero: Rep[Int] = unit(4))(implicit pos: SourceContext):Rep[VectorBuilder[T]] = NewObject[VectorBuilder[T]](length, initialNonZero)(implicitly, pos)

  def enrichHV_apply[T: Manifest](dv: Rep[HashVector[T]], i: Rep[Int])(implicit pos: SourceContext): Rep[T] = MethodInvocation[HashVector[T], T](dv, "apply", i)(implicitly, implicitly, pos)
  def enrichHV_isActive[T: Manifest](dv: Rep[HashVector[T]], i: Rep[Int])(implicit pos: SourceContext): Rep[Boolean] = MethodInvocation[HashVector[T], Boolean](dv, "isActive", i)(implicitly, implicitly, pos)

  def enrichHV_use[T: Manifest](dv: Rep[HashVector[T]], index: Rep[Array[Int]], data: Rep[Array[T]], i: Rep[Int])(implicit pos: SourceContext): Rep[Unit] = MethodInvocation[HashVector[T], Unit](dv, "use", index, data, i)(implicitly, implicitly, pos)

  def enrichHVBuilder_add[T: Manifest](dv: Rep[VectorBuilder[T]], i: Rep[Int], v: Rep[T])(implicit pos: SourceContext): Rep[Unit] = reflectEffect(MethodInvocation[VectorBuilder[T], Unit](dv, "add", i, v)(implicitly, implicitly, pos))

  def enrichHVBuilder_length[T: Manifest](dv: Rep[VectorBuilder[T]])(implicit pos: SourceContext): Rep[Int] = FieldDeref[VectorBuilder[T], Int](dv, "length")(implicitly, implicitly, pos)

  def enrichHVBuilder_toHashVector[T: Manifest](dv: Rep[VectorBuilder[T]])(implicit pos: SourceContext): Rep[HashVector[T]] = MethodInvocation[VectorBuilder[T], HashVector[T]](dv, "toHashVector")(implicitly, implicitly, pos)
}


