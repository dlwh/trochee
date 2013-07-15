package trochee.breeze

import breeze.linalg.{VectorBuilder, SparseVector}
import scala.virtualization.lms.common._
import trochee.basic.{ExtraBase, SimpleFieldsExp}
import scala.reflect.SourceContext
import breeze.math.Semiring
import scala.virtualization.lms.internal.Effects

/**
 * TODO
 *
 * @author dlwh
 **/
trait SparseVectorOps { this : Base =>

  implicit class enrichSV[T:Manifest](vv: Rep[SparseVector[T]]) {
    def data(implicit pos: SourceContext):Rep[Array[T]] = enrichSV_data(vv)(implicitly, pos)
    def index(implicit pos: SourceContext):Rep[Array[Int]] = enrichSV_index(vv)(implicitly, pos)
    def length(implicit pos: SourceContext):Rep[Int] = enrichSV_length(vv)(implicitly, pos)
    def activeSize(implicit pos: SourceContext):Rep[Int] = enrichSV_activeSize(vv)(implicitly, pos)
    def apply(i: Rep[Int])(implicit pos: SourceContext):Rep[T] = enrichSV_apply(vv, i)(implicitly, pos)
    def use(index: Rep[Array[Int]], data: Rep[Array[T]], activeSize: Rep[Int])(implicit pos: SourceContext):Rep[Unit] = enrichSV_use(vv, index, data, activeSize)(implicitly, pos)
  }

  def enrichSV_data[T:Manifest](dv: Rep[SparseVector[T]])(implicit pos: SourceContext):Rep[Array[T]]
  def enrichSV_index[T:Manifest](dv: Rep[SparseVector[T]])(implicit pos: SourceContext):Rep[Array[Int]]
  def enrichSV_length[T:Manifest](dv: Rep[SparseVector[T]])(implicit pos: SourceContext):Rep[Int]
  def enrichSV_activeSize[T:Manifest](dv: Rep[SparseVector[T]])(implicit pos: SourceContext):Rep[Int]
  def newSparseVectorBuilder[T:Manifest](length: Rep[Int], initialNonZero: Rep[Int] = unit(4))(implicit pos: SourceContext):Rep[VectorBuilder[T]]
  def enrichSV_apply[T:Manifest](dv: Rep[SparseVector[T]], i: Rep[Int])(implicit pos: SourceContext):Rep[T]
  def enrichSV_use[T:Manifest](dv: Rep[SparseVector[T]], index: Rep[Array[Int]], data: Rep[Array[T]], i: Rep[Int])(implicit pos: SourceContext):Rep[Unit]

  implicit class enrichSVBuilder[T:Manifest](vv: Rep[VectorBuilder[T]]) {
    def add(i: Rep[Int], v: Rep[T])(implicit pos: SourceContext):Rep[Unit] = enrichSVBuilder_add(vv, i, v)(implicitly, pos)
    def length(implicit pos: SourceContext):Rep[Int] = enrichSVBuilder_length(vv)(implicitly, pos)
    def toSparseVector(alreadySorted:Rep[Boolean] = unit(false), keysAlreadyUnique: Rep[Boolean] = unit(false))(implicit pos: SourceContext):Rep[SparseVector[T]] = enrichSVBuilder_toSparseVector(vv, alreadySorted, keysAlreadyUnique)(implicitly, pos)
  }
  def enrichSVBuilder_add[T:Manifest](dv: Rep[VectorBuilder[T]], i: Rep[Int], v: Rep[T])(implicit pos: SourceContext):Rep[Unit]
  def enrichSVBuilder_length[T:Manifest](dv: Rep[VectorBuilder[T]])(implicit pos: SourceContext):Rep[Int]
  def enrichSVBuilder_toSparseVector[T:Manifest](dv: Rep[VectorBuilder[T]], alreadySorted:Rep[Boolean] = unit(false), keysAlreadyUnique: Rep[Boolean] = unit(false))(implicit pos: SourceContext):Rep[SparseVector[T]]

  /*
  object repSparseVector {
    def zeros[T:Manifest:Semiring](len: Rep[Int])(implicit pos: SourceContext):Rep[SparseVector[T]] = repSparseVector_zeros[T](len)(implicitly, implicitly, pos)
  }

  def repSparseVector_zeros[T:Manifest:Semiring](len: Rep[Int])(implicit pos: SourceContext):Rep[SparseVector[T]]
  */
}

/**
 * TODO
 *
 * @author dlwh
 **/
trait SparseVectorOpsExp extends SparseVectorOps { this : BaseExp with SimpleFieldsExp with Effects =>
  def enrichSV_data[T:Manifest](dv: Rep[SparseVector[T]])(implicit pos: SourceContext):Rep[Array[T]] = reflectMutable(FieldDeref[SparseVector[T], Array[T]](dv, "data")(implicitly, implicitly, pos))
  def enrichSV_index[T:Manifest](dv: Rep[SparseVector[T]])(implicit pos: SourceContext):Rep[Array[Int]] = reflectMutable(FieldDeref[SparseVector[T], Array[Int]](dv, "index")(implicitly, implicitly, pos))
  def enrichSV_length[T:Manifest](dv: Rep[SparseVector[T]])(implicit pos: SourceContext):Rep[Int] = FieldDeref[SparseVector[T], Int](dv, "length")(implicitly[Manifest[SparseVector[T]]], implicitly, pos)
  def enrichSV_activeSize[T:Manifest](dv: Rep[SparseVector[T]])(implicit pos: SourceContext):Rep[Int] = FieldDeref[SparseVector[T], Int](dv, "activeSize")(implicitly[Manifest[SparseVector[T]]], implicitly, pos)
  def newSparseVectorBuilder[T:Manifest](length: Rep[Int], initialNonZero: Rep[Int] = unit(4))(implicit pos: SourceContext):Rep[VectorBuilder[T]] = NewObject[VectorBuilder[T]](length, initialNonZero)

  def enrichSV_apply[T: Manifest](dv: Rep[SparseVector[T]], i: Rep[Int])(implicit pos: SourceContext): Rep[T] = MethodInvocation[SparseVector[T], T](dv, "apply", i)(implicitly, implicitly, pos)

  def enrichSV_use[T: Manifest](dv: Rep[SparseVector[T]], index: Rep[Array[Int]], data: Rep[Array[T]], i: Rep[Int])(implicit pos: SourceContext): Rep[Unit] = MethodInvocation[SparseVector[T], Unit](dv, "use", index, data, i)(implicitly, implicitly, pos)

  def enrichSVBuilder_add[T: Manifest](dv: Rep[VectorBuilder[T]], i: Rep[Int], v: Rep[T])(implicit pos: SourceContext): Rep[Unit] = reflectEffect(MethodInvocation[VectorBuilder[T], Unit](dv, "add", i, v)(implicitly, implicitly, pos))

  def enrichSVBuilder_length[T: Manifest](dv: Rep[VectorBuilder[T]])(implicit pos: SourceContext): Rep[Int] = FieldDeref[VectorBuilder[T], Int](dv, "length")(implicitly, implicitly, pos)

  def enrichSVBuilder_toSparseVector[T: Manifest](dv: Rep[VectorBuilder[T]], alreadySorted: Rep[Boolean], keysAlreadyUnique: Rep[Boolean])(implicit pos: SourceContext): Rep[SparseVector[T]] = MethodInvocation[VectorBuilder[T], SparseVector[T]](dv, "toSparseVector", alreadySorted, keysAlreadyUnique)(implicitly, implicitly, pos)

}




