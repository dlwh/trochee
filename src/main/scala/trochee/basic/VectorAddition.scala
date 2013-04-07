package trochee.basic

import virtualization.lms.common._
import spire.algebra._
import spire.syntax._
import spire.math._
import spire.implicits._
import trochee.ops.{UpdateOp, IndexOp}
import trochee.util.ImprovedScalaCompile

import virtualization.lms.internal.Expressions
import trochee.codegen.{OpenCLKernelGenNumericOps, OpenCLKernelCodegen, OpenCLKernelGenArrayOps, OpenCLKernelGenBase}
import trochee.kernels.{KernelOpsExp, KernelOps}


trait LinearAlgebra { this: Base =>
  type Real
  type Vector
  implicit def vspace: VectorSpace[Rep[Vector], Rep[Real]]
  implicit def scalar: Field[Rep[Real]]

}

trait IndexedLinearAlgebra extends LinearAlgebra { this: Base =>
  type Index = Rep[Int]
  type Statement = Rep[Unit]

  implicit class IndexOps(x: Rep[Vector]) {
    def update(i: Index, v: Rep[Real]) = {
      vectorUpdate(x, i, v)
    }

    def apply(i: Index)(implicit update: IndexOp[Rep[Vector], Index, Rep[Real]]) = {
      update(x, i)
    }
  }

  implicit def vectorUpdate: UpdateOp[Rep[Vector], Index, Rep[Real], Statement]
  implicit def vectorIndex: IndexOp[Rep[Vector], Index, Rep[Real]]

  implicit def const[T<:AnyVal:Manifest](u: T):Rep[T]
}

trait Interpreter extends Base {
  override type Rep[+A] = A
  override def unit[A : Manifest](a: A) = a

}

trait ArrayDoubleInterpreter extends Interpreter with IndexedLinearAlgebra {
  type Real = Double
  type Vector = Array[Double]
  def vspace:VectorSpace[Rep[Vector], Rep[Real]] = implicitly[InnerProductSpace[Array[Double], Double]]
  def scalar = vspace.scalar

  implicit def vectorUpdate = new UpdateOp[Rep[Vector], Index, Rep[Real], Unit] {
    def apply(a: Rep[Vector], i: Index, v: Rep[Real]) {
      a(i) = v
    }
  }

  implicit def vectorIndex = new IndexOp[Rep[Vector], Index, Rep[Real]] {
    def apply(a: Rep[Vector], i: Index) = {
      a(i)
    }
  }

  def const[T<:AnyVal: Manifest](u: T) = u
}

trait ArrayDoubleExp extends IndexedLinearAlgebra with ArrayOpsExp with MathOpsExp with NumericOpsExp { this: BaseExp =>
  type Real = Double
  type Vector = Array[Double]
  def vspace:VectorSpace[Rep[Vector], Rep[Real]] = new VectorSpace[Rep[Vector], Rep[Real]] {
    def plus(x: Rep[Vector], y: Rep[Vector]): Rep[Vector] = {
      BinaryOp(x, "+", y)
    }

    def negate(x: Rep[Vector]): Rep[Vector] = {
      Negate(x)
    }

    def timesl(r: Rep[Real], v: Rep[Vector]): Rep[Vector] = {
      Scale(v, r)
    }


    def zero: Rep[Vector] = ???

    implicit def scalar: Field[Rep[Real]] = ArrayDoubleExp.this.scalar
  }
  lazy val scalar = new Field[Rep[Real]] {
    def one: Rep[Real] = const(1.0)
    def zero: Rep[Real] = const(0.0)

    def negate(x: Rep[Real]): Rep[Real] = Negate(x)

    def div(x: Rep[Real], y: Rep[Real]): Rep[Real] = new NumericDivide(x, y)

    def plus(x: Rep[Real], y: Rep[Real]): Rep[Real] = new NumericPlus(x, y)

    def ceil(a: Rep[Real]): Rep[Real] = math_ceil(a)

    def floor(a: Rep[Real]): Rep[Real] = math_floor(a)

    def round(a: Rep[Real]): Rep[Real] = MathRound(a)

    def isWhole(a: Rep[Real]): Boolean = ???

    def quot(a: Rep[Real], b: Rep[Real]): Rep[Real] = div(plus(a, negate(mod(a,b))), b)

    def mod(a: Rep[Real], b: Rep[Real]): Rep[Real] = new NumericMod(a, b)

    def gcd(a: Rep[Real], b: Rep[Real]): Rep[Real] = MathGCD(a, b)

    def times(x: Rep[Real], y: Rep[Real]): Rep[Real] = new NumericTimes(x, y)
  }


  case class NumericMod[T](lhs: Exp[T], rhs: Exp[T])(implicit num: scala.math.Numeric[T], man: Manifest[T]) extends DefMN[T]()(man, num)

  // operators
  case class BinaryOp[T:Manifest](a: Rep[T], op: String, b: Rep[T]) extends Exp[T]
  case class Negate[T:Manifest](a: Rep[T]) extends Exp[T]
  case class Scale[T:Manifest, U: Manifest](a: Rep[T], b:  Rep[U]) extends Exp[T]

  case class MathRound(a: Exp[Real]) extends Exp[Real]
  case class MathGCD(a: Exp[Real], b: Exp[Real]) extends Exp[Real]


  implicit def vectorUpdate = new UpdateOp[Rep[Vector], Index, Rep[Real], Rep[Unit]] {
    def apply(a: Rep[Vector], i: Index, v: Rep[Real]) = {
      toAtom(ArrayUpdate(a, i, v))
    }
  }

  implicit def vectorIndex = new IndexOp[Rep[Vector], Index, Rep[Real]] {
    def apply(a: Rep[Vector], i: Index) = {
      val ae = reifyEffects(a)
      val ie = reifyEffects(i)
      val sae = summarizeEffects(ae)
      val sie = summarizeEffects(ie)
      reflectEffect(ArrayApply(a, i), sae andThen sie)
    }
  }

  def const[T<:AnyVal:Manifest](u: T) = unit(u)

  override def reset {
    super.reset
  }
}

// Scala code generator
trait ScalaGenLinearAlgebra extends ScalaGenBase with ScalaGenArrayOps with ScalaGenNumericOps {
  val IR: Expressions with ArrayOpsExp with NumericOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], node: Def[Any]): Unit = node match {
    case _ => super.emitNode(sym, node)
  }

}

/**
 * 
 * @author dlwh
 */
abstract class VectorAddition { this: Base with IndexedLinearAlgebra =>
  def f(v: Rep[Vector], u: Rep[Vector]) = {
    v(0) = v(0) + u(0)
  }

}

