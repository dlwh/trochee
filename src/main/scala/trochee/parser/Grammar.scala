package trochee.parser

import java.io.File
import scala.io.Source
import breeze.util.Index
import scala.collection.mutable

/**
 * 
 * @author dlwh
 */
trait Grammar[T, Real] {
  def numSyms: Int = symIndex.size
  def numNonTerminals: Int = nontermIndex.size
  def numTerminals: Int = termIndex.size

  def symIndex: Index[T]
  def nontermIndex: Index[T]
  def termIndex: Index[T]
  def leftTermRules: IndexedSeq[(BinaryRule[Int], Int)]
  def rightTermRules: IndexedSeq[(BinaryRule[Int], Int)]
  def nontermRules: IndexedSeq[(BinaryRule[Int], Int)]
  def bothTermRules: IndexedSeq[(BinaryRule[Int], Int)]
  def unaryRules: IndexedSeq[(UnaryRule[Int], Int)]
  def unaryTermRules: IndexedSeq[(UnaryRule[Int], Int)]
  def root: Int

  def rules: IndexedSeq[(Rule[T],Int)]
  def ruleScores: Array[Real]

}

sealed trait Rule[+L] {
  def map[U](f: L=>U):Rule[U]
}
case class BinaryRule[+L](parent: L, leftChild: L, rightChild: L) extends Rule[L] {
  def map[U](f: (L) => U) = BinaryRule(f(parent), f(leftChild), f(rightChild))
}
case class UnaryRule[+L](parent: L, child: L) extends Rule[L] {
  def map[U](f: (L) => U) = UnaryRule(f(parent), f(child))
}

object Grammar {
  def parseFile(file: File) = {
    val ruleIndex = mutable.ArrayBuffer[Rule[String]]()
    val ruleScores = mutable.ArrayBuilder.make[Float]()

    for(line <- Source.fromFile(file).getLines()) {
      line.split("\\s") match {
        case Array(p, _, lc, rc, score) =>
          val rule = BinaryRule(p,lc,rc)
          ruleIndex += rule
          ruleScores += score.toFloat
        case Array(p, _, c, score) =>
          val rule = UnaryRule(p,c)
          ruleIndex += rule
          ruleScores += score.toFloat
        case x => throw new RuntimeException(x.toIndexedSeq.toString)
      }
    }



    apply(ruleIndex, ruleScores.result())
  }

  def apply[T, Real](rules: IndexedSeq[Rule[T]], ruleScores: Array[Real]): Grammar[T, Real] = {
    val r = rules
    val rs = ruleScores

    new Grammar[T, Real] {
      val rules = r.zipWithIndex
      val ruleScores = rs

      val (symIndex,
      nontermIndex,
      termIndex,
      leftTermRules,
      rightTermRules,
      nontermRules,
      bothTermRules,
      unaryRules,
      unaryTermRules,
      root: Int) = {
        // jesus
        val termIndex, nontermIndex,symIndex = Index[T]()
        val nonterms = rules.collect {
          case (BinaryRule(p,_,_),_) => p
          case (UnaryRule(p,c),_) if p != c => p
        }.toSet


        val rhsSyms = rules.flatMap {
          case (rule@BinaryRule(p,l,r),_) => Iterator(l,r)
          case (rule@UnaryRule(p,c),_) if p != c => Iterator(c)
          case (rule@UnaryRule(p,c),_) => Iterator.empty
        }.toSet

        val syms = nonterms ++ rhsSyms

        nonterms foreach {nontermIndex.index _}
        syms -- nonterms foreach {termIndex.index _}
        nontermIndex foreach {symIndex.index _}
        termIndex foreach {symIndex.index _}

        val rootSet = syms --rhsSyms
        if (rootSet.size > 1) {
          throw new RuntimeException("Too many roots in the grammar: " + rootSet)
        } else if(rootSet.size == 0) {
          throw new RuntimeException("No root symbol in the grammar!" + rhsSyms)
        }

        val root = (rootSet).iterator.next()

        def doIndex(sym: T) = { val nt = nontermIndex(sym); if (nt >= 0) nt -> false else termIndex(sym) -> true }
        val (binaries, unaries) = rules.map {
          case (r,i) => (r.map(doIndex), i)
        }.partition(_._1.isInstanceOf[BinaryRule[_]])

        val groupedByTerminess = binaries.asInstanceOf[IndexedSeq[(BinaryRule[(Int, Boolean)], Int)]].groupBy {case (r,i) => r.leftChild._2 -> r.rightChild._2}

        def patchRules(pair: (BinaryRule[(Int, Boolean)], Int)) = pair._1.map(_._1) -> pair._2
        val leftTermRules = groupedByTerminess.getOrElse(true -> false, IndexedSeq.empty).map(patchRules _)
        val rightTermRules = groupedByTerminess.getOrElse(false -> true, IndexedSeq.empty).map(patchRules _)
        val bothTermRules = groupedByTerminess.getOrElse(true -> true, IndexedSeq.empty).map(patchRules _)
        val nontermRules = groupedByTerminess.getOrElse(false -> false, IndexedSeq.empty).map(patchRules _)

        val uByTerminess = unaries.asInstanceOf[IndexedSeq[(UnaryRule[(Int, Boolean)], Int)]].filter(p => !p._1.child._2 || (p._1.child != p._1.parent)) groupBy {case (r,i) => r.child._2}
        def patchU(pair: (UnaryRule[(Int, Boolean)], Int)) = pair._1.map(_._1) -> pair._2
        val tUnaries = uByTerminess.getOrElse(true, IndexedSeq.empty).map(patchU _)
        val ntUnaries = uByTerminess.getOrElse(false, IndexedSeq.empty).map(patchU _)

        (symIndex, nontermIndex, termIndex, leftTermRules,
          rightTermRules, nontermRules, bothTermRules, ntUnaries, tUnaries, nontermIndex(root))
      }
    }
  }
}
