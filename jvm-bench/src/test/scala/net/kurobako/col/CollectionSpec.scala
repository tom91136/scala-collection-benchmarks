package net.kurobako.col

import net.kurobako.col.CollectionBench._
import org.scalacheck.Gen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class CollectionSpec extends AnyFlatSpec with Matchers with ScalaCheckDrivenPropertyChecks {

  private val ImmutableCollections: Gen[String] = Gen.oneOf(
    ScalaList,
    ScalaVector,
    ScalaLazyList,
    JavaArray,
    CatsChainList,
    CatsChainVector,
    CatsChainStream,
    CatsChainArray
  )
  private val MutableCollections: Gen[String] = Gen.oneOf(
    JavaArrayList,
    JavaLinkedList,
    ScalaArrayBuffer,
    ScalaListBuffer
    //		ChimeraSlidingBuffer
  )
  private val AllowedTpes: Gen[String] = Gen.oneOf(StringTpe, IntTpe)
  private val Sizes: Gen[Int] = Gen.chooseNum(0, 10000, 0)

  "immutable" should "produce the right results" in {
    forAll(Sizes, AllowedTpes, ImmutableCollections, minSuccessful(1000)) {
      (size, elementTpe, collection) =>
        val input = new ImmutableInput
        input.size = size
        input.elementTpe = elementTpe
        input.collection = collection
        input.setup()
        val ops = input.ops
        val actual = ops._fixture.actual
        val collapse = ops._collapse
        ops.head should be(actual.headOption)
        (collapse(ops.tail) should contain).theSameElementsInOrderAs(actual.tail)
        (collapse(ops.append) should contain).theSameElementsInOrderAs(actual :+ ops._fixture.a)
        (collapse(ops.prepend) should contain).theSameElementsInOrderAs(ops._fixture.a +: actual)
        (collapse(ops.concat) should contain).theSameElementsInOrderAs(actual ++ actual)
        (ops.drainArray.toSeq should contain).theSameElementsInOrderAs(actual)
        collapse(ops.apply) shouldBe empty
        collapse(ops.empty) shouldBe empty
        (collapse(ops.applyAll) should contain).theSameElementsInOrderAs(actual)
        ops.foldL should be(actual.foldLeft(0)(_.hashCode + _.hashCode))
        ops.size should be(actual.size)
    }

  }

  "mutable" should "produce the right results" in {
    forAll(Sizes, AllowedTpes, MutableCollections, minSuccessful(1000)) {
      (size, elementTpe, collection) =>
        val input = new MutableInput
        input.size = size
        input.elementTpe = elementTpe
        input.collection = collection

        def iter[A](f: MutableOps => A): A = {
          input.setup()
          val ops = input.ops
          f(ops)
        }

        iter(o =>
          (o._collapse(o.append) should contain)
            .theSameElementsInOrderAs(o._fixture.actual :+ o._fixture.a)
        )
        iter(o =>
          (o._collapse(o.prepend) should contain)
            .theSameElementsInOrderAs(o._fixture.a +: o._fixture.actual)
        )
        iter(o =>
          (o._collapse(o.concat) should contain)
            .theSameElementsInOrderAs(o._fixture.actual ++ o._fixture.actual)
        )
        iter(o => (o.drainArray.toSeq should contain).theSameElementsInOrderAs(o._fixture.actual))
        iter(o => o._collapse(o.apply) shouldBe empty)
        iter(o =>
          (o._collapse(o.applyAll) should contain).theSameElementsInOrderAs(o._fixture.actual)
        )
        iter(o => o.size should be(o._fixture.actual.size))
        iter(o => o.index should be(o._fixture.a))
    }

  }

}
