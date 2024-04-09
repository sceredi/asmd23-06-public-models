package pc.modelling

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers.*
import pc.examples.PNReadersWriters.pnRW

class PNSpec extends AnyFunSuite:

  test("PN for mutual exclusion should properly generate 7-length paths"):
    import pc.examples.PNMutualExclusion.*

    val expected1 = List(
      MSet(N, N),
      MSet(T, N),
      MSet(T, T),
      MSet(C, T),
      MSet(T),
      MSet(C),
      MSet(),
    )
    val expected2 = List(
      MSet(N, N),
      MSet(T, N),
      MSet(C, N),
      MSet(C, T),
      MSet(T),
      MSet(C),
      MSet(),
    )
    val expected3 = List(
      MSet(N, N),
      MSet(T, N),
      MSet(C, N),
      MSet(N),
      MSet(T),
      MSet(C),
      MSet(),
    )

    pnME.paths(MSet(N, N), 7).toSet should be:
      Set(expected1, expected2, expected3)

  test(
    "PN for readers and writers should maintain the invariant of at most one writer and multiple readers"
  ):
    import pc.examples.PNReadersWriters.*

    val illegalStates = List(
      MSet(Writing, Writing),
      MSet(Writing, Reading),
    )
    of(2)(15)
      .containsAny(illegalStates*) should be(false)

  test("PN where readers eventually surely does so"):
    import pc.examples.PNReadersWriters.*
    val illegalTransition =
      (MSet(WaitingRead, WaitingWrite), MSet(WaitingRead, Writing))
    pnRWReadersWillAlwaysRead.paths(
      MSet.ofList(List.fill(3)(Idle) ++ List(Resource)),
      15,
    ) forall { state =>
      def doLegalityCheck(l: List[MSet[Place]]): Boolean =
        l match
          case x1 :: x2 :: xs
              if (x1.extract(illegalTransition._1).isDefined)
                && (x2.extract(illegalTransition._2).isDefined) =>
            false && doLegalityCheck(x2 :: xs)
          case _ :: xs => doLegalityCheck(xs)
          case _       => true
      end doLegalityCheck
      doLegalityCheck(state)
    } should be(true)

  test("PN rw where at most 2 writers can write at the same time"):
    import pc.examples.PNReadersWriters.*
    val illegalStates = List(
      MSet(Writing, Writing, Writing),
      MSet(Writing, Reading),
    )
    pnRWMultipleWriters
      .paths(MSet.ofList(List.fill(3)(Idle) ++ List(Resource)), 15)
      .containsAny(illegalStates*) should be(false)

end PNSpec
