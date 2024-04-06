package pc.modelling

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers.*

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
      MSet()
    )
    val expected2 = List(
      MSet(N, N),
      MSet(T, N),
      MSet(C, N),
      MSet(C, T),
      MSet(T),
      MSet(C),
      MSet()
    )
    val expected3 = List(
      MSet(N, N),
      MSet(T, N),
      MSet(C, N),
      MSet(N),
      MSet(T),
      MSet(C),
      MSet()
    )

    pnME.paths(MSet(N, N), 7).toSet should be:
      Set(expected1, expected2, expected3)

  test(
    "PN for readers and writers should maintain the invariant of at most one writer and multiple readers"
  ):
    import pc.examples.PNReadersWriters.*

    val illegalStates = List(MSet(Writing, Writing), MSet(Writing, Reading))
    pnRW.paths(MSet(Resource, Idle, Idle), 15).toSet.foreach { state =>
      state forall { mset =>
        illegalStates forall { illegal =>
          mset.extract(illegal).isEmpty
        }
      } should be(true)
    }
