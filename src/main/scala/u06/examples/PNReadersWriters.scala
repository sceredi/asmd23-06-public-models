package pc.examples

export pc.modelling.PetriNet

import pc.utils.MSet
import pc.utils.MSet.ofList

object PNReadersWriters:
  enum Place:
    case Idle, Entering, WaitingRead, WaitingWrite, Resource, Reading, Writing

  export Place.*
  export pc.modelling.PetriNet.*
  export pc.modelling.SystemAnalysis.*
  export pc.utils.MSet

  def pnRW = PetriNet[Place](
    MSet(Idle) ~~> MSet(Entering),
    MSet(Entering) ~~> MSet(WaitingRead),
    MSet(Entering) ~~> MSet(WaitingWrite),
    MSet(Resource, WaitingRead) ~~> MSet(Resource, Reading),
    MSet(Reading) ~~> MSet(Idle),
    MSet(Resource, WaitingWrite) ~~> MSet(Writing) ^^^ MSet(Reading),
    MSet(Writing) ~~> MSet(Resource, Idle),
  ).toSystem

  def pnRWReadersWillAlwaysRead = PetriNet[Place](
    MSet(Idle) ~~> MSet(Entering),
    MSet(Entering) ~~> MSet(WaitingRead),
    MSet(Entering) ~~> MSet(WaitingWrite),
    MSet(Resource, WaitingRead) ~~> MSet(Resource, Reading),
    MSet(Reading) ~~> MSet(Idle),
    MSet(Resource, WaitingWrite) ~~> MSet(Writing)
      ^^^ MSet(Reading, WaitingRead),
    MSet(Writing) ~~> MSet(Resource, Idle),
  ).toSystem

  def of(n: Int)(depth: Int) =
    pnRW.paths(ofList(List.fill(n)(Idle) ++ List(Resource)), depth)

  extension (self: Seq[List[MSet[Place]]])
    def containsAny(places: MSet[Place]*) =
      !self.toSet.forall { state =>
        state forall { mset =>
          places forall { place =>
            mset.extract(place).isEmpty
          }
        }
      }
end PNReadersWriters

@main def mainPNReadersWriters =
  import PNReadersWriters.*
  println(of(2)(9).mkString("\n"))
  println(
    pnRWReadersWillAlwaysRead
      .paths(ofList(List.fill(3)(Idle) ++ List(Resource)), 10)
      .mkString("\n")
  )
