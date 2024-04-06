package pc.examples

export pc.modelling.PetriNet

import pc.utils.MSet

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
    MSet(Writing) ~~> MSet(Resource, Idle)
  ).toSystem

@main def mainPNReadersWriters =
  import PNReadersWriters.*
  println(pnRW.paths(MSet(Idle, Idle, Resource), 9).toList.mkString("\n"))
