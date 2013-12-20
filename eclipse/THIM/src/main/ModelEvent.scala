package src.main

abstract class ModelEvent 
case class Birth(parent: Sim) extends ModelEvent
case class Birthday(sim: Sim) extends ModelEvent
case class Death(sim: Sim) extends ModelEvent
case class Movement(sim: Sim) extends ModelEvent
case class ComputeStatistics extends ModelEvent
