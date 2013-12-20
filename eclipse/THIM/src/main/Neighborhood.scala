package src.main

import scala.collection.mutable.HashSet
import Util._
class Neighborhood(val capacity: Int) {
    val residents : HashSet[Sim] = HashSet()
    def avgIncome : Model.Income = residents filter (_.doneEducation) map (_.income) average
    def hasSpace: Boolean = capacity < (residents count (!_.livesAtHome)) // TODO make more efficient
    
    
}