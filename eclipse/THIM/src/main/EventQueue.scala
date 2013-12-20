package src.main

import scala.collection.mutable.PriorityQueue

class EventQueue[E] {
    var time : Double = 0.0
    val events : PriorityQueue[(E, Double)] = PriorityQueue.empty(Ordering by (_._2))
    
    def addEvent(event: E, delay: Double){
        events.enqueue((event, delay+time))
    }
    
    def nextEvent() = {
        val (event, eventTime) = events.dequeue()
        time = eventTime
        event
    }
    	

}