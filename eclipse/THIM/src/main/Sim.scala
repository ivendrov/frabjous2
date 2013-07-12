package main
import Model._
class Sim
	(var parent: Option[Sim], // could potentially not have a living parent		          
    val education: Education,
    var nbhd: Neighborhood,
    var age: Age = 0,
    var baseIncome: Income = 0,
    var income: Income = 0,
    var health: Health = Model.MAX_HEALTH,
    var livesAtHome: Boolean = true) {

    def incrAge() { age += 1 }

    def justDoneEducation: Boolean =
        age == education + 1

    def doneEducation: Boolean =
        age >= education + 1
}

object Sim {
    /**
     * create a new Sim based on its parent
     */
    def generateChild(parent: Sim, education: Education) =
        new Sim(Some(parent),
                education,
                parent.nbhd) // child is placed in the same neighborhood as its parent 
}