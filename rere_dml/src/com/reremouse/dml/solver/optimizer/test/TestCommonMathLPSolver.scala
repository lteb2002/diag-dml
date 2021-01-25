package com.reremouse.dml.solver.optimizer.test

import org.apache.commons.math3.optim.PointValuePair
import org.apache.commons.math3.optim.linear._
import org.apache.commons.math3.optim.nonlinear.scalar.GoalType

/**
  * Created by RereMouse on 2018-01-18.
  */
object TestCommonMathLPSolver {



  def main(args: Array[String]): Unit = { //describe the optimization problem
    val f = new LinearObjectiveFunction(Array[Double](3, 5), 0)
    val constraints = new java.util.ArrayList[LinearConstraint]
    constraints.add(new LinearConstraint(Array[Double](2, 8), Relationship.LEQ, 13))
    constraints.add(new LinearConstraint(Array[Double](5, -1), Relationship.LEQ, 11))
    constraints.add(new LinearConstraint(Array[Double](1, 0), Relationship.GEQ, 0))
    constraints.add(new LinearConstraint(Array[Double](0, 1), Relationship.GEQ, 0))
    val start = System.currentTimeMillis
    //create and run solver
    var solution: PointValuePair = null
    solution = new SimplexSolver().optimize(f, new LinearConstraintSet(constraints), GoalType.MAXIMIZE)
    if (solution != null) { //get solution
      val max = solution.getValue
      System.out.println("Opt: " + max)
      //print decision variables
      val ps = solution.getPoint
      for (p <- ps) {
        print(p + "\t")
      }
    }
    val end = System.currentTimeMillis
    System.out.println("\nTime cost:" + (end - start) / 1000.0 + "s")
  }


}
