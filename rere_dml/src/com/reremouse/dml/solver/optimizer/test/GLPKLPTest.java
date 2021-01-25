package com.reremouse.dml.solver.optimizer.test;

import scpsolver.constraints.LinearBiggerThanEqualsConstraint;
import scpsolver.constraints.LinearEqualsConstraint;
import scpsolver.constraints.LinearSmallerThanEqualsConstraint;
import scpsolver.lpsolver.LinearProgramSolver;
import scpsolver.lpsolver.SolverFactory;
import scpsolver.problems.LinearProgram;

/**
 * Created by Administrator on 2018/7/10.
 */
public class GLPKLPTest {

	/**
	 * 
	 * @param args
	 */
    public static void main(String[] args) {

        //System.out.println(GLPK.glp_version());
        LinearProgram lp = new LinearProgram(new double[]{-3, -1, -2, 0,0,0});
        lp.addConstraint(new LinearEqualsConstraint(new double[]{1.0, 1.0, 3.0, 1.0,0.0,0.0}, 30, "c1"));
        lp.addConstraint(new LinearEqualsConstraint(new double[]{2.0, 2.0, 5.0, 0.0,1.0,0.0}, 24, "c2"));
        lp.addConstraint(new LinearEqualsConstraint(new double[]{4.0, 1.0, 2.0, 0.0,0.0,1.0}, 36, "c3"));
//        lp.addConstraint(new LinearBiggerThanEqualsConstraint(new double[]{1,0,0,0}, 0, "x1"));
//        lp.addConstraint(new LinearBiggerThanEqualsConstraint(new double[]{0,1,0,0}, 0, "x2"));
//        lp.addConstraint(new LinearBiggerThanEqualsConstraint(new double[]{0,0,1,0}, 0, "x3"));
//        lp.addConstraint(new LinearBiggerThanEqualsConstraint(new double[]{0,0,0,1}, 0, "x4"));
        lp.setMinProblem(true);
        lp.setLowerbound(new double[]{0,0,0,0,0,0,0,0});
        long start = System.currentTimeMillis();
        LinearProgramSolver solver  = SolverFactory.newDefault();
        double[] sol = solver.solve(lp);
        System.out.println(sol);
        long end = System.currentTimeMillis();
        System.out.println("Time cost:"+(end-start)/1000.0+"s");
    }

}
