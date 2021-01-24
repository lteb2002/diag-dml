

push!(LOAD_PATH, "./")
push!(LOAD_PATH, "./lpsolver")
#include("RereDmlLpSolverY2.jl")
using RereDmlLpSolverAdmm
using RereDmlLpSolverPf


c=Vector([-2, -4, -1,-1, 5,5,5,5,0, 0, 0,0])
b= Vector([8, 6, 6,9])
A= Matrix([1.0  3.0  0.0  4.0  1  0  0  0  -1  0  0  0;
            2.0  1.0  0.0  0.0  0  1  0  0   0 -1  0  0;
            0.0  1.0  1.0  1.0  0  0  1  0   0  0 -1  0;
            1.0  1.0  1.0  0.0  0  0  0  1   0  0  0 -1])
@time x = RereDmlLpSolverAdmm.solveDmlLpWithAdmm(c,A,b,4,4,4,"l2",10)
println("Admm solution",x[1:4])

@time x=RereDmlLpSolverPf.solveDmlLp(c,A,b,"l2",10)
println("Pf solution",x[1:4])
