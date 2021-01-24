

#This module works as a example of solving linear programming problems for Ju4ja
module RereDmlLpSolver
using JuMP
using GLPK
export solveDmlLp

  # Just for testing purpose, Diag-DML and its $\ell _1$ regularization solve linear programming problems using a Scala library called "SCPSolver.jar".
  #solve linear programming
  function solveDmlLp(c::Array{Float32},A::Array{Float32,2},b::Array{Float32},ifReg::Bool=true)
    model = JuMP.Model(with_optimizer(GLPK.Optimizer))
    #println(length(c))
    @variable(model, x[i=1:length(c)])
    @constraint(model, con, A * x .>= b)
    @constraint(model, con0, x .>= 0)
    @objective(model, Min, c'*x )
    status = optimize!(model)
    result=JuMP.value.(x)
    obj=JuMP.objective_value(model)
    #println("x = ", result)
    #println("Objective value: ", obj)
    return result
  end
end
