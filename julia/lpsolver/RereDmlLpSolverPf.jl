

#
module RereDmlLpSolverPf
using LinearAlgebra
using Optim, LineSearches
export solveDmlLp


  # solve the L1 and L2 reglized LP problem for DML
  function solveDmlLp(c,A,b, regType::String="l2", regWeight::Number=0,mainVarNum::Number=0)
    c = Float32.(c)
    A = Float32.(A)
    b = Float32.(b)
    if ndims(A) == 1
      # Julia的reshape为先列再行的格式，因此需要转置
      A = transpose(reshape(A,(length(c),length(b))))
      println("1D array of A is converted to 2D array.")
    end
    # println("c:",c)
    # println("A:",A)
    # println("b:",b)
    x0=ones(length(c))*1.5   #x初始值
    alpha=1.0 #tho增长系数
    #G=ones(length(c))
    function ctr(x)
      #println(x)
      ct= -  sum(log.(indicator(x))) +  sum((A*x-b)^2)
      return ct
    end

    function bf(x)
      bf0 = sum(indicator(x)) + sum((A*x-b).^2)
      #println(typeof(bf0))
      return bf0
    end

    function indicator(x)
      y=[e<0 ? e^2 : 0.0 for e in x]
      return y
    end

    function indicator2(x)
      y = [e < 0 ? 2.0 * e : 0 for e in x]
      return y
    end

    function pure_objective(x)
      y = c' * x + (lowercase(regType) == "l2" ? regWeight * sum(x .^ 2) : 0.0)
      return y
    end

    #原函数
    f(x)=c' * x + alpha * bf(x) + (lowercase(regType) == "l2" ? regWeight * sum(x .^ 2) : 0.0)
    #f(x)=c' * x - alpha * sum(log.(indicator(x))) - alpha * sum(log.(indicator(-A*x+b)))
    #f(x) = c' * x + alpha * sum(1.0 ./ x) - alpha * sum(1.0 ./ (A*x-b))
    #梯度函数
    function g!(G, xx)
      G .= c + alpha * indicator2(xx) + alpha * 2.0 * A' * (A*xx-b) + (lowercase(regType) == "l2" ? regWeight * 2.0 * xx : zeros(length(xx)))
      #G .= c - alpha * (1 ./ xx .^2) + alpha * A * (1 ./ (A*xx-b).^2 )
    end

    println("f:",f(x0))
    #println("g:",g!(G,x0))
    maxStep=100
    error=1.0
    currentStep = 0
    while error>1.0E-4 && currentStep < maxStep
      currentStep += 1
      res=optimize(f, g!,x0, LBFGS())
      x0 = Optim.minimizer(res)
      #println("result = ", x0 )
      error = bf(x0)
      alpha *= 3
      println("Pf Step:",currentStep,",Objective value: ",pure_objective(x0),", error:",error)
      #error=sum(abs.(ctr(x0)))
    end
    #println("result = ", x0 )
    # println("error:",error)
    return x0
  end


end
