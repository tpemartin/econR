Consumer <- function(utility_function, budget_constraint) {
  consumer <- new.env()
  rlang::with_env(
    consumer,
    {
      preference=utility_function
      budget_constraint=budget_constraint
    })
  return(consumer)
  }

Utility <- function(){}

getContainer_bundgetConstraint <- function(utility_function) {
  arglist <- formals(utility_function)
  commodityNames <- names(arglist)[unlist(lapply(arglist, is.name))]
  Prices <- vector("list", length(commodityNames))
  names(Prices) <- commodityNames
  list(
    Prices=Prices,
    Income=list()
  )
}


utility_function <- function(x, y , alpha=0.5) x^alpha*y**(1-alpha)

budget_constraint <- getContainer_bundgetConstraint(utility_function)
budget_constraint$Prices$x=5
budget_constraint$Prices$y=10
budget_constraint$Income=5000

household1 <- build_consumer(utility_function, budget_constraint)

expenditureCoefs <- {
  expenditureCoefs <- budget_constraint
  expenditureCoefs$Income <- NULL
  expenditureCoefs
}

Income <- budget_constraint$Income

commodityNames <- {
  arglist <- formals(utility_function)
  names(arglist)[unlist(lapply(arglist, is.name))]
}
Price <- unlist(expenditureCoefs)
Quantity <- vector("numeric", length(Price))
names(Quantity) <- commodityNames
Parameters_list <-
  list(
    Quantity, lambda=0
  )
getContainer_ParametersList <- function(){
  utilityArgs <- vector("list", length(commodityNames))
  names(utilityArgs) <- commodityNames
  parList <-
    list(
      utilityArgs = vector("list", length(commodityNames)),
      lambda=numeric(0)
    )
  names(parList$utilityArgs) <- commodityNames
  parList
}

par_list <- getContainer_ParametersList()
par_list$utilityArgs$x=0
par_list$utilityArgs$y=0
par_list$lambda=1
parameterize <- function(Parameters_list) unlist(Parameters_list)

lagrangianObjective2Max <- function(Parameters, Parameters_list, utility_function, budget_constraint) {
    # Parameters <- unlist(par_list)
    # Parameters_list <- par_list

    parList <- relist(Parameters, Parameters_list)
    util <- do.call("utility_function",parList$utilityArgs)

    util - parList$lambda * (budget_constraint$Income - sum(unlist(budget_constraint$Prices)*unlist(parList$utilityArgs))
                             )
  }

Income - Price*Quantity
