% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/opportunity_cost.R
\name{compute_opportunityCost}
\alias{compute_opportunityCost}
\title{Compute the opportunity cost}
\usage{
compute_opportunityCost(choices, chosen)
}
\arguments{
\item{choices}{A list of available choices}

\item{chosen}{A character of chosen option name}
}
\value{
Populate chosen choice with its best forgone choice and its opportunity cost
}
\description{
Compute the opportunity cost
}
\examples{
choices <- list()
choices$A <- list(benefit=300, accounting_cost=210)
choices$B <- list(benefit=132, accounting_cost=100)
choices$C <- list(benefit=256, accounting_cost=231)
choices$D <- list(benefit=168, accounting_cost=262)

choices$A
choices$B
choices$C
choices$D

compute_opportunityCost(choices, "A")
compute_opportunityCost(choices, "B")
compute_opportunityCost(choices, "C")

computeEconomicGain(choices,"A")
}
