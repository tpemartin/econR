% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/production_possibility_frontier.R
\name{get_PPF}
\alias{get_PPF}
\title{Construct PPF}
\usage{
get_PPF(endowment_L, produce_x, produce_y)
}
\arguments{
\item{endowment_L}{A number.}

\item{produce_x}{A production function of L input}

\item{produce_y}{A production function of L input}
}
\value{
An environment with above 3 input arguments, a plot_PPF function and an update_endowmentL function
}
\description{
Construct PPF
}
\examples{
produce_x <- function(L) 3*L
produce_y <- function(L) 4*L

PPF_A <- get_PPF(20, produce_x, produce_y)
PPF_A$plot_PPF()
PPF_A$update_endowmentL(30)
PPF_A$plot_PPF()
}
