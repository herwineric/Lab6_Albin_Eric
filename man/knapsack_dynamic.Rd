% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/knapsack_dynamic.R
\name{knapsack_dynamic}
\alias{knapsack_dynamic}
\title{Dynamic knapsack method}
\usage{
knapsack_dynamic(x, W)
}
\arguments{
\item{x}{must be a data frame with variables v (value) and w (weight)}

\item{W}{integer. Specify the maximum value for the knapsack.}
}
\value{
Returns a list of the maximum value and which elemets used.
}
\description{
Calulate with the dynamic knapsack method how to get the maximum value out of optimal weight.
}
\examples{
knapsack_objects <-
data.frame(
 w=sample(1:4000, size = 2000, replace = TRUE),
 v=runif(n = 2000, 0, 10000))
 knapsack_dynamic(x = knapsack_objects[1:8,], W = 2000)
 knapsack_dynamic(x = knapsack_objects[1:8,], W = 3500)
}
