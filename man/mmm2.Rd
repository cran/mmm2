\name{mmm2}
\alias{mmm2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function to fit multivariate marginal models with shared regression parameters
}
\description{
fits multivariate marginal models with shared regression parameters for both continous and discrete responses
}
\usage{
mmm2(data, nresp, rtype = TRUE, interaction = NULL, coefnames = NULL,
family = "gaussian", tol = 0.001, maxiter = 25, corstr = "independence", 
Mv = 1, silent = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
a data frame or matrix. Id of the subjects should be placed in the first column, multivariate responses should be placed in the columns next to id variable and the covariates which are to be included in the model should be placed in the columns next to the multivariate responses.
}
  \item{nresp}{
number of multivariate responses.
}
  \item{rtype}{
a logical variable which determines the inclusion of response type indicator variables as new covariates. The default is
set to TRUE which corresponds to the inclusion of response types by placing them right after the last covariate in the design matrix. For k multiple responses, k-1 indicator variables are to be
created and mmm2 has a systematic way of creating these variables: The first response takes 0 for all the k-1 indicator variables and jth response ( j = 2, \ldots, k) takes 1 only for the (k-j+1)th indicator variable and takes 0 otherwise.
}
  \item{interaction}{
expects a vector of integers which includes the column number of the covariates (by considering only the covariate matrix, not the whole data) which are to be interacted with the response type indicator variables. These interactions are added as new covariates right after the last response type indicator variable. If rtype is set to FALSE, mmm2 ignores interaction even if it is set to a vector of column numbers of some covariates.
}
  \item{coefnames}{
a list of names of the coefficients which provides better output.
}
  \item{family}{
an object which defines the link and variance function. The possible choices are same with the ones in the \code{"gee"} package. For details see the \code{\link{gee}} documentation. Note that \code{family=binomial} handles multivariate longitudinal binary data, \code{family=poisson} handles multivariate longitudinal count data, \code{family=gaussian} handles multivariate longitudinal (normal type) continous data and \code{family=gamma} handles multivariate longitudinal (gamma type) continous data. 
}
  \item{tol}{
the tolerance which specifies the convergency of the algorithm. 
}
  \item{maxiter}{
the maximum number of iterations to be consumed by the algorithm.
}
  \item{corstr}{
a character string which defines the structure of the working correlation matrix. For details see the \code{\link{gee}} documentation.
}
  \item{Mv}{
specifies the lag value, e.g. specification of \code{"corstr=AR-M"} and \code{"Mv=1"} indicates AR(1).
}
  \item{silent}{
a logical variable which decides the print of the iterations.
}
}
\details{
The \code{mmm2} function utilizes the \code{\link{gee}} package within. Therefore, installation and loading of that package is mandatory. \code{mmm2} loads it automatically. 
}
\value{
\item{list1 }{A list which includes the output of the multivariate marginal modeling. For the details of manipulation of the output, see the example below.}}
\references{
Ilk, O., Daniels, M. (2007). Marginalized transition random effects models for multivariate longitudinal binary data. \emph{Canadian Journal of Statistics}, \bold{35}, 105-123.

Liang, K. L., Zeger, S. L. (1986). Longitudinal data analysis using generalized linear models. \emph{Biometrika}, \bold{73}, 13-22.

Shelton, B. J., Gilbert, G. H., Liu, B., Fisher, M. (2004). A SAS macro for the analysis of multivariate longitudinal binary outcomes. \emph{Computer Methods and Programs in Biomedicine}, \bold{76}, 163-175. 

Zeger, S. L., Liang, K. L (1986). Longitudinal data analysis for discrete and continous outcomes. \emph{Biometrics}, \bold{42}, 121-130.

}
\author{
Ozgur Asar, Ozlem Ilk
}
\note{
This is the version 1.0 of this user documentation file.
}

\seealso{
\code{\link{gee}}
}

\examples{
data(mscm)
coefnames<-c("intercept","married","education",
"employed","chlth","mhlth","race","csex","housize",
"bstress","billness","week","resptype","married*resptype",
"education*resptype","employed*resptype","chlth*resptype",
"mhlth*resptype","race*resptype","csex*resptype",
"housize*resptype","bstress*resptype","billness*resptype",
"week*resptype")
# mmm2 automatically assign 0 to response type indicator
# variable for response=stress and 1 to it for response=illness
mmm2.fit<-mmm2(data=mscm,nresp=2,interaction=seq(1:11),coefnames=coefnames,
family=binomial(link=logit), corstr="exchangeable")
round(summary(mmm2.fit)$coef,3)
}
\keyword{ parsimonious modeling}
\keyword{ multivariate responses}