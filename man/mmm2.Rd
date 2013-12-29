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
mmm2(formula, id, data = NULL, rtype = TRUE, interaction = NULL, R = NULL, 
b = NULL, tol = 0.001, maxiter = 25, family = "gaussian", corstr = "independence", 
Mv = 1, silent = TRUE, scale.fix = FALSE, scale.value = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{
a formula expression, see the examples given below.
}
  \item{id}{
a vector for identification of the clusters or a single sided formula, see examples.
}
  \item{data}{
an optional data frame.
}
  \item{rtype}{
a logical variable which determines the inclusion of response type indicator variables as new covariates. The default is
set to TRUE which corresponds to the inclusion of response types by placing them right after the last covariate in the design matrix. For k multiple responses, k-1 indicator variables are to be
created and mmm2 has a systematic way of creating these variables: The first response takes 0 for all the k-1 indicator variables and jth response ( j = 2, \ldots, k) takes 1 only for the (k-j+1)th indicator variable and takes 0 otherwise.
}
  \item{interaction}{
expects a vector of integers which includes the column number of the covariates (by considering only the covariate matrix, not the whole data) which are to be interacted with the response type indicator variables. These interactions are added as new covariates right after the last response type indicator variable. If rtype is set to FALSE, mmm2 ignores interaction even if it is set to a vector of column numbers of some covariates.
}
  \item{R}{
a user specified square matrix for the working correlation matrix, appropriate when \code{corstr="fixed"}.
}
  \item{b}{
user specified initials for the parameter estimates.
}
  \item{tol}{
the tolerance which specifies the convergency of the algorithm. 
}
  \item{maxiter}{
the maximum number of iterations to be consumed by the algorithm.
}
  \item{family}{
an object which defines the link and variance function. The possible choices are same with the ones in the \code{"gee"} package. For details see the \code{\link{gee}} documentation. Note that \code{family=binomial} handles multivariate longitudinal binary data, \code{family=poisson} handles multivariate longitudinal count data, \code{family=gaussian} handles multivariate longitudinal (normal type) continous data and \code{family=gamma} handles multivariate longitudinal (gamma type) continous data. 
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
  \item{scale.fix}{
%%     ~~Describe \code{scale.fix} here~~
}
  \item{scale.value}{
%%     ~~Describe \code{scale.value} here~~
}
}
\details{
The \code{mmm2} function utilizes the \code{\link{gee}} package within.
}
\value{
Returns an onject of the results. See the examples given below.
}
\references{
Asar, O. (2012). \emph{On multivariate longitudinal binary data models and their applications in forecasting}. MS Thesis, Middle East Technical University. Available
at http://www.lancaster.ac.uk/pg/asar/thesis_Ozgur%20Asar.

Asar, O., Ilk, O. (2013). mmm: an R package for analyzing multivariate longitudinal data with multivariate marginal models. \emph{Computer Methods and Programs in Biomedicine}, \bold{112} 649--654.

Ilk, O., Daniels, M. (2007). Marginalized transition random effects models for multivariate longitudinal binary data. \emph{Canadian Journal of Statistics}, \bold{35}, 105-123.

Liang, K. L., Zeger, S. L. (1986). Longitudinal data analysis using generalized linear models. \emph{Biometrika}, \bold{73}, 13-22.

Shelton, B. J., Gilbert, G. H., Liu, B., Fisher, M. (2004). A SAS macro for the analysis of multivariate longitudinal binary outcomes. \emph{Computer Methods and Programs in Biomedicine}, \bold{76}, 163-175. 

Zeger, S. L., Liang, K. L. (1986). Longitudinal data analysis for discrete and continous outcomes. \emph{Biometrics}, \bold{42}, 121-130.
}
\author{
Ozgur Asar, Ozlem Ilk
}
\note{
This is the version 1.2 of this user documentation file.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{gee}}
}
\examples{
## binary data application
data(Kenya_morbidity)
fit1 <- mmm2(cbind(poor_appetite, headache) ~ visit_num + age_at_onset + boy + 
             as.factor(treatment), id = ~ ID, data = Kenya_morbidity, interaction = 1 : 6, 
             family = binomial(link = logit), corstr = "unstructured")
summary(fit1)$coef

## count data application
data(mlcd)
fit2<-mmm2(formula=cbind(mlcd$resp1,mlcd$resp2)~mlcd$X+mlcd$time+
mlcd$X.time,id=mlcd$ID,rtype=TRUE,interaction=1:3,family=poisson,corstr="unstructured")
summary(fit2)$coef

## continuous data application
data(mlgd)
fit3<-mmm2(formula=cbind(mlgd$resp1,mlgd$resp2)~mlgd$X+mlgd$time+
mlgd$X.time,id=mlgd$ID,rtype=TRUE,interaction=1:3,family=gaussian,corstr="unstructured")
summary(fit3)$coef
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ generalized estimationg equations }
\keyword{ multivariate marginal models}% __ONLY ONE__ keyword per line
