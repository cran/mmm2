\name{Kenya_morbidity}
\alias{Kenya_morbidity}
\docType{data}
\title{
Kenya Morbidity Data Set
}
\description{
A bivariate longitudinal binary data set.
}
\usage{data(Kenya_morbidity)}
\format{
  A data frame with 4692 observations on the following 7 variables.
  \describe{
    \item{\code{ID}}{a vector for subject ID}
    \item{\code{poor_appetite}}{a vector for poor appetite: 0 = absence, 1 = presence}
    \item{\code{headache}}{a vector for headache: 0 = absence, 1 = presence}
    \item{\code{visit_num}}{a vector for follow-up time (in month)}
    \item{\code{age_at_onset}}{a vector for age at onset (in years)}
    \item{\code{boy}}{a vector for gender: 0 = girl, 1 = boy}
    \item{\code{treatment}}{a vector for treatment: 0 = control, 1 = calorie, 2 = meat, 3 = milk}
  }
}
\details{
This data set is a sample of the big data set available at the link below. Only children with full data for the first year were included.
}
\source{
http://rem.ph.ucla.edu/mld/data/tabdelimiteddata/morbidity_class.txt
}
\references{
Neumann, C. G., Bwibo, N. O., Murphy, S. P., Sigman, M., Whaley, S., Allen, L. H., Guthrie, D., Weiss, R. E. Demment, M. W. (2003). Animal source foods improve dietary quality, 
micronutrient status, growth and cognitive function in Kenyan school children: Background, study design and baseline findings. \emph{Journal of Nutrition}, \bold{11}, 
3491S--3949S.

Weiss, R. E. (2005). \emph{Modeling longitudinal data}. Springer-Verlag, New York.
}
\examples{
data(Kenya_morbidity)
head(Kenya_morbidity, 10)
summary(Kenya_morbidity$age_at_onset)
table(Kenya_morbidity$treatment)
}
\keyword{datasets}
