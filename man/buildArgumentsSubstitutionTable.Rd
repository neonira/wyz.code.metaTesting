\name{buildArgumentsSubstitutionTable}
\alias{buildArgumentsSubstitutionTable}
\title{Build parameter code transformation table.}
\description{
Use method \code{buildArgumentsSubstitutionTable} to build parameter code transformation table.
}
\usage{
buildArgumentsSubstitutionTable(fun_f_1)
}
\arguments{
\item{fun_f_1}{A constrained vector of function values. Vector length must be 1.}
}
\value{
A \code{data.table} with one line per parameter name. Most interesting column
is named \code{requires_name_propagation}. It hols a boolean value, that is
\code{TRUE} if and only if the parameter is default valued and required a
parameter name substitution.
}
\author{
\packageAuthor{wyz.code.metaTesting}

Maintainer: \packageMaintainer{wyz.code.metaTesting}
}
\examples{
# Typical case
print(buildArgumentsSubstitutionTable(sum))
#
}
\keyword{ programming }
\keyword{ utilities }
\concept{ meta testing }

