\name{new.panel}
\alias{new.panel}
\title{Pull vote data from NY Times Congress API.}
\usage{new.panel(wv, width=500, height=500)}
\description{Pull vote data from NY Times Congress API.}
\details{\code{getNYTCongress} pulls vote data from the NY Times Congress API.}
\value{XML results from NY Times API}
\keyword{data}
\author{Shane Conway \email{shane.conway@gmail.com}}
\references{\url{http://developer.nytimes.com/}}
\seealso{\code{\link{parseVotes}} which fully parse the output from this function}
\arguments{\item{congress.number}{}
\item{chamber}{}
\item{session.number}{}
\item{roll.call.number}{}
\item{type}{Specifies the type of data to retrieve}
\item{api.key}{The Times API requires an API}}
\examples{\dontrun{
roll.call.xml <- getNYTCongress(110, "senate", 2, 194)
}
data(xml, package="nytR")
votes <- parseVotes(roll.call.xml)
vote.details <- voteDetail(roll.call.xml)}
