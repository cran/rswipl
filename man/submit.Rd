% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/query.R
\name{submit}
\alias{submit}
\title{Submit a query that has been opened with \code{\link[=query]{query()}} before.}
\usage{
submit()
}
\value{
If the query fails, \code{FALSE} is returned. If the query succeeds, a
(possibly empty) list is returned that includes the bindings required to
satisfy the query.
}
\description{
Submit a query that has been opened with \code{\link[=query]{query()}} before.
}
\examples{
query(call("member", expression(X), list(quote(a), "b", 3L, 4, expression(Y))))
submit() # X = 3L
submit() # X = 4.0
submit() # X = TRUE
submit() # X = expression(Y) or Y = expression(X)
submit() # FALSE
submit() # warning that no query is open

query(call("member", expression(X), list(quote(a), "b", 3L, 4)))
submit() # X = a
submit() # X = "b"
clear()

}
\seealso{
\code{\link[=query]{query()}}
for a opening a query.

\code{\link[=clear]{clear()}}
for a clearing a query.
}