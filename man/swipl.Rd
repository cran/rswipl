\name{swipl}
\alias{swipl}
\title{
Invoke SWI-Prolog
}
\description{
This function is internally used to emulate swipl using the R program:
R -e "rswipl:::swipl()" -q --no-echo --args -g goal
}
\usage{
swipl()
}
\value{
\code{TRUE} on success
}
