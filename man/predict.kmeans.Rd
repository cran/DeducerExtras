\name{predict.kmeans}
\alias{predict.kmeans}
\alias{applyModel}
\title{
  Applies cluster labels
}
\description{
Applies cluster labels
}
\usage{
\method{predict}{kmeans}(object,data=NULL,...)
applyModel(object,data,...)
}
\arguments{
  \item{object}{A kmeans object}
  \item{data}{a dataset to apply the model to}
  \item{...}{Additional parameters}
}
\value{
predict returns a vector of cluster assignments. 
applyModel returns the data with a column of cluster assignments appended to it.
}



