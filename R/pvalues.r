#' P-value calculations.
#'
#' These set of functions allow the user to calculate a p-value from the lineup after
#' it has been evaluated by K independent observers. The different functions
#' accommodate different lineup construction and showing to observers.
#' Details are in the papers Majumder et al (2012) JASA, and Hofmann et al (2015).
#' We distinguish between three different scenarios:
#' \itemize{
#' \item Scenario I: in each of K evaluations a different data set and a different set of (m-1) null plots is shown.
#' \item Scenario II: in each of K evaluations the same data set but a different set of (m-1) null plots is shown.
#' \item Scenario III: the same lineup, i.e. same data and same set of null plots, is shown to K different observers.
#' }
#' @param x number of observed picks of the data plot
#' @param K number of evaluations
#' @param m size of the lineup
#' @param N MC parameter: number of replicates on which MC probabilities are based. Higher number of replicates will decrease MC variability.
#' @param type type of simulation used: scenario 3 assumes that the same lineup is shown in all K evaluations
#' @param xp exponent used, defaults to 1
#' @param target integer value identifying the location of the data plot
#' @param upper.tail compute probabilities P(X >= x). Be aware that the use of this parameter is not consistent with the other distribution functions in base. There, a value of P(X > x) is computed for upper.tail=TRUE.
#' @return Vector/data frame. For comparison a p value based on a binomial distribution is provided as well.
#' @importFrom stats pbinom runif
#' @export
#' @examples
#' pvisual(15, 20, m=3) # triangle test
pvisual <- function(x, K, m=20, N=10000, type="scenario3", xp=1, target=1, upper.tail=TRUE) {
  freq <- get(type)(N=N, K=K, m=m, xp=xp, target=target)
  if (upper.tail) {
    sim <- vapply(x, function(y) sum(freq[as.numeric(names(freq)) >= y]), numeric(1))
    return(cbind(x=x, "simulated"=sim, "binom"=1-pbinom(x-1, size=K, prob=1/m)))
  } else {
    sim <- vapply(x, function(y) sum(freq[as.numeric(names(freq)) < y]), numeric(1))
    return(cbind(x=x, "simulated"=sim, "binom"= pbinom(x-1, size=K, prob=1/m)))
  }
}

pickData <- function(m, xp=1, dataprob=NULL, nulls=NULL) {
  probs <- runif(m)
  n.targets = length(dataprob)
  if (!is.null(dataprob)) {
    probs[seq_len(n.targets)] <- dataprob
  }
  if (!is.null(nulls)) probs[(n.targets+1):m] <- nulls
  #  sample(m, size=1, prob=1-probs)
  #  rbinom(1, size=1, prob=f(probs))
  ps <- (1-probs)^xp
  if (all (ps==0)) ps <- rep(1, length(probs))
  rbinom(1, size=1, prob=sum(ps[seq_len(n.targets)])/sum(ps))
}

scenario1 <- function(N, K, m = 20, xp=1, target=1) {
  # new lineup in each evaluation: new data, new sample of nulls
  table(replicate(N,  {
    individual <- sum(replicate(K, pickData(m, dataprob=NULL, xp=xp)) %in% target)
    individual
  }))/N
}


scenario2 <- function(N, K, m = 20, xp=1, target=1) {
  # each data evaluated K times, always with different nulls
  table(replicate(N,  {
    n.targets = length(target)
    dataprob <- runif(n.targets)
    individual <- sum(replicate(K, pickData(m, dataprob=dataprob, xp=xp) %in% target))
    individual
  }))/N
}


scenario3 <- function(N, K, m=20, xp=1, target=1) {
  # each data evaluated K times, with the same nulls
  table(replicate(N/100,
                  replicate(100, {
                    n.targets = length(target)
                    dataprob <- runif(n.targets)
                    nulls <- runif(m-n.targets)

                    individual <- sum(replicate(K, pickData(m, dataprob=dataprob, nulls=nulls, xp=xp)) %in% target)
                    individual
                  })))/N
}

scenario4 <- function(N, K, m=20, xp=1, target=1) {
  # K is vector: length(K) lineups are shown K[i] times to observers
  # all length(K) lineups show the same data, but have different nulls

  res <- replicate(N, {
    n.targets = length(target)
    dataprob <- runif(n.targets)

    individual <- vapply(seq_along(K),
                         function(i) sum(replicate(K[i], pickData(m, dataprob=dataprob, nulls=runif(m-n.targets), xp=xp)) %in% target),
                         numeric(1))
    sum(individual)
  })
  table(res)/N
}
