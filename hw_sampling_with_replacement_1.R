# sampling with replacement:
# 1. Using element sampling with replacement, estimate the total number of
#  eforensics-fraudulent votes (variable Nfraudmean) for one of the populations that
#  have data in a file *fv.csv.  Repeat the estimations using
#  (1) selection probabilities proportional to (Votes+1)/(NValid+1)
#  and (2) selection probabilities that are equal for all elements.  Compare the
#  estimated variances of the estimated totals for sample sizes of $n=50$,
#  $n=500$ and $n=1500$, comparing across sample sizes and across selection
#  probability definitions.

# work with California 2006 data

dat <- read.csv("Desktop/Stats 480/Data/California2006GOVfv.csv", row.names=1);
names(dat);
dim(dat);
#> names(dat);
# [1] "cname"       "cnum"        "county"      "precinct"    "eftype"     
# [6] "NVoters"     "NValid"      "Votes"       "Ntfraudmean" "Nfraudmean" 
#> dim(dat);
#[1] 22820    10

N <- dim(dat)[1];
N;

set.seed(123)
n <- 50;
#change for 50, 500, 1500
# function to estimate the total with with-replacement sample
estimate_total_wr <- function(y,delta) {
  n <- length(y);
  sum(y/delta)/n;
}
estimate_total_wr()
# function to estimate variance of estimate of the total with with-replacement sample
variance_total_wr <- function(y,delta) {
  n <- length(y);
  tauhat <- estimate_total_wr(y,delta);
  sum((y/delta-tauhat)^2)/(n*(n-1))
}

#  (1) selection probabilities proportional to Vote/NValid

delta <- (dat$Votes+1)/(dat$NValid+1);
delta <- delta/sum(delta);
table(delta>0);
range(delta);
plot(density(delta,from=min(delta),to=max(delta)));

s <- sample(N,n, replace=TRUE, prob=delta);  # sample membership indexes
length(s);
sdat <- dat[s,];
sdelta <- delta[s];
dim(sdat);

tauhat <- estimate_total_wr(sdat$Nfraudmean, sdelta);
tauhat;
vartauhat <- variance_total_wr(sdat$Nfraudmean, sdelta);
vartauhat;
sqrt(vartauhat);

#  (2) selection probabilities that are equal for all elements

delta1 <- rep(1,N);
delta1 <- delta1/sum(delta1);

s1 <- sample(N,n, replace=TRUE, prob=delta1);  # sample membership indexes
length(s1);
sdat1 <- dat[s1,];
sdelta1 <- delta1[s1];
dim(sdat1);

tauhat <- estimate_total_wr(sdat1$Nfraudmean, sdelta1);
tauhat;
vartauhat <- variance_total_wr(sdat1$Nfraudmean, sdelta1);
vartauhat;
sqrt(vartauhat);

# Monte Carlo sample of samples

delta <- (dat$Votes+1)/(dat$NValid+1);
delta <- delta/sum(delta);
delta1 <- rep(1,dim(dat)[1]);
delta1 <- delta1/sum(delta1);
K <- 1000;
N <- dim(dat)[1];
n <- 500;
#change for 50, 500, 1500
tuneq <- varuneq <- teq <- vareq <- rep(NA,K);
for (k in 1:K) {
  s <- sample(N,n, replace=TRUE, prob=delta);  # unequal selection probs
  y <- dat$Nfraudmean[s];
  sdelta <- delta[s];
  tuneq[k] <- estimate_total_wr(y, sdelta);
  varuneq[k] <- variance_total_wr(y, sdelta);
  s1 <- sample(N,n, replace=TRUE);  # equal selection probs
  y1 <- dat$Nfraudmean[s1];
  sdelta1 <- delta1[s1];
  teq[k] <- estimate_total_wr(y1, sdelta1);
  vareq[k] <- variance_total_wr(y1, sdelta1);
}
mean(tuneq);
sqrt(var(tuneq));
mean(sqrt(varuneq));

mean(teq);
sqrt(var(teq));
mean(sqrt(vareq));

seuneq <- sqrt(varuneq);
seeq <- sqrt(vareq);
par(mfrow=c(2,2))
plot(density(tuneq,from=min(tuneq),to=max(tuneq)), main="unequal:  mean")
plot(density(varuneq,from=min(varuneq),to=max(varuneq)), main="unequal:  variance")
plot(density(teq,from=min(teq),to=max(teq)), main="equal:  mean")
plot(density(vareq,from=min(vareq),to=max(vareq)), main="equal:  variance")

par(mfrow=c(2,2))
plot(density(tuneq,from=min(tuneq),to=max(tuneq)), main="unequal:  mean")
plot(density(seuneq,from=min(seuneq),to=max(seuneq)), main="unequal:  stderr")
plot(density(teq,from=min(teq),to=max(teq)), main="equal:  mean")
plot(density(seeq,from=min(seeq),to=max(seeq)), main="equal:  stderr")

# show totals with bounds
par(mfrow=c(2,1))
ouneq <- order(tuneq);
plot(c((tuneq[ouneq]-2*sqrt(varuneq[ouneq]))[1:(K/2)],
  (tuneq[ouneq]+2*sqrt(varuneq[ouneq]))[(K/2+1):K]), type="n",
  ylab="frauds", xlab="sorted Monte Carlo sample")
points(tuneq[ouneq], pch=".")
points(tuneq[ouneq]+2*sqrt(varuneq[ouneq]))
points(tuneq[ouneq]-2*sqrt(varuneq[ouneq]))

oeq <- order(teq);
plot(c((teq[oeq]-2*sqrt(vareq[oeq]))[1:(K/2)],
  (teq[oeq]+2*sqrt(vareq[oeq]))[(K/2+1):K]), type="n",
  ylab="frauds", xlab="sorted Monte Carlo sample")
points(teq[oeq], pch=".")
points(teq[oeq]+2*sqrt(vareq[oeq]))
points(teq[oeq]-2*sqrt(vareq[oeq]))

