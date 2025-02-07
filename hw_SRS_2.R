# 2.  Using srswor, estimate the proportion of aggregation units that are
# eforensics-fraudulent (variable "eftype" not equal to 1) for one of the
# populations that have data in a file *fv.csv.  Using bounds on the error of
# estimation, compare the estimated proportions for sample sizes of n=50, n=500
# and n=1500.  Also compute estimates designed to achieve a bound of B=.003 for
# the proportion of aggregation units that are eforensics-fraudulent, evaluating
# what bound on the error of estimation your design actually achieves.

# work with California 2006 data
setwd("C:/Ian School/Math/Stats 480")
dat <- read.csv("California2006GOVfv.csv", row.names=1);
names(dat);
dim(dat);
#> names(dat);
# [1] "cname"       "cnum"        "county"      "precinct"    "eftype"     
# [6] "NVoters"     "NValid"      "Votes"       "Ntfraudmean" "Nfraudmean" 
#> dim(dat);
#[1] 22820    10

N <- dim(dat)[1];
N;

n <- 50;
# sample
set.seed(123)
s <- sample(N,n);
y <- ifelse(dat$eftype[s]>1,1,0);

# mean
phat <- sum(y)/n;
phat;
varphat <- phat*(1-phat)/(n-1)*(N-n)/N;
varphat;
SEphat <- sqrt(varphat);
boundphat <- 2*sqrt(varphat);
c(phat,SEphat,boundphat);

n <- 500;
# sample
set.seed(123)
s <- sample(N,n);
y <- ifelse(dat$eftype[s]>1,1,0);

# mean
phat <- sum(y)/n;
phat;
varphat <- phat*(1-phat)/(n-1)*(N-n)/N;
varphat;
SEphat <- sqrt(varphat);
boundphat <- 2*sqrt(varphat);
c(phat,SEphat,boundphat);

n <- 1500;
# sample
set.seed(123)
s <- sample(N,n);
y <- ifelse(dat$eftype[s]>1,1,0);

# mean
phat <- sum(y)/n;
phat;
varphat <- phat*(1-phat)/(n-1)*(N-n)/N;
varphat;
SEphat <- sqrt(varphat);
boundphat <- 2*sqrt(varphat);
c(phat,SEphat,boundphat);

# sample size for mean B=.003
# n <- Np(1-p) / ((N-1)(B^2/4)+p(1-p))
B <- .003;

p <- .05;  # guesstimate for p
nn <- N*p*(1-p) / ((N-1)*(B^2/4)+p*(1-p));
nn;
nB <- ceiling(nn);
nB;

# sample
sB <- sample(N,nB);
yB <- ifelse(dat$eftype[sB]>1,1,0);

# mean
phatB <- sum(yB)/nB;
phatB;
varphatB <- phatB*(1-phatB)/(nB-1)*(N-nB)/N;
varphatB;
SEphatB <- sqrt(varphatB);
boundphatB <- 2*sqrt(varphatB);
c(phatB,SEphatB,boundphatB);
c(phat,SEphat,boundphat);

# Monte Carlo sample of samples

K <- 1000;
N <- dim(dat)[1];
n <- 500;
p <- .05;  # guesstimate for p
nn <- N*p*(1-p) / ((N-1)*(B^2/4)+p*(1-p));
nn;
nB <- ceiling(nn);
nB;
phatB <- varphatB <- phatn <- varphatn <- rep(NA,K);
for (k in 1:K) {
  sB <- sample(N,nB);
  yB <- ifelse(dat$eftype[sB]>1,1,0);
  phatB[k] <- sum(yB)/nB;
  varphatB[k] <- phatB[k]*(1-phatB[k])/(nB-1)*(N-nB)/N;
  s <- sample(N,n);
  y <- ifelse(dat$eftype[s]>1,1,0);
  phatn[k] <- sum(y)/n;
  varphatn[k] <- phatn[k]*(1-phatn[k])/(n-1)*(N-n)/N;
}
mean(phatn);
sqrt(var(phatn));
mean(sqrt(varphatn));

mean(phatB);
sqrt(var(phatB));
mean(sqrt(varphatB));

seB <- sqrt(varphatB);
sen <- sqrt(varphatn);
par(mfrow=c(2,2))
plot(density(phatB,from=min(phatB),to=max(phatB)), main="n from B:  phat")
plot(density(varphatB,from=min(varphatB),to=max(varphatB)), main="n from B:  varphatiance")
plot(density(phatn,from=min(phatn),to=max(phatn)), main="n fixed:  phat")
plot(density(varphatn,from=min(varphatn),to=max(varphatn)), main="n fixed:  varphatiance")

par(mfrow=c(2,2))
plot(density(phatB,from=min(phatB),to=max(phatB)), main="n from B:  phat")
plot(density(seB,from=min(seB),to=max(seB)), main="n from B:  stderr")
plot(density(phatn,from=min(phatn),to=max(phatn)), main="n fixed:  phat")
plot(density(sen,from=min(sen),to=max(sen)), main="n fixed:  stderr")

# show phats with bounds
par(mfrow=c(2,1))
oB <- order(phatB);
plot(c((phatB[oB]-2*sqrt(varphatB[oB]))[1:(K/2)],
  (phatB[oB]+2*sqrt(varphatB[oB]))[(K/2+1):K]), type="n",
  ylab="frauds", xlab="sorted Monte Carlo sample")
points(phatB[oB], pch=".")
points(phatB[oB]+2*sqrt(varphatB[oB]))
points(phatB[oB]-2*sqrt(varphatB[oB]))

on <- order(phatn);
plot(c((phatn[on]-2*sqrt(varphatn[on]))[1:(K/2)],
  (phatn[on]+2*sqrt(varphatn[on]))[(K/2+1):K]), type="n",
  ylab="frauds", xlab="sorted Monte Carlo sample")
points(phatn[on], pch=".")
points(phatn[on]+2*sqrt(varphatn[on]))
points(phatn[on]-2*sqrt(varphatn[on]))
