# 1.  Using simple random sampling without replacement (srswor), estimate the
# total and mean number of eforensics-fraudulent votes (variable "Nfraudmean")
# for one of the populations that have data in a file *fv.csv.  Using bounds on
# the error of estimation, compare the estimates of the estimated totals and
# means for sample sizes of n=50, n=500 and n=1500, and compare total estimates
# for each of those sample sizes to results for the same sample size when using
# element sampling with replacement.  Also compute srswor estimates designed to
# achieve a bound of B=5 for the mean, evaluating what bound on the error of
# estimation your design actually achieves (when designing the sample you
# may treat population variables "NVoters", "NValid" and "Votes" as known).

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
y <- dat$Nfraudmean[s];

# mean
ybar <- sum(y)/n;
ybar;
s2 <- sum((y-ybar)^2)/(n-1);
varybar <- s2/n * (N-n)/N;
varybar;
SEybar <- sqrt(varybar);
boundybar <- 2*sqrt(varybar);
c(ybar,SEybar,boundybar);


# total
tauhat <- N*ybar;
tauhat;
vartauhat <- N^2 * varybar;
vartauhat;
SEtauhat <- sqrt(vartauhat);
boundtauhat <- 2*sqrt(vartauhat);
c(tauhat,SEtauhat,boundtauhat);

n <- 500;

# sample
set.seed(123)
s <- sample(N,n);
y <- dat$Nfraudmean[s];

# mean
ybar <- sum(y)/n;
ybar;
s2 <- sum((y-ybar)^2)/(n-1);
varybar <- s2/n * (N-n)/N;
varybar;
SEybar <- sqrt(varybar);
boundybar <- 2*sqrt(varybar);
c(ybar,SEybar,boundybar);

# total
tauhat <- N*ybar;
tauhat;
vartauhat <- N^2 * varybar;
vartauhat;
SEtauhat <- sqrt(vartauhat);
boundtauhat <- 2*sqrt(vartauhat);
c(tauhat,SEtauhat,boundtauhat);

n <- 1500;

# sample
set.seed(123)
s <- sample(N,n);
y <- dat$Nfraudmean[s];

# mean
ybar <- sum(y)/n;
ybar;
s2 <- sum((y-ybar)^2)/(n-1);
varybar <- s2/n * (N-n)/N;
varybar;
SEybar <- sqrt(varybar);
boundybar <- 2*sqrt(varybar);
c(ybar,SEybar,boundybar);

# total
tauhat <- N*ybar;
tauhat;
vartauhat <- N^2 * varybar;
vartauhat;
SEtauhat <- sqrt(vartauhat);
boundtauhat <- 2*sqrt(vartauhat);
c(tauhat,SEtauhat,boundtauhat);


####WITH REPLACEMENT


n <- 50;

# sample
set.seed(123)
s <- sample(N,n, replace = TRUE);
y <- dat$Nfraudmean[s];

# mean
ybar <- sum(y)/n;
ybar;
s2 <- sum((y-ybar)^2)/(n-1);
varybar <- s2/n * (N-n)/N;
varybar;
SEybar <- sqrt(varybar);
boundybar <- 2*sqrt(varybar);
c(ybar,SEybar,boundybar);

# total
tauhat <- N*ybar;
tauhat;
vartauhat <- N^2 * varybar;
vartauhat;
SEtauhat <- sqrt(vartauhat);
boundtauhat <- 2*sqrt(vartauhat);
c(tauhat,SEtauhat,boundtauhat);

n <- 500;

# sample
set.seed(123)
s <- sample(N,n, replace = TRUE);
y <- dat$Nfraudmean[s];

# mean
ybar <- sum(y)/n;
ybar;
s2 <- sum((y-ybar)^2)/(n-1);
varybar <- s2/n * (N-n)/N;
varybar;
SEybar <- sqrt(varybar);
boundybar <- 2*sqrt(varybar);
c(ybar,SEybar,boundybar);

# total
tauhat <- N*ybar;
tauhat;
vartauhat <- N^2 * varybar;
vartauhat;
SEtauhat <- sqrt(vartauhat);
boundtauhat <- 2*sqrt(vartauhat);
c(tauhat,SEtauhat,boundtauhat);

n <- 1500;

# sample
set.seed(123)
s <- sample(N,n, replace = TRUE);
y <- dat$Nfraudmean[s];

# mean
ybar <- sum(y)/n;
ybar;
s2 <- sum((y-ybar)^2)/(n-1);
varybar <- s2/n * (N-n)/N;
varybar;
SEybar <- sqrt(varybar);
boundybar <- 2*sqrt(varybar);
c(ybar,SEybar,boundybar);

# total
tauhat <- N*ybar;
tauhat;
vartauhat <- N^2 * varybar;
vartauhat;
SEtauhat <- sqrt(vartauhat);
boundtauhat <- 2*sqrt(vartauhat);
c(tauhat,SEtauhat,boundtauhat);

# sample size for mean B=5
# n <- N*sigma^2 / ((N-1)(B^2/4)+sigma^2)
B <- 5;

sigma2 <- var(.1 * dat$Votes);  # guesstimate for sigma^2
sigma2;
nn <- N*sigma2 / ((N-1)*(B^2/4)+sigma2);
nn;
nB <- ceiling(nn);
nB;

# sample
set.seed(123)
sB <- sample(N,nB);
yB <- dat$Nfraudmean[sB];

# mean
ybarB <- sum(yB)/nB;
ybarB;
s2B <- sum((yB-ybarB)^2)/(nB-1);
varybarB <- s2B/nB * (N-nB)/N;
varybarB;
SEybarB <- sqrt(varybarB);
boundybarB <- 2*sqrt(varybarB);
c(nB,ybarB,SEybarB,boundybarB);
c(n,ybar,SEybar,boundybar);



# Monte Carlo sample of samples

K <- 1000;
N <- dim(dat)[1];
n <- 500;
sigma2 <- var(.1 * dat$Votes);  # guesstimate for sigma^2
sigma2;
nn <- N*sigma2 / ((N-1)*(B^2/4)+sigma2);
nn;
nB <- ceiling(nn);
nB;
meanB <- varB <- meann <- varn <- rep(NA,K);
for (k in 1:K) {
  sB <- sample(N,nB);
  yB <- dat$Nfraudmean[sB];
  meanB[k] <- sum(yB)/nB;
  varB[k] <- (sum((yB-ybarB)^2)/(nB-1))/nB * (N-nB)/N;
  s <- sample(N,n);
  y <- dat$Nfraudmean[s];
  meann[k] <- sum(y)/n;
  varn[k] <- (sum((y-ybar)^2)/(n-1))/n * (N-n)/N;
}
mean(meann);
sqrt(var(meann));
mean(sqrt(varn));

mean(meanB);
sqrt(var(meanB));
mean(sqrt(varB));

seB <- sqrt(varB);
sen <- sqrt(varn);
par(mfrow=c(2,2))
plot(density(meanB,from=min(meanB),to=max(meanB)), main="n from B:  mean")
plot(density(varB,from=min(varB),to=max(varB)), main="n from B:  variance")
plot(density(meann,from=min(meann),to=max(meann)), main="n fixed:  mean")
plot(density(varn,from=min(varn),to=max(varn)), main="n fixed:  variance")

par(mfrow=c(2,2))
plot(density(meanB,from=min(meanB),to=max(meanB)), main="n from B:  mean")
plot(density(seB,from=min(seB),to=max(seB)), main="n from B:  stderr")
plot(density(meann,from=min(meann),to=max(meann)), main="n fixed:  mean")
plot(density(sen,from=min(sen),to=max(sen)), main="n fixed:  stderr")

# show means with bounds
par(mfrow=c(2,1))
oB <- order(meanB);
plot(c((meanB[oB]-2*sqrt(varB[oB]))[1:(K/2)],
  (meanB[oB]+2*sqrt(varB[oB]))[(K/2+1):K]), type="n",
  ylab="frauds", xlab="sorted Monte Carlo sample")
points(meanB[oB], pch=".")
points(meanB[oB]+2*sqrt(varB[oB]))
points(meanB[oB]-2*sqrt(varB[oB]))

on <- order(meann);
plot(c((meann[on]-2*sqrt(varn[on]))[1:(K/2)],
  (meann[on]+2*sqrt(varn[on]))[(K/2+1):K]), type="n",
  ylab="frauds", xlab="sorted Monte Carlo sample")
points(meann[on], pch=".")
points(meann[on]+2*sqrt(varn[on]))
points(meann[on]-2*sqrt(varn[on]))
