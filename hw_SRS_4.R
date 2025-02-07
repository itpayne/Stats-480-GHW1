dat <- read.csv("Desktop/Stats 480/Data/California2006GOVfv_miss.csv", row.names=1);
dat <- dat %>%
  filter(!is.na(Nfraudmean))
names(dat);
dim(dat);
#> names(dat);
# [1] "cname"       "cnum"        "county"      "precinct"    "eftype"     
# [6] "NVoters"     "NValid"      "Votes"       "Ntfraudmean" "Nfraudmean" 
#> dim(dat);
#[1] 22820    10
N <- dim(dat)[1];
N;
n <- 500;
#change for 50, 500, 1500

# sample
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

# sample size for mean B=5
#n <- N*sigma2 / ((N-1)(B^2/4)+sigma^2)
B <- 5;
sigma2 <- var(.1 * dat$Votes, na.rm = TRUE);  # guesstimate for sigma^2
sigma2;
nn <- N*sigma2 / ((N-1)*(B^2/4)+sigma2);
nn;
nB <- ceiling(nn);
nB;

# sample

sB <- sample(N,nB);
yB <- dat$Nfraudmean[sB];



# mean
ybarB <- sum(yB, na.rm = TRUE)/nB;
ybarB;
tauhatB <- N * ybarB
tauhatB
s2B <- sum((yB-ybarB)^2, na.rm = TRUE)/(nB-1);
varybarB <- s2B/nB * (N-nB)/N;
varybarB;
SEybarB <- sqrt(varybarB);
boundybarB <- 2*sqrt(varybarB);
c(nB,ybarB,SEybarB,boundybarB);
c(n,ybar,SEybar,boundybar);

# Monte Carlo sample of samples
K <- 1000;
N <- dim(dat)[1];
set.seed(123)
n <- 1500;
sigma2 <- var(.1 * dat$Votes, na.rm = TRUE);  # guesstimate for sigma^2
sigma2;
nn <- N*sigma2 / ((N-1)*(B^2/4)+sigma2);
nn;
nB <- ceiling(nn);
nB;
meanB <- varB <- meann <- varn <- rep(NA,K);
for (k in 1:K) {
  sB <- sample(N,nB);
  yB <- dat$Nfraudmean[sB];
  meanB[k] <- sum(yB, na.rm = TRUE)/nB;
  varB[k] <- (sum((yB-ybarB)^2, na.rm = TRUE)/(nB-1))/nB * (N-nB)/N;
  s <- sample(N,n);
  y <- dat$Nfraudmean[s];
  meann[k] <- sum(y, na.rm = TRUE)/n;
  varn[k] <- (sum((y-ybar)^2, na.rm = TRUE)/(n-1))/n * (N-n)/N;
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

