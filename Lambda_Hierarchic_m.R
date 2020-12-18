rm(list=ls())

#Putting all Latin American country in the same group doesn't work, so I create three different groups (high, medium and low). Note that Chile is both in high and medium longevity group, just to show 
countryh <- c("Cos","Cub","Chi","Ecu","Mex","Pan")#e0>70
countrym <- c("Arg","Bra","Chi","Col","Dom","Par","Per","Ven","Uru") #e_0<70 & e_0>67
countryl <- c("Els","Gua","Nic")#e_0<=67

#Read life table data of LAtin AMerican countries, 
data.m <- read.csv("tables_LAMBDA.csv",header=TRUE)
Age <- c(0,1,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85)

#Set negative values, if any (might be at extreme ages) at 0
data.m[data.m<0] <- 0

#in order to make the algorithm faster we reduce the sample size. This is equivaletn to make a 10% sample from data
data.m[,-1] <- round(data.m[,-1]/10,digits=0)

#We trasform data from aggregated to individual, assuming a uniform distribution for all age classes and an exponential for the firs and the last one
y <- c(rexp(n=data.m[1,2],rate=4),unlist(mapply(FUN=runif, data.m[-c(1,19),2],Age[-length(Age)][-1],Age[-1][-1])),(85+rexp(data.m[19,2],0.45)))
country <- (rep(1,length(y)))
for (j in 3:ncol(data.m)){
    d1 <- c(rexp(n=data.m[1,j],rate=4),unlist(mapply(FUN=runif, data.m[-c(1,19),j],Age[-length(Age)][-1],Age[-1][-1])),(85+rexp(data.m[19,j],0.45)))
    country1 <- (rep((j-1),length(d1)))
    y <- c(y,d1)
    country <- c(country,country1)
}

#nJ is the number of countries in the group
nJ <- ncol(data.m)-1

#dropping ages<5, we disregard infant and child mortality
country <- country[y>=5]
y <- y[y>=5]


#data 
mydat <- list(J=nJ,
              N=length(y),
              y=y,
              country=country
              )
## Starting values of parameters
    myinit <- function(chain.id=1){
        list(
            sigma_mu_m=runif(1,1.5,2),
            alpha=runif(nJ,0.2,0.3),
            mu_m_tilde=rep(0,nJ),
            sigma_m=runif(nJ,5,15),
            mu_M_tilde=0,
            sigma_M=8.45,
            gamma_M=-0.43
        )
    }

    n.chains <- 4
    init.ll <- lapply(1:n.chains, function(id) myinit(chain.id = id))

##Parameters
mypar <- c("sigma_mu_m","sigma_gamma_m","alpha","mu_m_tilde","sigma_m","gamma_m","mu_M_tilde","sigma_m","gamma_m","mu_M_tilde","sigma_M","gamma_M")

library(rstan)

rstan_options(auto_write = TRUE)
options(mc.cores = 4)

##IT might take a while
fit <- stan(file = 'hierarchical_model.stan', data = mydat, init=init.ll, verbose=TRUE, iter = 6000, chains = n.chains,control = list(adapt_delta = 0.95))

save.image(file="LAMbDA_fit_m.RData")


