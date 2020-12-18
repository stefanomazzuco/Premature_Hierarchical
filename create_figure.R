load("LAMbDA_fit_m.RData")
library(rstan)
library(sn)

draws <- extract(fit)
apply(draws$alpha,2,mean)
apply(draws$mu_m,2,mean)
apply(draws$sigma_m,2,mean)

skew <- function(x,param){
    xi <- param[1]
    om <- param[2]
    al <- param[3]
    dsn <- (2/om)*dnorm((x-xi)/om,mean=0,sd=1)*pnorm((al*(x-xi)/om),mean=0,sd=1)
    return(dsn)}


pdf("LaMBDA_fit_m.pdf")
par(mfrow=c(3,3))
for (j in 1:9){
    alpha <- mean(draws$alpha[,j])
    mu.m <- mean(draws$mu_m[,j])
    sigma.m <- mean(draws$sigma_m[,j])
    gamma.m <- 0
    mu.M <- mean(draws$mu_M)
    sigma.M <- mean(draws$sigma_M)
    gamma.M <- mean(draws$gamma_M)

    
    hist(y[country==j],probability=TRUE,breaks=c(5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,95,105,110), col="lightgray",ylim=c(0,0.08), main=countrym[j],xlab="Age",ylab="Death counts",cex.main=1.5,cex.lab=1.5)
    curve(skew(x,cp2dp(c(mu.m,sigma.m,gamma.m),family="sn")),lwd=2,add=TRUE,col="green")
    curve(skew(x,cp2dp(c(mu.M,sigma.M,gamma.M),family="sn")),lwd=2,add=TRUE,col="blue")
    curve(alpha*skew(x,cp2dp(c(mu.m,sigma.m,gamma.m),family="sn"))+(1-alpha)*skew(x,cp2dp(c(mu.M,sigma.M,gamma.M),family="sn")),lwd=2,add=TRUE,col="red")
    legend("topleft", c(paste("Prem., (" ,round(100*alpha),"%)",sep=""),paste("Adult, (" ,round(100*(1-alpha)),"%)",sep=""),"Total"), col=c("green","blue","red"),lty=1,lwd=2, cex=1.2)
}
dev.off()
