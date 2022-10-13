#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Intro materials                              ####
#
# PLSC 502 -- Fall 2022
#
# Day Eight materials: Estimation
#
# Load some packages: 

library(readr)
library(htmltab)
library(psych)
library(MASS)

# Also: setwd() here someplace...
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Efficiency plot example                      ####

x <- seq(-4,4,by=0.01)
fat<-dnorm(x,0,2)
thin<-dnorm(x,0,1)

pdf("EfficiencyPicR.pdf",6,5)
par(mar=c(2,2,2,2))
plot(x,thin,t="l",lwd=2,lty=1,xaxt="n",yaxt="n",xlab="",
     ylab="")
lines(x,fat,lwd=2,lty=2)
arrows(x[450],thin[450],x[450]+2,thin[450],lwd=2,length=0.1,code=1)
text(x[450]+2.3,thin[450],labels=expression(paste(hat(theta)[1])))
arrows(x[600],fat[600],x[600]+1,fat[600]+0.1,lwd=2,length=0.1,code=1)
text(x[600]+1.3,fat[600]+0.1,labels=expression(paste(hat(theta)[2])))
axis(1,at=0,labels=expression(paste(theta)))
abline(v=0,lty=3,lwd=1)
legend(-5,0.42,bty="n",col="white",lty=c(1),cex=0.8,
       legend=c(expression(paste(hat(theta)[1])~" is more efficient than "~
       paste(hat(theta)[2]))))
dev.off()

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# MSE / "6" plot...                            ####

x<-seq(4,8,by=0.01)
MSEsix <- 36-12*x+x^2
MSEMean20 <- 10/20
MSEMean100 <- 10/100
MSEMean1000 <- 10/1000

pdf("VariousMSEsR.pdf",6,5)
par(mar=c(4,4,2,2))
plot(x,MSEsix,t="l",lwd=2,xlab=expression(mu),
     ylab="MSE")
abline(h=MSEMean20,col="red",lty=2,lwd=2)
abline(h=MSEMean100,col="darkgreen",lty=3,lwd=2)
abline(h=MSEMean1000,col="blue",lty=4,lwd=2)
legend(6,4,bty="n",lty=c(1,2,3,4),lwd=2,
       col=c("black","red","darkgreen","blue"),
       legend=c(expression(paste(zeta,"=6")),"Mean (N=20)",
                "Mean (N=100)","Mean (N=1000)"))
dev.off()

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Consistency...                               ####

x <- seq(-4,4,by=0.01)
fat<-dnorm(x,-1.5,2)
med<-dnorm(x,-0.75,1.3)
thin<-dnorm(x,0,1)

pdf("ConsistencyPicR.pdf",6,5)
par(mar=c(2,2,2,2))
plot(x,thin,t="l",lwd=2,lty=1,xaxt="n",yaxt="n",xlab="",
     ylab="")
lines(x,med,lwd=2,lty=2)
lines(x,fat,lwd=2,lty=3)
arrows(x[450],thin[450],x[450]+2,thin[450],lwd=2,length=0.1,code=1)
text(x[450]+2.8,thin[450],labels="N=1000")
arrows(x[250],med[250],x[250]-0.5,med[250]+0.1,lwd=2,length=0.1,code=1)
text(x[250]-0.5,med[250]+0.12,labels="N=100")
arrows(x[150],fat[150],x[150]-0.5,fat[150]+0.1,lwd=2,length=0.1,code=1)
text(x[150]-0.5,fat[150]+0.12,labels="N=10")
axis(1,at=0,labels=expression(paste(theta)))
abline(v=0,lty=3,lwd=1)
dev.off()

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Estimation example 1: simulation             ####
#
# The \lambdas:

L<-c(0.2,1,8)

# The Ns:

N<-c(5,50,500)

# We'll do 4000 simulations of each combination of 
# values for \lambda and N. So:

sims<-4000  # We'll simulate each combination of N and
            # \lambda 4000 times...

# Now build a data frame to keep our simulation results
# in. We'll need 3x3=9 columns and 4000 rows for that:

Out<-data.frame(matrix(nrow=sims,ncol=length(N)*length(L)))
colnames(Out) <- c("M0.2.5","M1.5","M8.5","M0.2.50","M1.50",
                   "M8.50","M0.2.500","M1.500","M8.500")

# Now, set a seed and write some loops:

c <- 0            # column indicator for "Out"
set.seed(7222009) # Seediness

for(i in 1:length(N)) {    # Looping over sample sizes...
  for(j in 1:length(L)) {  # Looping over lambdas
    c <- c+1               # increment column indicator
    for(k in 1:sims) {     # Looping over 4000 simulations each
      df<-rpois(N[i],L[j]) # Draw N values from Poisson(lambda)
      Out[k,c]<-mean(df)   # Store the mean of the N draws
      rm(df)
    } 
  }
}

# Now let's visualize those:

pdf("PoissonMeanSims.pdf",10,7)
par(mfrow=c(3,3))
c<-0
for(i in 1:length(N)){
  for(j in 1:length(L)) {
    c<-c+1
    Label<-paste0("Lambda=",L[j],", N=",N[i])
    plot(density(Out[,c]),lwd=2,xlim=c(0,15),
         main=Label,xlab="Means of X")
    abline(v=L[j],lwd=1,lty=2,col="red")
  }
}
dev.off()

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Estimation example 2: "real" data            ####

# Get current Peremier League data:

url<-"https://www.skysports.com/premier-league-table"
PL<-htmltab(doc = url)
PL[,c(3:10)]<-apply(PL[,c(3:10)],2,
                    function(x) as.numeric(x)) # make data numeric

# Rename some variables:

colnames(PL)<-c("Rank","Team","GamesPlayed","Won","Drew","Lost",
                "GoalsFor","GoalsAgainst","GoalDifference","Points")

# Zap URL:

rm(url)

# Print:

PL

# Describe the data:

psych::describe(PL)

# Now, let's fit a Poisson distribution to these data.
# Recall that the Poisson has one parameter, often
# called \lambda, that determines the shape and 
# location of the distribution.
#
# We can use -fitdistr- in the MASS package to fit parametric
# distributions to data. That looks like this:

PoisMean <- fitdistr(PL$Drew,"poisson")
PoisMean

# Components:

coef(PoisMean)
vcov(PoisMean)

# and note that the latter is the same as:

coef(PoisMean) / nrow(PL)

# Finally, here's a histogram of the frequency of 
# draws, along with an overlay of a Poisson density 
# with \lambda equal to the estimated empirical mean 
# of the data:

pdf("PL-Draws-Barplot.pdf",6,5)
par(mar=c(4,4,2,2))
p<-with(PL, barplot(table(Drew),col="grey",main="",xlim=c(0,7),
                    ylim=c(0,6),xlab="Number of Draws",ylab="Frequency"))
par(new=TRUE)
plot(p,dpois(seq(0,5),lambda=coef(PoisMean)),xlim=c(0,7),
     pch=19,col="red",xlab="",ylab="",xaxt="n",yaxt="n")
segments(p,0,p,dpois(seq(0,5),lambda=mean(PL$Drew)),
         lwd=2,lty=2,col="red")
dev.off()

# \fin