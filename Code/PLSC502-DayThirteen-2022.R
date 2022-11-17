#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Intro bits...                                ####
#
# PLSC 502 -- Fall 2022
#
# Day Thirteen materials: Linear Regression
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Packages, etc.:

P<-c("readr","DescTools","psych","car","mvtnorm",
     "plyr","htmltab","L1pack")

for (i in 1:length(P)) {
  ifelse(!require(P[i],character.only=TRUE),install.packages(P[i]),
         print(":)"))
  library(P[i],character.only=TRUE)
}
rm(P)
rm(i)

# Set significant digits:

options(digits=4)

# Also -setwd()- in here if you'd like...
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# First hypothetical scatterplot with regression lines:

set.seed(7222009)
X<-runif(100,-5,5)
Y<-6+X+rnorm(100) # B0=6, B1=1

pdf("VariousRegressionLines.pdf",6,5)
par(mar=c(4,4,2,2))
plot(X,Y,pch=20)
abline(v=0,lty=3,col="grey")
abline(lm(Y~X),lwd=3)
abline(a=8,b=1,lwd=3,lty=2,col="navy")
abline(a=6,b=2,lwd=3,lty=4,col="orange")
dev.off()

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# "World's simplest regression"                ####

x <- c(1,2)
y <- c(3,5)
d <- data.frame(x=x,y=y)
d

pdf("WorldsSimplestRegression.pdf",5,5)
par(mar=c(4,4,2,2))
plot(d,pch=19,xlab="X",ylab="Y",xlim=c(0,3),ylim=c(1,6))
abline(lm(y~x),lwd=1,lty=2,col="grey")
dev.off()

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Court data...                                ####
#
# This is code to aggregate SCOTUS votes from the raw 
# vote data at http://scdb.wustl.edu/data.php:

temp <- tempfile()
download.file("http://scdb.wustl.edu/_brickFiles/2021_01/SCDB_2021_01_justiceCentered_Docket.csv.zip",temp)
SCDB <- read.csv(unz(temp, "SCDB_2021_01_justiceCentered_Docket.csv"))
unlink(temp)
rm(temp)

CivLib<-SCDB[SCDB$issueArea==2,]
CivLib$LibVote<-CivLib$direction-1
Justices <- ddply(CivLib,.(justiceName),summarize,
                  justice = mean(justice),
                  CivLibs = mean(LibVote,na.rm=TRUE)*100)

# Now get and merge the "Segal-Cover" scores:

url<-"https://en.wikipedia.org/wiki/Segal%E2%80%93Cover_score"
SC<-htmltab(doc = url,rm_nodata_cols=FALSE)
rm(url)
SC<-SC[SC$Nominee!="Harlan F. Stone",]
SC<-SC[SC$Nominee!="James F. Byrnes",]
SC<-SC[SC$Nominee!="Clement Haynsworth, Jr.",]
SC<-SC[SC$Nominee!="G. Harrold Carswell",]
SC<-SC[SC$Nominee!="Robert H. Bork",]
SC<-SC[SC$Nominee!="Douglas Ginsburg",]
SC<-SC[SC$Nominee!="Harriet E. Miers",]
SC<-SC[SC$SenateVote!="45 – 43 *",] # Fortas CJ
SC<-SC[SC$SenateVote!="65 – 33",]   # Rehnquist CJ
SC$justice<-as.numeric(SC$Nom.Order)+77
SC$justice<-ifelse(SC$justice>82,SC$justice-1,SC$justice)
SC$justice<-recode(SC$justice,"100=99;103=100;104=101;105=102;
                   106=103;107=104;109=105;112=106;113=107;
                   114=108;115=109;116=110;117=111;119=112;
                   120=113;121=114;122=115;123=116")

# Merge (messily):

SCOTUS<-merge(Justices,SC,by=c("justice"))
SCOTUS$IdeologyScore<-as.numeric(SCOTUS$IdeologyScore)
SCOTUS$Year<-as.numeric(SCOTUS$Year)

describe(SCOTUS,skew=FALSE,trim=0)

pdf("SCOTUSScatterR1.pdf",6,5)
par(mar=c(4,4,2,2))
with(SCOTUS, plot(IdeologyScore,CivLibs,pch=20,xlim=c(-0.1,1.1),
                  xlab="Editorial-Based Liberalism",
                  ylab="Pro-Civil Rights Voting Percentage"))
abline(v=mean(SCOTUS$IdeologyScore),lty=3)
abline(h=mean(SCOTUS$CivLibs),lty=3)
abline(lm(CivLibs~IdeologyScore, SCOTUS),lwd=2)
dev.off()

pdf("SCOTUSScatterR2.pdf",6,5)
par(mar=c(4,4,2,2))
with(SCOTUS, plot(IdeologyScore,CivLibs,pch="",xlim=c(-0.2,1.2),
                  xlab="Editorial-Based Liberalism",
                  ylab="Pro-Civil Rights Voting Percentage"))
with(SCOTUS, text(IdeologyScore,CivLibs,labels=justiceName,cex=0.8))
abline(v=mean(SCOTUS$IdeologyScore),lty=3)
abline(h=mean(SCOTUS$CivLibs),lty=3)
abline(lm(CivLibs~IdeologyScore, SCOTUS),lwd=2)
dev.off()

# Betas:

Beta1 <- with(SCOTUS, (sum((IdeologyScore - mean(IdeologyScore)) * 
                           (CivLibs - mean(CivLibs))) / 
                        sum((IdeologyScore - mean(IdeologyScore))^2)))
Beta1

Beta0 <- with(SCOTUS, mean(CivLibs) - (Beta1 * mean(IdeologyScore)))
Beta0

# Residuals, etc.

SCOTUS$Yhats <- with(SCOTUS, Beta0 + Beta1*IdeologyScore)
SCOTUS$Uhats <- with(SCOTUS, CivLibs - Yhats)

# Y itself:
describe(SCOTUS$CivLibs)

# Predicted Ys:
describe(SCOTUS$Yhats)

# Residuals:
describe(SCOTUS$Uhats)

pdf("SCOTUSYhats.pdf",6,5)
par(mar=c(4,4,2,2))
with(SCOTUS, hist(Yhats,col="grey",main="",
                  breaks=9,xlab="Predicted Y Values"))
dev.off()

pdf("SCOTUSresids.pdf",6,5)
par(mar=c(4,4,2,2))
with(SCOTUS, hist(Uhats,col="grey",main="",
     breaks=16,xlab="Residual Values"))
dev.off()

# Sums of squares:

TotalYVar <- with(SCOTUS, sum((CivLibs - mean(CivLibs))^2))
TotalYVar

TotalUVar <- with(SCOTUS, sum((Uhats)^2))
TotalUVar

TotalModelVar <- with(SCOTUS, sum((Yhats - mean(CivLibs))^2))
TotalModelVar

RSE <- with(SCOTUS, sqrt(TotalUVar / (nrow(SCOTUS)-2)))
RSE
  
# Using lm:

fit<-lm(CivLibs~IdeologyScore,data=SCOTUS)
summary(fit)

anova(fit)

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# The inference bit...                           ####

with(SCOTUS, describe(CivLibs))
with(SCOTUS, describe(IdeologyScore))

# Scatterplot:

pdf("SCOTUSPlot.pdf",7,6) # <- create PDF
par(mar=c(4,4,2,2))
with(SCOTUS, plot(IdeologyScore,CivLibs,pch=19,
                  xlab="Segal-Cover Liberalism Score",
                  ylab="Voting Liberalism"))
with(SCOTUS, abline(v=mean(IdeologyScore,na.rm=TRUE),lty=2))
with(SCOTUS, abline(h=mean(CivLibs,na.rm=TRUE),lty=2))
dev.off()

# Regression:

SCLib<-lm(CivLibs~IdeologyScore,data=SCOTUS)
summary(SCLib)   # regression
anova(SCLib)     # ANOVA

# Other things:

vcov(SCLib)
confint(SCLib)
confint(SCLib,level=0.99)
SEs<-predict(SCLib,interval="confidence")
SEs

# Plot:

Sort<-order(SCOTUS$IdeologyScore)

pdf("SCLib-CI.pdf",6,5)
par(mar=c(4,4,2,2))
plot(SCOTUS$IdeologyScore,SCOTUS$CivLibs,pch=20,
     xlab="Segal-Cover Ideology Score",
     ylab="Voting Liberalism")
abline(SCLib,lwd=3)
lines(sort(SCOTUS$IdeologyScore),SEs[Sort,2],col="red",lwd=2,lty=2)
lines(sort(SCOTUS$IdeologyScore),SEs[Sort,3],col="red",lwd=2,lty=2)
legend("topleft",bty="n",lty=c(1,2),lwd=2,col=c("black","red"),
       legend=c("Fitted Line","95 percent C.I.s"))
dev.off()

# /fin