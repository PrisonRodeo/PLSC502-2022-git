#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Introduction / Preliminaries                     ####
#
# PLSC 502 -- Fall 2022
#
# Day Three materials: Univariate, bivariate, and
# multivariate graphics...
#
# Load some packages:

library(readr)
# install.packages("car") # <- you may need to do this
library("car")
# install.packages("lattice") # <- this, too...
library(lattice)
# install.packages("fmsb") # <- and this...
library(fmsb)
# install.packages("akima") # <- this, too...
library(akima)
# install.packages("rgl") # <- you get the idea...
library(rgl)

# Set a working directory, if you want (it's a good idea, IMO):
#
# setwd("~/Dropbox (Personal)/PLSC 502/Notes & Slides")

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Univariate graphics...                           ####
#
# Get Africa (2001) data:

Africa<-as.data.frame(read_csv("https://raw.githubusercontent.com/PrisonRodeo/PLSC502-2022-git/master/Data/africa2001.csv"))

# Summary statistics:

summary(Africa)

# Dotchart, population (in millions)

pdf("PopulationDotchartR.pdf",6,5)
with(Africa, dotchart(popthou/1000,pch=19,labels=country,
             cex=0.5,xlab="Population in Millions"))
dev.off()

# Dotchart redux (sorted):

Africa<-Africa[order(Africa$popthou),]

pdf("PopulationDotchartR2.pdf",6,5)
with(Africa, dotchart(popthou/1000,pch=19,labels=country,
                      cex=0.5,xlab="Population in Millions"))
abline(v=c(20,40,60,80,100,120),lty=2,lwd=1)
dev.off()

# Barchart (just like a dotchart...):

pdf("PopulationBarchartR.pdf",6,5)
par(mar=c(4,6,2,2))
with(Africa, barplot(popthou/1000,horiz=TRUE,names.arg=country,
             las=1,cex.names=0.5,xlab="Population in Millions"))
dev.off()

# Histogram, Muslim percentage:

pdf("MuslimPercentHistogramR.pdf",6,5)
par(mar=c(4,4,2,2))
with(Africa, hist(muslperc,breaks=10,col="grey",main=" ",
             xlab="Muslim Percentage Of The Population"))
dev.off()

# Histogram, Sub-saharan:

pdf("SubsaharanHistogramR.pdf",6,5)
par(mar=c(4,4,2,2))
xx<-with(Africa, barplot(table(subsaharan),col="grey",main=" ",
         xlab="Region",ylim=c(0,45),
         beside=TRUE,xpd=FALSE))
# Add Ns to top of bars:
with(Africa, text(xx, table(subsaharan),pos=3,
         labels=paste("N = ",c(table(subsaharan)),sep="")))
dev.off()

# Kernel density:

pdf("MuslimPctKDensityR.pdf",6,5)
par(mar=c(4,4,2,2))
with(Africa, plot(density(muslperc),t="l",lwd=2,main="",
             xlab="Muslim Percentage Of The Population",
             xlim=c(0,100)))
dev.off()

# Overlay histogram and density plot:

pdf("MuslimPctHistoDensityR.pdf",6,5)
par(mar=c(4,4,2,2))
with(Africa, hist(muslperc,breaks=10,col="grey",main=" ",
                  xlab="Muslim Percentage Of The Population",
                  freq=FALSE))
with(Africa, lines(density(muslperc),t="l",lwd=2))
dev.off()

# Density plot, Health Expenditures:

pdf("HealthExpKDensityR.pdf",6,5)
par(mar=c(4,4,2,2))
with(Africa, plot(density(healthexp),t="l",lwd=2,main="",
                  xlab="Health Expenditures, Percent of GDP"))
dev.off()

# Q-Q plot, Health Expenditures:

pdf("HealthExpQQR.pdf",6,5)
par(mar=c(4,4,2,2))
with(Africa, qqnorm(healthexp,main="",
                  ylab="Health Expenditures, Percent of GDP"))
with(Africa, qqline(healthexp,lwd=2))
dev.off()

# Q-Q plot, Muslim Percentage:

pdf("MuslimPctQQR.pdf",6,5)
par(mar=c(4,4,2,2))
with(Africa, qqnorm(muslperc,main="",
                    ylab="Muslim Percentage of the Population"))
with(Africa, qqline(muslperc,lwd=2))
dev.off()

# Boxplots, Muslim percentage:

pdf("MuslimPctBoxplotR.pdf",6,5)
par(mar=c(2,4,2,2))
with(Africa, boxplot(muslperc,main="",
                    ylab="Muslim Percentage of the Population"))
dev.off()

# Boxplots, three variables:

pdf("MultipleBoxplotR.pdf",6,5)
par(mar=c(2,4,2,2))
boxplot(Africa[,c("muslperc","literacy","adrate")],main="",
        ylab="Percentage",names=c("Muslim Pct.","Literacy","HIV Rate"))
dev.off()

# Spider / Radar plots...
# 
# Let's subset the data... first, *just* grab Botswana:

BWA<-Africa[Africa$country %in% c("Botswana"),]

# Subset variables too:

BWA<-BWA[,c("tradegdp","adrate","muslperc","literacy")]

# Now we have to add two additional rows with the minimum and maximum
# values of the variables, as desceribed here:
#
# https://r-graph-gallery.com/142-basic-radar-chart.html

BWA<-rbind(rep(100,4),rep(0,4),BWA)

# Now, draw the radar plot:

pdf("RadarChart1.pdf",6,6)
radarchart(BWA)
dev.off()

# Add a second country... how about Libya?

LBY<-Africa[Africa$country %in% c("Libya"),c("tradegdp","adrate","muslperc","literacy")]

DF<-rbind(BWA,LBY)

# Now a plot with the two countries:

pdf("RadarChart2.pdf",6,6)
radarchart(DF)
legend("topright",col=c("black","red"),pch=19,bty="n",
       legend=c("Botswana","Libya"))
dev.off()


#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Bivariate & Multivariate Plots                   ####
#
# First, scatterplots...
#
# Basic scatterplot:

pdf("MuslimLiteracyScatterplotR.pdf",6,5)
par(mar=c(4,4,2,2))
with(Africa, plot(muslperc,literacy))
dev.off()

# Nicer scatterplot:

pdf("AltMuslimLiteracyScatterplotR.pdf",6,5)
par(mar=c(4,4,2,2))
with(Africa, plot(muslperc,literacy,pch=19,ylab="Adult Literacy Rate",
                  xlab="Muslim Percentage of the Population",
                  ylim=c(10,100)))
with(Africa, text(muslperc,literacy,labels=cabbr,pos=3,cex=0.8))
abline(h=mean(Africa$literacy,na.rm=TRUE),lty=2)
abline(v=mean(Africa$muslperc,na.rm=TRUE),lty=2)
dev.off()

# Skewed data:

pdf("TradeGDPScatterplotR.pdf",6,5)
par(mar=c(4,4,2,2))
with(Africa, plot(gdppppd,tradegdp,pch=19,
                  xlab="GDP Per Capita",ylab="Trade (% GDP)"))
dev.off()

# Skewed data, logged:

pdf("LoggedTradeGDPScatterplotR.pdf",6,5)
par(mar=c(4,4,2,2))
with(Africa, plot(gdppppd,tradegdp,pch=19,log="xy",
                  xlab="Logged GDP Per Capita",
                  ylab="LoggedTrade (% GDP)"))
dev.off()

# Binned data:

pdf("PolityWarScatterplotR.pdf",6,5)
par(mar=c(4,4,2,2))
with(Africa, plot(polity,intensity,pch=19,yaxp=c(0,3,3),
                  xlab="POLITY Score",ylab="Conflict Intensity"))
dev.off()

# Avec jitter:

pdf("JitteredPolityWarScatterplotR.pdf",6,5)
set.seed(7222009)
par(mar=c(4,4,2,2))
with(Africa, plot(polity,jitter(intensity,1),pch=19,yaxp=c(0,3,3),
                  xlab="POLITY Score",ylab="Conflict Intensity"))
dev.off()

# How Not To Draw A Scatterplot:

pdf("SubsaharanCivilWarScatterplotR.pdf",6,5)
par(mar=c(4,4,2,2))
with(Africa, plot(as.numeric(subsaharan)-1,internalwar,pch=19,
                  xaxp=c(0,1,1),yaxp=c(0,1,1),xlab="Region",
                  ylab="Civil War"))
dev.off()

# Frequency tables are better:

with(Africa, xtabs(~subsaharan+internalwar))

# Binary-Continuous:

pdf("PolityCivilWarScatterplotR.pdf",6,5)
par(mar=c(4,4,2,2))
with(Africa, plot(polity,internalwar,pch=19,
                  yaxp=c(0,1,1),xlab="POLITY Score",
                  ylab="Civil War"))
dev.off()

# Add lowess:

pdf("LowessPolityCivilWarScatterplotR.pdf",6,5)
par(mar=c(4,4,2,2))
with(Africa, plot(lowess(polity,internalwar),xlab="POLITY Score",
                  ylab="Civil War",t="l",lwd=2,ylim=c(-0.1,1),
                  xlim=c(-10,10)))
with(Africa, points(polity,internalwar,pch=19))
dev.off()

# Bivariate boxplots:

pdf("SubsaharanHIVBoxplotsR.pdf",6,5)
par(mar=c(4,4,2,2))
with(Africa, boxplot(adrate~subsaharan,xlab="Region",
                     ylab="HIV Pravelence Rate"))
dev.off()

# Multiple conditioned boxplots:

pdf("SubsaharanMultipleBoxplotsR.pdf",9,4)
par(mfrow=c(1,3))
par(mar=c(4,4,2,2))
with(Africa, boxplot(muslperc~subsaharan,xlab="Region",
                     ylab="Muslim Percentage",cex=0.6,
                     main="Muslim Percentage"))
with(Africa, boxplot(literacy~subsaharan,xlab="Region",
                     ylab="Literacy Rate",cex=0.6,
                     main="Literacy"))
with(Africa, boxplot(adrate~subsaharan,xlab="Region",
                     ylab="HIV Prevalence Rate",cex=0.6,
                     main="HIV Rate"))
dev.off()

# QQ-plot comparisons

pdf("HIV-QQ-WarR.pdf",6,5)
par(mar=c(4,4,2,2))
with(Africa, qq(internalwar~adrate, col="black",pch=20,
                xlab="No Internal War", ylab="Internal War"))
dev.off()

# Multivariate plots...
#
# Scatterplot matrix:

pdf("ScatterplotMatrixAfricaR.pdf",6,5)
par(mar=c(4,4,2,2))
dd <- Africa[,c("gdppppd","tradegdp","muslperc",
                "literacy","adrate")]
scatterplotMatrix(dd,reg.line=FALSE,smoother=FALSE,pch=19,
                  var.labels=c("GDP","Trade","Muslim Percent","Literacy","HIV Rate"))
dev.off()

# Conditional scatterplots:

pdf("GDP-HIV-Region-R.pdf",7,5)
par(mfrow=c(1,2)) # <- Create a combined plot: 1 row, 2 columns
par(mar=c(4,4,4,2))
with(Africa[Africa$subsaharan=="Not Sub-Saharan",],
     plot(gdppppd,adrate,pch=19,main="Not Sub-Saharan",log="x",
          xlab="GDP Per Capita",ylab="HIV Prevalence"))
with(Africa[Africa$subsaharan=="Sub-Saharan",],
     plot(gdppppd,adrate,pch=19,main="Sub-Saharan",log="x",
          xlab="GDP Per Capita",ylab="HIV Prevalence"))
dev.off()

# Contour plot (requires akima package):

cpdata <- with(Africa, interp(muslperc,literacy,adrate,
                              duplicate="mean"))

pdf("MuslimLiteracyHIVContourR.pdf",7,5)
par(mar=c(4,4,2,2))
filled.contour(cpdata,color.palette=topo.colors,
               xlab="Muslim Percentage",
               ylab="Literacy")
dev.off()


# "3-D" scatterplot:

pdf("AltMuslimHIVLiteracyScatterR.pdf",6,5)
par(mar=c(2,2,2,2))
cloud(adrate~literacy*muslperc,Africa,col="red",
      pch=20)
dev.off()

# Interactive "3D scatterplot"

with(Africa, plot3d(muslperc,literacy,adrate,
                    size=0.8, col="red",type="s",
                    xlab="Muslim Percentage",
                    ylab="Literacy",zlab="HIV Rate"))
with(Africa, plot3d(muslperc,literacy,adrate,
                    size=1, type="h",xlab="Muslim Percentage",
                    ylab="Literacy",zlab="HIV Rate",add=TRUE)) # (Add lines)

rgl.postscript("InteractiveMuslimLiteracyHIVScatter.pdf",
               fmt="pdf") # (Save the output)

# Multivariate Data display:

Africa$big<-factor(Africa$population>median(Africa$population),
                   labels=c("Big","Small")) # Splitting population at its median
Africa$civilwar<-factor(Africa$internalwar,
                        labels=c("No Civil War","Civil War")) # creating a "factor" variable for civil war

pdf("HIVLiteracySizeCivilWarScatterR.pdf",6,5)
with(Africa, xyplot(adrate~muslperc | civilwar * big,
                    col="black",panel=function(x,y){panel.xyplot(x,y);
                      panel.loess(x,y,span=1)},
                    xlab="Muslim Percentage",ylab="HIV Rate"))
dev.off()

# fin