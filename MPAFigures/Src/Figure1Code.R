library(dplyr)
library(ggplot2)
library(tidyr)
#install.packages("RColorBrewer")
library(RColorBrewer)

rm(list = ls())

data<- read.csv("Data/Clean/MPA Data Modified.csv")


#####Ecology#####

Ecology<- subset(data, Category=='Ecology')
df<- Ecology%>%
  group_by(Indicator, Score, Category) %>%
  summarise(Count = n())
df

#write.csv(df,"Data/Compiled/df.csv")

Ecology.df <- read.csv("Data/Compiled/df.csv")
Indicator.2 <- c("Habitat quality in protected area", "Habitat quality outside protected area",
                 "Non-target abundance/biomass outside protected area",
                 "Non-target abundance/biomass in protected area",
                 "Target abundance/biomass outside protected area",
                 "Target abundance/biomass in protected area")

Main.2 <- c("Habitat quality \ninside", "Habitat quality \noutside",
                 "Non-target abundance or \nbiomass outside",
                 "Non-target abundance or \nbiomass inside",
                 "Target abundance or \nbiomass outside",
                 "Target abundance or\nbiomass inside")
brewer.pal(n = 6, name = "Blues")
brewer.pal(n = 8, name = "Reds")



#####Economics#####

Economics<- subset(data, Category=='Economics')
df.Econ<- Economics%>%
  group_by(Indicator, Score, Category) %>%
  summarise(Count = n())
df
brewer.pal(n = 6, name = "Greens")
###write.csv(df.Econ,"Data/Compiled/df.Econ.csv")

Econ.df <- read.csv("Data/Compiled/df.Econ.csv")
Indicator.3 <- c("Anything about processing", "Catch per vessel/fisherman (new 2/14)",
                 "Costs","CPUE (new 2/21)","Ex vessel price","Net Revenue","No. vessels",
                 "Total Catch","Total Revenue","Wholesale prices")

Main.3 <- c("Anything about processing", "Catch per vessel/fisherman",
                 "Costs","CPUE","Ex vessel price","Net Revenue","No. vessels",
                 "Total Catch","Total Revenue","Wholesale prices")


#####Community#####

Community<- subset(data, Category=='Community')
df.Com<- Community%>%
  group_by(Indicator, Score, Category) %>%
  summarise(Count = n())
df.Com

###write.csv(df.Com,"Data/Compiled/df.Com.csv")

Com.df <- read.csv("Data/Compiled/df.Com.csv")
Indicator.4 <- c("Employment", "Food security",
                 "Women's involvement","Working Conditions/safety")



#####Multipanel#####

mat<- matrix(c(21,21,21,21,21,21,
              1,2,3,4,5,6,
              22,22,22,22,22,22,
              7,8,9,10,11,0,
              12,13,14,15,16,0,
              23,23,23,23,23,23,
              17,18,19,20,24,0),ncol=6,nrow=7, byrow=TRUE)
par(mar=c(4,1,1,1))

layout(mat,heights = c(0.5,8,0.5,8,8,0.5,8))
#layout.show(n = 23)

pdf(file = "Results/Plot.pdf",   # The directory you want to save the file in
    width = 11, # The width of the plot in inches
    height = 8)
par(mar=c(3,5,2,1))
layout(mat,heights = c(1,4,1,4,4,1,4))
for (j in 1:6){
  df.2 <- filter(Ecology.df, Indicator == Indicator.2[j])
  barplot(df.2$Count, main= Main.2[j],
          xlab="Score", ylim = c(0,20), names.arg=c("0", "1", "2","3", "4", "5"),
          col=c("#FB6A4A",brewer.pal(n = 6, name = "Blues")),
          space=c(0.05), ylab="Count", cex.main=1)
}

for (j in 1:length(Indicator.3)){
  df.2 <- filter(Econ.df, Indicator == Indicator.3[j])
  barplot(df.2$Count, main= Main.3[j],
          xlab="Score", ylim = c(0,20), names.arg=c("0", "1", "2","3", "4", "5"),
          col=c("#FB6A4A", brewer.pal(n = 6, name = "Greens")),
          space=c(0.05), ylab="Count", cex.main=1)
}

for (j in 1:length(Indicator.4)){
  df.2 <- filter(Com.df, Indicator == Indicator.4[j])
  barplot(df.2$Count, main= Indicator.4[j],
          xlab="Score", ylim = c(0,20), names.arg=c("0", "1", "2","3", "4", "5"),
          col=c("#FB6A4A", brewer.pal(n = 6, name = "Purples")),
          space=c(0.05), ylab="Count", cex.main=1)
}
par(mar=c(1,1,1,1))

plot(0, xaxt = 'n', yaxt = 'n', bty = 'n', pch = '', ylab = '', xlab = '', ylim=c(0,10), xlim=c(0,10))
text(5,5,"Ecology", cex=1.5, font=2, col=brewer.pal(n = 6, name = "Blues")[6])
plot(0, xaxt = 'n', yaxt = 'n', bty = 'n', pch = '', ylab = '', xlab = '', ylim=c(0,10), xlim=c(0,10))
text(5,5,"Economics", cex=1.5, font=2, col=brewer.pal(n = 6, name = "Greens")[6])
plot(0, xaxt = 'n', yaxt = 'n', bty = 'n', pch = '', ylab = '', xlab = '', ylim=c(0,10), xlim=c(0,10))
text(5,5,"Community", cex=1.5, font=2, col=brewer.pal(n = 6, name = "Purples")[6])

plot(0, xaxt = 'n', yaxt = 'n', bty = 'n', pch = '', ylab = '', xlab = '', ylim=c(0,12), xlim=c(0,10))
text(5,11,"Scale", cex=1.5, font=2)
text(5,9,"5 = ", cex=1, font=2)
text(5,7.5,"4 = ", cex=1, font=2)
text(5,6,"3 = ", cex=1, font=2)
text(5,4.5,"2 = ", cex=1, font=2)
text(5,3,"1 = ", cex=1, font=2)
text(5,1.5,"0 = Not Reported", cex=1, font=2)

par(xpd=NA)
rect( grconvertX(0.005, from='ndc'), grconvertY(0.995, from='ndc'),
      grconvertX(0.995, from='ndc'), grconvertY(0.745, from='ndc'),  border=brewer.pal(n = 6, name = "Blues")[6], lwd=2)

rect( grconvertX(0.005, from='ndc'), grconvertY(0.74, from='ndc'),
      grconvertX(0.995, from='ndc'), grconvertY(0.26, from='ndc'),  border=brewer.pal(n = 6, name = "Greens")[6], lwd=2)


rect( grconvertX(0.005, from='ndc'), grconvertY(0.005, from='ndc'),
      grconvertX(0.995, from='ndc'), grconvertY(0.255, from='ndc'), border=brewer.pal(n = 6, name = "Purples")[6], lwd=2)
dev.off()
