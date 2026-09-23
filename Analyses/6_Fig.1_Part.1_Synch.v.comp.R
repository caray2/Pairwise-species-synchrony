#This code produces a plot that demonstrates the difference between synchronous and compensatory dynamics

library(ggplot2)
library(ggtext)
library(dplyr)
library(gridExtra)
library(grid)
library(here)

set.seed(7220)

#Establish file location
i_am("Analyses/6_Fig.1_Part.1_Synch.v.comp.R")

#Create an example of synchronous versus compensatory dynamics
#First generate a compensatory plot
x <- seq(-2*pi,2*pi,by=0.05)
sin.x <- sin(x) #Values now oscillate between 1 and -1
neg.sin.x <- -sin(x)  #Create a oscillating line that is opposite of sin(x) for compensatory pattern
df.sin <-as.data.frame(cbind(x, sin.x)) 
df.sin$func <-'sin'
df.neg.sin <-as.data.frame(cbind(x, neg.sin.x))
df.neg.sin$func <-'neg.sin'

#Unify header names before joining
colnames(df.sin)[which(names(df.sin) == "sin.x")] <- "func.value"
colnames(df.neg.sin)[which(names(df.neg.sin) == "neg.sin.x")] <- "func.value"
Compensatory <-full_join(df.sin, df.neg.sin, by = join_by(x, func.value, func))

#Plot
Comp.D <-
  Compensatory  %>%
  ggplot( aes(x=x, y=func.value, group=func, color=func)) +
  geom_line(linewidth=2) +
  theme_bw(base_size = 16) + 
  theme(legend.position="none", 
        plot.title = element_markdown(size = 18),
        plot.margin = unit(c(0.2, 0.5, 0.5, 0.5), "cm"),
        axis.text.x=element_blank(),
        axis.text.y=element_blank()) +
  ggtitle("<span style='color:#a6611a;'>**Compensatory**</span>") +
  ylab(NULL) +
  xlab(NULL) +
  scale_color_manual(values=c("#543005","#dfc27d"))

#Make a synchronous plot
df.synchrony <- df.sin # duplicate the sin wave we made above
df.synchrony$func.value <- df.synchrony$func.value - 0.4 # add an offset
df.synchrony$func <- 'offset'
Synchronous <-rbind(df.synchrony, df.sin)

Synchrony <-
  Synchronous  %>%
  ggplot( aes(x=x, y=func.value, group=func, color=func)) +
  geom_line(linewidth=2) +
  theme_bw(base_size = 16) + 
  theme(legend.position="none", 
        plot.title = element_markdown(size = 18),
        plot.margin = unit(c(0.2, 0.5, 0.5, 0.5), "cm"),
        axis.text.x=element_blank(),
        axis.text.y=element_blank()) +
  ggtitle("<span style='color:#018571;'>**Synchronous**</span>") +
  ylab(NULL) +
  xlab(NULL) +
  scale_color_manual(values=c("#003c30","#80cdc1")) 

#Plot the compensatory and synchronous plots together with centered labels
full.dyn.ann <- arrangeGrob(Synchrony, Comp.D, ncol = 1,
                            left = textGrob("Abundance", rot = 90, gp = gpar(fontsize = 18)),
                            bottom = textGrob("Time", gp = gpar(fontsize = 18)))

#Display
grid.arrange(full.dyn.ann)

#Save graphic - must use png() or pdf() device for grid objects
setwd("H:/My Drive/Synchrony/Graphics")
png("Fig.1A_Syn.vs.Comp.png", height=4, width=5, units="in", res=500)
grid.draw(full.dyn.ann)
dev.off()

#Save as an RDS
setwd("H:/My Drive/Synchrony/Graphics/RDS.plots")
saveRDS(full.dyn.ann, "Figure.1A.rds")