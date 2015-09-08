#Loading Packages required for analysis
require(ggplot2)
require(gridExtra)


#Reading Data from the file
yield <- read.table("yield.txt",sep="",header=TRUE) 

#Initial inspection of data
summary(yield)
meansByIrrigation <- aggregate(Yield~Irrigation,yield,mean)


#Creating Plot
yield.irrigation.plot <- ggplot(data=yield,aes(x=factor(Irrigation,labels=c("High","Low")),y=Yield,fill=Irrigation)) + xlab("Irrigation") +
  ylab("Yield (bushels/acre)") + scale_fill_manual(values=c("green","orange"),
                            guide=guide_legend(title="Irrigation"),labels=c("High","Low"))
yield.boxplot <- yield.irrigation.plot + geom_boxplot() +ggtitle("Yield vs. Irrigation") +
  stat_summary(fun.y=mean,color="darkred",geom="point",shape=18,size=5,
               show_guide=FALSE) +
  stat_summary(fun.y=mean,color="black",geom="text",show_guide=FALSE,
               hjust=-0.25,vjust=c(+0.25,+0.15),aes(label=round(..y..,digits=2))) +
  theme_bw() + theme(axis.text=element_text(size=12),
                     axis.title=element_text(size=16),
                     plot.title=element_text(size=18,face="bold"),
                     panel.grid.major=element_blank(),
                     legend.position="none") +
  scale_y_continuous(limits=c(min(yield$Yield),max(yield$Yield)))

yield.density <- ggplot(data=yield,aes(Yield,fill=Irrigation)) + geom_density(alpha=0.5)+ 
  ylab("Density") +
  coord_flip() + scale_fill_manual(values=c("green","orange"),labels=c("High","Low")) +
  theme_bw() + theme(axis.text=element_text(size=12),
                                                    axis.title=element_text(size=16),
                                                    panel.grid.major=element_blank(),
                     axis.title.y=element_blank(),
                     legend.position=c(1,1),legend.justification=c(1,1))

g1 <- ggplotGrob(yield.boxplot)
g2 <- ggplotGrob(yield.density)

maxHeights <- grid::unit.pmax(g1$heights[2:5],g2$heights[2:5])

g1$heights[2:5] <- as.list(maxHeights)
g2$heights[2:5] <- as.list(maxHeights)
blank<-grid::rectGrob(gp=grid::gpar(col="white"))

finalPlot <- arrangeGrob(g1,g2,ncol=2,widths=c(0.6,0.4))
finalPlot2 <- grid.arrange(g1,g2,ncol=2,widths=c(0.7,0.3))

ggsave(filename="yieldBoxplot.pdf",yield.boxplot,width=8,height=5)

ggsave(filename="yieldCombinePlot.pdf",plot=finalPlot,width=8,height=5)

pdf("yieldCombinePlot.pdf",width=11,height=8)
plot(finalPlot)
dev.off()

# Statisitcal model
require(lme4)
options(contrasts=c("contr.sum","contr.sum"))
# Model 1 with interaction effects (Best of all models)
yield.model.lm1 <- lm(Yield~Irrigation*Variety,data=yield)
plot(yield.model.lm1)
summary(yield.model.lm1)

# Model 2 to determine the effect of variety on yields
yield.model.lm2 <- lm(Yield~Variety,data=yield)
plot(yield.model.lm2)
summary(yield.model.lm2)

# Model 3 to determine the effect of irrigation on yields
yield.model.lm3 <- lm(Yield~Irrigation,data=yield)
plot(yield.model.lm3)
summary(yield.model.lm3)

# Model 4 to determine the fixed effects while controlling the variation among fields
yield.model.lmer <- lmer(Yield~Irrigation*Variety+(1|Irrigation),data=yield,REML=TRUE)
yield.model.lmer2 <- update(yield.model.lmer,REML=FALSE)
