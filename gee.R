library(ggplot2)
library(readxl)
library(lme4)
library(lmerTest)
library(grid)
library(lsmeans)
library(cowplot)
library(geepack)
set.seed(123)
options(scipen=999)

plotData<- function(dTemp){
  # Covariance structure at the 2nd level... Look at Intercept variability and TDD variabilities
  
  mdf<-aggregate(PROD ~ TYPE_SUBTASK + OVERHEAD + ORDER , FUN=mean, data=dTemp)
  print(mdf)
  ggplot(data=mdf, aes(x=as.factor(OVERHEAD), y=PROD, group = ORDER, colour = ORDER)) +
    geom_line() +   geom_point( size=4, shape=21, fill="white")+ 
    labs(x="TYPE_SUBTASK", y="LIKERT")  + facet_grid(. ~ TYPE_SUBTASK) + ylim(1,5) + theme_gray()
}


plotDataByExperiment<- function(dTemp){
  # Covariance structure at the 2nd level... Look at Intercept variability and TDD variabilities
  
  mdf<-aggregate(PROD ~ TYPE_SUBTASK + OVERHEAD + ORDER + EXPERIMENT, FUN=mean, data=dTemp)
  print(mdf)
  ggplot(data=mdf, aes(x=as.factor(OVERHEAD), y=PROD, group = ORDER, colour = ORDER)) +
    geom_line() +   geom_point( size=4, shape=21, fill="white")+ 
    labs(x="TYPE_SUBTASK", y="LIKERT")  + facet_grid(EXPERIMENT ~ TYPE_SUBTASK) + ylim(1,5) + theme_gray()
}

plotDataByExperiment(d)
plotData(d)

ICClmer <- function(model){
  varests <- as.data.frame(VarCorr(model))$vcov
  return(paste("ICC =",varests[1]/sum(varests)))
}
ICClmer3Levels <- function(model){
  dfTemp<-as.data.frame(VarCorr(model))
  var_SUBJECT<-dfTemp$vcov[1]
  var_SITE<-dfTemp$vcov[2]
  var_residual<-dfTemp$vcov[3]
  ### ICC for level 2
  ICC_l2 <- (var_SITE + var_SUBJECT)/(var_SITE + var_SUBJECT + var_residual)
  ### ICC for level 3
  ICC_l3 <- var_SITE/(var_SITE + var_SUBJECT + var_residual) 
  return(paste("ICC SITE=",ICC_l3,"; ICC SUBJECT_ID",ICC_l2 ))
}

# 4 different plots
plotsModel <-function(df){
  # # Divide by captive-continuous, continuous-captive
  # dfPlot0 <- subset(df, OVERHEAD==0)
  # dfPlot1 <- subset(df, OVERHEAD==20)
  # dfPlot2 <- subset(df, OVERHEAD==80)
  # dfPlot3 <- subset(df, OVERHEAD==140)
  # 
  # p0<- ggplot(dfPlot0, aes(x=TYPE_SUBTASK, y=lsmean, group=ORDER)) + ggtitle("Overhead 0") +
  #   geom_line(aes(color=ORDER))+ geom_point(aes(color=ORDER)) + ylab("Likert") + xlab("Type Subtask") + scale_y_continuous(limits=c(1,5), breaks=c(1,2,3,4,5), labels=c("Too slow","Slower than normal","Normal","Faster than normal","Too fast")) + theme_bw() 
  # 
  # legend <- get_legend(p0)  
  # 
  # p0<- p0 + theme(legend.position="none")  
  # 
  # 
  # p1<- ggplot(dfPlot1, aes(x=TYPE_SUBTASK, y=lsmean, group=ORDER)) + ggtitle("Overhead 20") +
  #   geom_line(aes(color=ORDER))+ geom_point(aes(color=ORDER)) + ylab("Likert") + xlab("Type Subtask") + scale_y_continuous(limits=c(1,5), breaks=c(1,2,3,4,5), labels=c("Too slow","Slower than normal","Normal","Faster than normal","Too fast")) + theme_bw() + theme(legend.position="none")
  # 
  # p2<- ggplot(dfPlot2, aes(x=TYPE_SUBTASK, y=lsmean, group=ORDER)) + ggtitle("Overhead 80") +
  #   geom_line(aes(color=ORDER))+ geom_point(aes(color=ORDER)) + ylab("Likert") + xlab("Type Subtask") + scale_y_continuous(limits=c(1,5), breaks=c(1,2,3,4,5), labels=c("Too slow","Slower than normal","Normal","Faster than normal","Too fast")) + theme_bw() + theme(legend.position="none")
  # 
  # p3<- ggplot(dfPlot3, aes(x=TYPE_SUBTASK, y=lsmean, group=ORDER)) + ggtitle("Overhead 140") +
  #   geom_line(aes(color=ORDER))+ geom_point(aes(color=ORDER)) + ylab("Likert") + xlab("Type Subtask") + scale_y_continuous(limits=c(1,5), breaks=c(1,2,3,4,5), labels=c("Too slow","Slower than normal","Normal","Faster than normal","Too fast")) + theme_bw() + theme(legend.position="none")
  # 
  # 
  # p<-plot_grid(p0,p1,p2,p3, ncol=4)
  # 
  # plot(plot_grid(p,legend, rel_widths = c(0.9,0.1)))
  # 
  # # Divide by Type-subtask
  # dfPlot0 <- subset(df, OVERHEAD==0)
  # dfPlot1 <- subset(df, OVERHEAD==20)
  # dfPlot2 <- subset(df, OVERHEAD==80)
  # dfPlot3 <- subset(df, OVERHEAD==140)
  # 
  # p0<- ggplot(dfPlot0, aes(x=ORDER, y=lsmean, group=TYPE_SUBTASK)) + ggtitle("Overhead 0") +
  #   geom_line(aes(color=TYPE_SUBTASK))+ geom_point(aes(color=TYPE_SUBTASK)) + ylab("Likert") + xlab("Order") + scale_y_continuous(limits=c(1,5), breaks=c(1,2,3,4,5), labels=c("Too slow","Slower than normal","Normal","Faster than normal","Too fast")) + theme_bw()
  # 
  # 
  # legend <- get_legend(p0)  
  # 
  # p0<- p0 + theme(legend.position="none")  
  # 
  # 
  # p1<- ggplot(dfPlot1, aes(x=ORDER, y=lsmean, group=TYPE_SUBTASK)) + ggtitle("Overhead 20") +
  #   geom_line(aes(color=TYPE_SUBTASK))+ geom_point(aes(color=TYPE_SUBTASK)) + ylab("Likert") + xlab("Order") + scale_y_continuous(limits=c(1,5), breaks=c(1,2,3,4,5), labels=c("Too slow","Slower than normal","Normal","Faster than normal","Too fast")) + theme_bw() + theme(legend.position="none")
  # 
  # p2<- ggplot(dfPlot2, aes(x=ORDER, y=lsmean, group=TYPE_SUBTASK)) + ggtitle("Overhead 80") +
  #   geom_line(aes(color=TYPE_SUBTASK))+ geom_point(aes(color=TYPE_SUBTASK)) + ylab("Likert") + xlab("Order") + scale_y_continuous(limits=c(1,5), breaks=c(1,2,3,4,5), labels=c("Too slow","Slower than normal","Normal","Faster than normal","Too fast")) + theme_bw() + theme(legend.position="none")
  # 
  # p3<- ggplot(dfPlot3, aes(x=ORDER, y=lsmean, group=TYPE_SUBTASK)) + ggtitle("Overhead 140") +
  #   geom_line(aes(color=TYPE_SUBTASK))+ geom_point(aes(color=TYPE_SUBTASK)) + ylab("Likert") + xlab("Order") + scale_y_continuous(limits=c(1,5), breaks=c(1,2,3,4,5), labels=c("Too slow","Slower than normal","Normal","Faster than normal","Too fast")) + theme_bw() + theme(legend.position="none")
  # 
  # 
  # p<-plot_grid(p0,p1,p2,p3, ncol=4)
  # 
  # plot(plot_grid(p,legend, rel_widths = c(0.9,0.1)))
  # 
  # 
  # # Divide by Type-subtask
  # dfPlot0 <- subset(df, TYPE_SUBTASK=="Instantaneous")
  # dfPlot1 <- subset(df, TYPE_SUBTASK=="Immediate")
  # dfPlot2 <- subset(df, TYPE_SUBTASK=="Continuous")
  # dfPlot3 <- subset(df, TYPE_SUBTASK=="Captive")
  # 
  # 
  # p0<- ggplot(dfPlot0, aes(x=ORDER, y=lsmean, group=OVERHEAD)) + ggtitle("Instantaneous") +
  #   geom_line(aes(color=OVERHEAD))+ geom_point(aes(color=OVERHEAD)) + ylab("Likert") + xlab("Order") + scale_y_continuous(limits=c(1,5), breaks=c(1,2,3,4,5), labels=c("Too slow","Slower than normal","Normal","Faster than normal","Too fast")) + theme_bw()
  # 
  # 
  # legend <- get_legend(p0)  
  # 
  # p0<- p0 + theme(legend.position="none")  
  # 
  # 
  # p1<- ggplot(dfPlot1, aes(x=ORDER, y=lsmean, group=OVERHEAD)) + ggtitle("Immediate") +
  #   geom_line(aes(color=OVERHEAD))+ geom_point(aes(color=OVERHEAD)) + ylab("Likert") + xlab("Order") + scale_y_continuous(limits=c(1,5), breaks=c(1,2,3,4,5), labels=c("Too slow","Slower than normal","Normal","Faster than normal","Too fast")) + theme_bw() + theme(legend.position="none")
  # 
  # p2<- ggplot(dfPlot2, aes(x=ORDER, y=lsmean, group=OVERHEAD)) + ggtitle("Continuous") +
  #   geom_line(aes(color=OVERHEAD))+ geom_point(aes(color=OVERHEAD)) + ylab("Likert") + xlab("Order") + scale_y_continuous(limits=c(1,5), breaks=c(1,2,3,4,5), labels=c("Too slow","Slower than normal","Normal","Faster than normal","Too fast")) + theme_bw() + theme(legend.position="none")
  # 
  # p3<- ggplot(dfPlot3, aes(x=ORDER, y=lsmean, group=OVERHEAD)) + ggtitle("Captive") +
  #   geom_line(aes(color=OVERHEAD))+ geom_point(aes(color=OVERHEAD)) + ylab("Likert") + xlab("Order") + scale_y_continuous(limits=c(1,5), breaks=c(1,2,3,4,5), labels=c("Too slow","Slower than normal","Normal","Faster than normal","Too fast")) + theme_bw() + theme(legend.position="none")
  # 
  # 
  # p<-plot_grid(p0,p1,p2,p3, ncol=4)
  # 
  # plot(plot_grid(p,legend, rel_widths = c(0.9,0.1)))
  
  #########################
  dfPlot0 <- subset(df, ORDER=="Captive - Continuous")
  dfPlot1 <- subset(df, ORDER=="Continuous - Captive")
  
  
  p0<- ggplot(dfPlot0, aes(x=OVERHEAD, y=lsmean, group=TYPE_SUBTASK)) + 
    ggtitle("Captive - Continuous") +
    geom_line(aes(color=TYPE_SUBTASK)) + 
    geom_point(aes(color=TYPE_SUBTASK), size=2, stroke = 3) + 
    ylab("Likert") + xlab("Overhead Level [%]") + 
    scale_y_continuous(limits=c(1,5), breaks=c(1,2,3,4,5), labels=c("Too slow","Slower than normal","Normal","Faster than normal","Too fast")) + theme_bw(base_size=28) + labs(color = "Operation Category") 
  
  legend <- get_legend(p0 + theme(legend.position = "top",legend.text=element_text(size=27)))
  p0<- p0 + theme(legend.position="none") 
  
  # legend <- get_legend(p0 + theme(legend.position = "top",legend.text=element_text(size=27)))
  
  p1<- ggplot(dfPlot1, aes(x=OVERHEAD, y=lsmean, group=TYPE_SUBTASK)) + ggtitle("Continuous - Captive") +
    geom_line(aes(color=TYPE_SUBTASK))+  geom_point(aes(color=TYPE_SUBTASK), size=2, stroke = 3) + ylab("Likert") + xlab("Overhead Level [%]") + scale_y_continuous(limits=c(1,5), breaks=c(1,2,3,4,5), labels=c("Too slow","Slower than normal","Normal","Faster than normal","Too fast")) + theme_bw(base_size=28) + 
    theme(legend.position="none") 
  
  
  p<-plot_grid(p0,p1, ncol=2)
  png(file = "/Users/cornejo/Desktop/1.png",width = 1400,height = 600)
  plot(plot_grid(p, legend, ncol = 1, rel_heights = c(1,0.1)))
  dev.off()
  
  #####################################################
  
  dfPlot0 <- subset(df, ORDER=="Captive - Continuous")
  dfPlot1 <- subset(df, ORDER=="Continuous - Captive")
  
  pd <- position_dodge(0.05) # move them .05 to the left and right
  
  p0<- ggplot(dfPlot0, aes(x=TYPE_SUBTASK, y=lsmean, group=OVERHEAD, label=OVERHEAD)) + 
    ggtitle("Captive - Continuous") + 
    geom_text(aes(label=OVERHEAD),hjust=-0.6, vjust=-0.3,size=6) +
    geom_point(aes(color=OVERHEAD), size=3.5, stroke = 2,position=pd) + ylab("Likert") + 
    # geom_errorbar(aes(ymin=lsmean-SE, ymax=lsmean+SE, color=OVERHEAD), width=.7,position=pd)+
    xlab("Operation Category") + 
    scale_y_continuous(limits=c(1,5), breaks=c(1,2,3,4,5), labels=c("Too slow","Slower than normal","Normal","Faster than normal","Too fast")) + 
    theme_bw(base_size=28) + scale_color_manual(values=c("#00B300", "#FFD11A", "#FF9900", "#FF0000")) + labs(color = "Overhead Level [%]") 
  
  legend <- get_legend(p0 + theme(legend.position = "top",legend.text=element_text(size=27)))
  p0<- p0 + theme(legend.position="none", axis.text.x = element_text(angle = 20, hjust = 1)) 
  
  p1<- ggplot(dfPlot1, aes(x=TYPE_SUBTASK, y=lsmean, group=OVERHEAD, label=OVERHEAD)) + 
    ggtitle("Continuous - Captive") + 
    geom_text(aes(label=OVERHEAD),hjust=-0.6, vjust=0.3,size=6) +
    geom_point(aes(color=OVERHEAD), size=3.5, stroke = 2,position=pd) + ylab("Likert") + 
    # geom_errorbar(aes(ymin=lsmean-SE, ymax=lsmean+SE, color=OVERHEAD), width=.7,position=pd)+
    xlab("Operation Category") + 
    scale_y_continuous(limits=c(1,5), breaks=c(1,2,3,4,5), labels=c("Too slow","Slower than normal","Normal","Faster than normal","Too fast")) + 
    theme_bw(base_size=28) + scale_color_manual(values=c("#00B300", "#FFD11A", "#FF9900", "#FF0000"))
  p1<- p1 + theme(legend.position="none",axis.text.x = element_text(angle = 20, hjust = 1))  
  
  p<-plot_grid(p0,p1, ncol=2)
  
  png(file = "/Users/cornejo/Desktop/2.png",width = 1300,height = 600)
  plot(plot_grid(p, legend, ncol = 1, rel_heights = c(1,0.1)))
  dev.off()
  ############################
  
  
  # Divide by Type-subtask
  dfPlot0 <- subset(df, TYPE_SUBTASK=="Continuous")
  dfPlot1 <- subset(df, TYPE_SUBTASK=="Captive")
  
  pd <- position_dodge(0.10) # move them .05 to the left and right
  
  p0<- ggplot(dfPlot0, aes(x=OVERHEAD, y=lsmean, group=ORDER)) + ggtitle("Continuous") +
    geom_line(aes(color=ORDER),position=pd) + 
    geom_point(aes(color=ORDER), size=2, stroke = 2,position=pd) + 
    # geom_errorbar(aes(ymin=lsmean-SE, ymax=lsmean+SE, color=ORDER), width=.2,position=pd)+
    ylab("Likert") + xlab("Overhead Level [%]") + scale_y_continuous(limits=c(1,5), breaks=c(1,2,3,4,5), labels=c("Too slow","Slower than normal","Normal","Faster than normal","Too fast")) + theme_bw(base_size=21) + labs(color = "Op. Category Order")
  
  legend <- get_legend(p0 + theme(legend.position = "top",legend.text=element_text(size=26)))
  p0<- p0 + theme(legend.position="none") 
  
  
  p1<- ggplot(dfPlot1, aes(x=OVERHEAD, y=lsmean, group=ORDER)) + ggtitle("Captive") +
    geom_line(aes(color=ORDER),position=pd) + 
    geom_point(aes(color=ORDER), size=2, stroke = 2,position=pd) + 
    # geom_errorbar(aes(ymin=lsmean-SE, ymax=lsmean+SE, color=ORDER), width=.2,position=pd)+
    ylab("Likert") + xlab("Overhead Level [%]") + scale_y_continuous(limits=c(1,5), breaks=c(1,2,3,4,5), labels=c("Too slow","Slower than normal","Normal","Faster than normal","Too fast")) + theme_bw(base_size=21) + theme(legend.position="none")
  
  p<-plot_grid(p0,p1, ncol=2)
  png(file = "/Users/cornejo/Desktop/3.png",width = 900,height = 500)
  plot(plot_grid(p, legend, ncol = 1, rel_heights = c(1,0.1)))
  dev.off()
}


plotsModelByExperiment <-function(df){
  # Divide by Type-subtask
  dfPlot0 <- subset(df, TYPE_SUBTASK=="Instantaneous")
  dfPlot1 <- subset(df, TYPE_SUBTASK=="Immediate")
  dfPlot2 <- subset(df, TYPE_SUBTASK=="Continuous")
  dfPlot3 <- subset(df, TYPE_SUBTASK=="Captive")
  
  
  p0<- ggplot(dfPlot0, aes(x=ORDER, y=lsmean, group=OVERHEAD)) + ggtitle("Instantaneous") +
    geom_line(aes(color=OVERHEAD))+ geom_point(aes(color=OVERHEAD)) + ylab("Likert") + xlab("Order") + scale_y_continuous(limits=c(1,5), breaks=c(1,2,3,4,5), labels=c("Too slow","Slower than normal","Normal","Faster than normal","Too fast")) + theme_bw() + facet_grid(. ~ EXPERIMENT)
  
  
  legend <- get_legend(p0)  
  
  p0<- p0 + theme(legend.position="none")  
  
  
  p1<- ggplot(dfPlot1, aes(x=ORDER, y=lsmean, group=OVERHEAD)) + ggtitle("Immediate") +
    geom_line(aes(color=OVERHEAD))+ geom_point(aes(color=OVERHEAD)) + ylab("Likert") + xlab("Order") + scale_y_continuous(limits=c(1,5), breaks=c(1,2,3,4,5), labels=c("Too slow","Slower than normal","Normal","Faster than normal","Too fast")) + theme_bw() + theme(legend.position="none")  + facet_grid(. ~ EXPERIMENT)
  
  p2<- ggplot(dfPlot2, aes(x=ORDER, y=lsmean, group=OVERHEAD)) + ggtitle("Continuous") +
    geom_line(aes(color=OVERHEAD))+ geom_point(aes(color=OVERHEAD)) + ylab("Likert") + xlab("Order") + scale_y_continuous(limits=c(1,5), breaks=c(1,2,3,4,5), labels=c("Too slow","Slower than normal","Normal","Faster than normal","Too fast")) + theme_bw() + theme(legend.position="none")  + facet_grid(. ~ EXPERIMENT)
  
  p3<- ggplot(dfPlot3, aes(x=ORDER, y=lsmean, group=OVERHEAD)) + ggtitle("Captive") +
    geom_line(aes(color=OVERHEAD))+ geom_point(aes(color=OVERHEAD)) + ylab("Likert") + xlab("Order") + scale_y_continuous(limits=c(1,5), breaks=c(1,2,3,4,5), labels=c("Too slow","Slower than normal","Normal","Faster than normal","Too fast")) + theme_bw() + theme(legend.position="none")  + facet_grid(. ~ EXPERIMENT)
  
  
  p<-plot_grid(p0,p1,p2,p3, ncol=4)
  
  plot(plot_grid(p,legend, rel_widths = c(0.9,0.1)))
  
  
  
  
  # Divide by Type-subtask
  dfPlot0 <- subset(df, TYPE_SUBTASK=="Instantaneous")
  dfPlot1 <- subset(df, TYPE_SUBTASK=="Immediate")
  dfPlot2 <- subset(df, TYPE_SUBTASK=="Continuous")
  dfPlot3 <- subset(df, TYPE_SUBTASK=="Captive")
  
  
  p0<- ggplot(dfPlot0, aes(x=OVERHEAD, y=lsmean, group=ORDER)) + ggtitle("Instantaneous") + 
    geom_line(aes(color=ORDER))+ geom_point(aes(color=ORDER)) + ylab("Likert") + xlab("Overhead") + scale_y_continuous(limits=c(1,5), breaks=c(1,2,3,4,5), labels=c("Too slow","Slower than normal","Normal","Faster than normal","Too fast")) + theme_bw()  + facet_grid(. ~ EXPERIMENT)
  
  
  legend <- get_legend(p0)  
  
  p0<- p0 + theme(legend.position="none")  
  
  
  p1<- ggplot(dfPlot1, aes(x=OVERHEAD, y=lsmean, group=ORDER)) + ggtitle("Immediate") +
    geom_line(aes(color=ORDER))+ geom_point(aes(color=ORDER)) + ylab("Likert") + xlab("Overhead") + scale_y_continuous(limits=c(1,5), breaks=c(1,2,3,4,5), labels=c("Too slow","Slower than normal","Normal","Faster than normal","Too fast")) + theme_bw() + theme(legend.position="none")  + facet_grid(. ~ EXPERIMENT)
  
  p2<- ggplot(dfPlot2, aes(x=OVERHEAD, y=lsmean, group=ORDER)) + ggtitle("Continuous") +
    geom_line(aes(color=ORDER))+ geom_point(aes(color=ORDER)) + ylab("Likert") + xlab("Overhead") + scale_y_continuous(limits=c(1,5), breaks=c(1,2,3,4,5), labels=c("Too slow","Slower than normal","Normal","Faster than normal","Too fast")) + theme_bw() + theme(legend.position="none")  + facet_grid(. ~ EXPERIMENT)
  
  p3<- ggplot(dfPlot3, aes(x=OVERHEAD, y=lsmean, group=ORDER)) + ggtitle("Captive") +
    geom_line(aes(color=ORDER))+ geom_point(aes(color=ORDER)) + ylab("Likert") + xlab("Overhead") + scale_y_continuous(limits=c(1,5), breaks=c(1,2,3,4,5), labels=c("Too slow","Slower than normal","Normal","Faster than normal","Too fast")) + theme_bw() + theme(legend.position="none")  + facet_grid(. ~ EXPERIMENT)
  
  
  p<-plot_grid(p0,p1,p2,p3, ncol=4)
  
  plot(plot_grid(p,legend, rel_widths = c(0.9,0.1)))
}


# TYPE_TASK|OVERHEAD*ORDER*TYPE_SUBTASK
plotsModel4InteractionTask <-function(df){
  # Divide by captive-continuous, continuous-captive
  dfPlot0 <- subset(df, OVERHEAD==0)
  dfPlot1 <- subset(df, OVERHEAD==20)
  dfPlot2 <- subset(df, OVERHEAD==80)
  dfPlot3 <- subset(df, OVERHEAD==140)
  
  p0<- ggplot(dfPlot0, aes(x=TYPE_SUBTASK, y=lsmean, group=ORDER)) + ggtitle("Overhead 0") +
    geom_line(aes(color=ORDER))+ geom_point(aes(color=ORDER)) + ylab("Likert") + xlab("Type Subtask") + scale_y_continuous(limits=c(1,5), breaks=c(1,2,3,4,5), labels=c("Too slow","Slower than normal","Normal","Faster than normal","Too fast")) + theme_bw() + facet_grid(. ~ TYPE_TASK)
  
  legend <- get_legend(p0)  
  
  p0<- p0 + theme(legend.position="none")  
  
  
  p1<- ggplot(dfPlot1, aes(x=TYPE_SUBTASK, y=lsmean, group=ORDER)) + ggtitle("Overhead 20") +
    geom_line(aes(color=ORDER))+ geom_point(aes(color=ORDER)) + ylab("Likert") + xlab("Type Subtask") + scale_y_continuous(limits=c(1,5), breaks=c(1,2,3,4,5), labels=c("Too slow","Slower than normal","Normal","Faster than normal","Too fast")) + theme_bw() + theme(legend.position="none") + facet_grid(. ~ TYPE_TASK)
  
  p2<- ggplot(dfPlot2, aes(x=TYPE_SUBTASK, y=lsmean, group=ORDER)) + ggtitle("Overhead 80") +
    geom_line(aes(color=ORDER))+ geom_point(aes(color=ORDER)) + ylab("Likert") + xlab("Type Subtask") + scale_y_continuous(limits=c(1,5), breaks=c(1,2,3,4,5), labels=c("Too slow","Slower than normal","Normal","Faster than normal","Too fast")) + theme_bw() + theme(legend.position="none") + facet_grid(. ~ TYPE_TASK)
  
  p3<- ggplot(dfPlot3, aes(x=TYPE_SUBTASK, y=lsmean, group=ORDER)) + ggtitle("Overhead 140") +
    geom_line(aes(color=ORDER))+ geom_point(aes(color=ORDER)) + ylab("Likert") + xlab("Type Subtask") + scale_y_continuous(limits=c(1,5), breaks=c(1,2,3,4,5), labels=c("Too slow","Slower than normal","Normal","Faster than normal","Too fast")) + theme_bw() + theme(legend.position="none") + facet_grid(. ~ TYPE_TASK)
  
  
  p<-plot_grid(p0,p1,p2,p3, ncol=4)
  
  plot(plot_grid(p,legend, rel_widths = c(0.9,0.1)))
}

# library(dplyr)
# length(distinct(dMILAN01, SUBJECT_ID, .keep_all = TRUE)$SUBJECT_ID)
# length(distinct(dMILAN02, SUBJECT_ID, .keep_all = TRUE)$SUBJECT_ID)
# length(distinct(dMADRID01, SUBJECT_ID, .keep_all = TRUE)$SUBJECT_ID)


d <- as.data.frame(read_excel("/Users/cornejo/Workspace/monitoring/reports/user study/Analisis FSE Usabilidad/Raw Data ALL Experiments.xlsx"))

d$EXPERIMENT=as.factor(d$EXPERIMENT)
d$SUBJECT_ID=as.factor(d$SUBJECT_ID)
d$OVERHEAD=as.factor(d$OVERHEAD)
d$TASK=as.factor(d$TASK)
d$ORDER=as.factor(d$ORDER)
d$SUB_TASK=as.factor(d$SUB_TASK)
d$TYPE_SUBTASK=as.factor(d$TYPE_SUBTASK)
d$TYPE_SUBTASK = factor(d$TYPE_SUBTASK,levels(d$TYPE_SUBTASK)[c(4,3,2,1)])
d$TYPE_SUBTASK_2=as.factor(d$TYPE_SUBTASK_2)
d$TYPE_SUBTASK_3=as.factor(d$TYPE_SUBTASK_3)
d$TYPE_TASK=as.factor(d$TYPE_TASK)

d$PROD_SQRT <- sqrt(d$PROD)
d$PROD_LOG <- log(d$PROD)
d$PROD_CUBE <- sign(d$PROD) * abs(d$PROD)^(1/3)
d$PROD_SQUARE <- (d$PROD)^2

plotDataByExperiment(d)
plotData(d)
# If just the first sub-task is used
# d<-subset(d, TYPE_SUBTASK_2!=5)
# If just the second sub-task is used
# d<-subset(d, TYPE_SUBTASK_3!=5)


#dMADRID01<-subset(d,EXPERIMENT=="MADRID01")
dMILAN01<-subset(d,EXPERIMENT=="MILAN01")
dMILAN02<-subset(d,EXPERIMENT=="MILAN02")
dMILAN03<-subset(d,EXPERIMENT=="MILAN03")

# library(splitstackshape)
# d <- cSplit(d, "IDE", sep = ",")
# d <- cSplit(d, "Languages", sep = ",")
# d <- cSplit(d, "giochi", sep = "|")
# d <- cSplit(d, "HDD", sep = ",")
# 
# d[, "GAMES"] <- apply(d, 1, elaborate_giochi)
# d[, "WAITS"] <- apply(d, 1, elaborate_attendi)
# d[, "TYPE_OF_USER"] <- apply(d, 1, elaborate_type_of_user)
# d[, "TAK"] <- apply(d, 1, elaborate_tak)
# d[, "TYPE_OF_PC"] <- apply(d, 1, elaborate_type_of_pc)

######################################################################################################################
################################################### Individual analyses ##############################################
######################################################################################################################
# First check ICC
model0<- lmer(PROD ~ 1 + (1|SUBJECT_ID), data=dMILAN01)
ICClmer(model0)
model0 <- geeglm(PROD ~ TYPE_SUBTASK*OVERHEAD*ORDER, data=dMILAN01, id=dMILAN01$SUBJECT_ID, corstr="exchangeable")
anova(model0)
summary(model0)
p <- lsmeans(model0, ~ TYPE_SUBTASK*OVERHEAD*ORDER, type="response")
df <- as.data.frame(summary(p))
str(df)
plotsModel(df)
lsmeans(model0, pairwise ~ ORDER|OVERHEAD*TYPE_SUBTASK)

# First check ICC
model0<- lmer(PROD ~ 1 + (1|SUBJECT_ID), data=dMILAN02)
ICClmer(model0)
model0 <- geeglm(PROD ~ TYPE_SUBTASK*OVERHEAD*ORDER, data=dMILAN02, id=SUBJECT_ID, corstr="exchangeable")
anova(model0)
summary(model0)
p <- lsmeans(model0, ~ TYPE_SUBTASK*OVERHEAD*ORDER, type="response")
df <- as.data.frame(summary(p))
str(df)
plotsModel(df)
lsmeans(model0, pairwise ~ ORDER|OVERHEAD*TYPE_SUBTASK)

# First check ICC
model0<- lmer(PROD ~ 1 + (1|SUBJECT_ID), data=dMILAN03)
ICClmer(model0)
model0 <- geeglm(PROD ~ TYPE_SUBTASK*OVERHEAD*ORDER, data=dMILAN03, id=dMILAN03$SUBJECT_ID, corstr="exchangeable")
anova(model0)
summary(model0)
p <- lsmeans(model0, ~ TYPE_SUBTASK*OVERHEAD*ORDER, type="response")
df <- as.data.frame(summary(p))
str(df)
plotsModel(df)
lsmeans(model0, pairwise ~ ORDER|OVERHEAD*TYPE_SUBTASK)



# First check ICC
# model0<- lmer(PROD ~ 1 + (1|SUBJECT_ID), data=dMADRID01)
# ICClmer(model0)
# model0 <- geeglm(PROD ~ TYPE_SUBTASK*OVERHEAD*ORDER, data=dMADRID01, id=dMADRID01$SUBJECT_ID, corstr="exchangeable")
# anova(model0)
# summary(model0)
# p <- lsmeans(model0, ~ TYPE_SUBTASK*OVERHEAD*ORDER, type="response")
# df <- as.data.frame(summary(p))
# str(df)
# plotsModel(df)
# lsmeans(model0, pairwise ~ ORDER|OVERHEAD*TYPE_SUBTASK)
############ Weird behaviour for OVERHEAD 20
# dMADRID01OUTLIER<- subset(dMADRID01, (TYPE_SUBTASK=="Instantaneous" | TYPE_SUBTASK=="Immediate") & (OVERHEAD==20))
# str(dMADRID01OUTLIER)
# aggregate(PROD ~ TYPE_SUBTASK + SUBJECT_ORIGINAL , FUN = function(x) c(N = length(x), MEAN = mean(x), SD=sd(x) ,MDN=median(x)), data=dMADRID01OUTLIER)
# # SUBJECT_ID 4 is suspicious...
# # dMADRID01OUTLIER$TYPE_SUBTASK<-droplevels(dMADRID01OUTLIER$TYPE_SUBTASK)
# # dMADRID01OUTLIER$SUBJECT_ID<-droplevels(dMADRID01OUTLIER$SUBJECT_ID)
# # dMADRID01 <- subset(dMADRID01, SUBJECT_ID!="MADRID01_4") 
# # dMADRID01$SUBJECT_ID<-droplevels(dMADRID01$SUBJECT_ID)
# # model0 <- geeglm(PROD ~ TYPE_SUBTASK*OVERHEAD*ORDER, data=dMADRID01, id=SUBJECT_ID, corstr="exchangeable")
# # anova(model0)
# # summary(model0)
# # p <- lsmeans(model0, ~ TYPE_SUBTASK*OVERHEAD*ORDER, type="response")
# # df <- as.data.frame(summary(p))
# # str(df)
# # plotsModel(df)

######################################################################################################################
################################################### Global Analysis ##################################################
######################################################################################################################
#plotData(d)
# Check Experiment's ICC
model0<- lmer(PROD ~ 1 + (1|EXPERIMENT), data=d)
ICClmer(model0)
model0<- lmer(PROD ~ 1 + (1|SUBJECT_ID), data=d)
ICClmer(model0)
model0<- lmer(PROD ~ 1 + (1|EXPERIMENT) + (1|EXPERIMENT:SUBJECT_ID), data=d)
summary(model0)
ICClmer3Levels(model0)

####### IPD mega-trial
model0 <- geeglm(PROD ~ TYPE_SUBTASK*OVERHEAD*ORDER, data=d, id=SUBJECT_ID, corstr="exchangeable")
anova(model0)
summary(model0)

p <- lsmeans(model0, ~ TYPE_SUBTASK*OVERHEAD*ORDER, type="response")
df <- as.data.frame(summary(p))
str(df)
plotsModel(df)
lsmeans(model0, pairwise ~ ORDER|OVERHEAD*TYPE_SUBTASK)

lsmeans(model0, pairwise ~ OVERHEAD|ORDER*TYPE_SUBTASK)
lsmeans(model0, pairwise ~ ORDER|OVERHEAD*TYPE_SUBTASK)
lsmeans(model0, pairwise ~ TYPE_SUBTASK|ORDER*OVERHEAD)

####### IPD stratified
model0 <- geeglm(PROD ~ TYPE_SUBTASK*OVERHEAD*ORDER + EXPERIMENT, data=d, id=SUBJECT_ID, corstr="exchangeable")
anova(model0)
summary(model0)
p <- lsmeans(model0, ~ TYPE_SUBTASK*OVERHEAD*ORDER, type="response")
df <- as.data.frame(summary(p))
str(df)
plotsModel(df)
lsmeans(model0, pairwise ~ ORDER|OVERHEAD*TYPE_SUBTASK)

####### IPD interaction
model0 <- geeglm(PROD ~ TYPE_SUBTASK*OVERHEAD*ORDER*EXPERIMENT, data=d, id=SUBJECT_ID, corstr="exchangeable")
anova(model0)
summary(model0)
p <- lsmeans(model0, ~ TYPE_SUBTASK*OVERHEAD*ORDER, type="response")
df <- as.data.frame(summary(p))
str(df)
# Order 3 plot
plotsModel(df)
lsmeans(model0, pairwise ~ ORDER|OVERHEAD*TYPE_SUBTASK)
# Order 4 plot
p <- lsmeans(model0, ~ TYPE_SUBTASK*OVERHEAD*ORDER*EXPERIMENT, type="response")
df <- as.data.frame(summary(p))
str(df)
plotsModelByExperiment(df)
lsmeans(model0, pairwise ~ EXPERIMENT|ORDER*OVERHEAD*TYPE_SUBTASK)
lsmeans(model0, pairwise ~ EXPERIMENT|ORDER*OVERHEAD*TYPE_SUBTASK)
lsmeans(model0, pairwise ~ ORDER|OVERHEAD*TYPE_SUBTASK)
lsmeans(model0, pairwise ~ TYPE_SUBTASK|OVERHEAD*ORDER)
# Do results agree with a linear mixed model?
modellmer<- lmer(PROD ~ TYPE_SUBTASK*OVERHEAD*ORDER*EXPERIMENT + (1|EXPERIMENT:SUBJECT_ID) + (1|TASK/SUB_TASK), data=d)
anova(modellmer)
p <- lsmeans(modellmer, ~ TYPE_SUBTASK*OVERHEAD*ORDER, type="response")
df <- as.data.frame(summary(p))
str(df)
plotsModel(df)
lsmeans(modellmer, pairwise ~ ORDER|OVERHEAD*TYPE_SUBTASK)
qqnorm(resid(modellmer)) 
qqline(resid(modellmer))



###############################################################################################################
################################ Analyze Interaction By Task ##################################################
###############################################################################################################

######### Now get rid of the data for Captive and continuous and analyze whether there is an interaction
df <-subset(d, TYPE_SUBTASK!="Captive")
df <-subset(df, TYPE_SUBTASK!="Continuous")
df$TYPE_SUBTASK<-droplevels(df$TYPE_SUBTASK)

# The interaction is significant! My godness forget about understanding this..
model0 <- geeglm(PROD ~ TYPE_SUBTASK*TYPE_TASK*ORDER*OVERHEAD, data=df, id=SUBJECT_ID, corstr="exchangeable")
summary(model0)
anova(model0)
p <- lsmeans(model0, ~ TYPE_SUBTASK*TYPE_TASK*ORDER*OVERHEAD, type="response")
df <- as.data.frame(summary(p))
str(df)
plotsModel4InteractionTask(df)
lsmeans(model0, pairwise ~ TYPE_TASK|OVERHEAD*ORDER*TYPE_SUBTASK)

###############################################################################################################
################################ Analyze With LMER ############################################################
###############################################################################################################
# Assumptions in R http://ademos.people.uic.edu/Chapter18.html
model0 <- lmer(PROD ~ TYPE_SUBTASK*ORDER*OVERHEAD +(1|SUBJECT_ID) + (1|TASK/SUB_TASK) , data=d)
anova(model0)
model1 <- lmer(PROD ~ TYPE_SUBTASK*ORDER*OVERHEAD +(1|EXPERIMENT) + (1|SUBJECT_ID) + (1|TASK/SUB_TASK) , data=d)
anova(model1)
model2 <- lmer(PROD ~ TYPE_SUBTASK*ORDER*OVERHEAD +EXPERIMENT + (1|EXPERIMENT:SUBJECT_ID) + (1|TASK/SUB_TASK) , data=d)
anova(model2)
anova(model0, model1, model2)
lsmeans(model2, pairwise ~ ORDER|OVERHEAD*TYPE_SUBTASK)
qqnorm(resid(model2))
qqline(resid(model2))
plot(model2)

plot(model2, resid(.) ~ fitted(.))
plot(model2, resid(.) ~ fitted(.) | TYPE_SUBTASK)
plot(model2, resid(.) ~ fitted(.) | TYPE_SUBTASK*ORDER)
plot(model2, resid(.) ~ fitted(.) | TYPE_SUBTASK*ORDER*OVERHEAD)


