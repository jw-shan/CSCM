rm(list=ls())
options(warn=-1)
library(pampe)
library(openxlsx)


# load data
data <- read.xlsx("data.xlsx", sheet = 3)
year = data$year
city = colnames(data)[-1]
data_growth.df = data[,-1]


## or you can load RData directly by
# load("./RData/financepeople.RData")


# ------------------- #
#  run regression
# --------------------
time.pretr = 1:12
time.tr = 13:17
control = city[-1]


pol.integ <- pampe(time.pretr=time.pretr, time.tr=time.tr, treated="北京",
                   controls=control, data=data_growth.df, nvmax = 8)
summary(pol.integ)



# ------------------- #
#    plot
# --------------------
tr_time = time.tr[1]
plot.df.tr = data.frame(year=year,True=pol.integ$counterfactual[,1],Predicted=pol.integ$counterfactual[,2])

#计算RMSPE
{  
  RMSPE.df = data.frame(city=city,RMSPE=NA,RMSPE_post=NA,ind=0)
  RMSPE.df[1,2] = sqrt(sum((plot.df.tr$True[1:tr_time-1]-plot.df.tr$Predicted[1:tr_time-1])^2)/(tr_time-1))
  RMSPE.df[1,3] = sqrt(sum((plot.df.tr$True[-1:-tr_time+1]-plot.df.tr$Predicted[-1:-tr_time+1])^2)/(length(year)-tr_time+1))
  print(paste0("RMSPE=",round(RMSPE.df[1,2],4)))
}



g_p <- ggplot(data=plot.df.tr) +
  geom_line(mapping = aes(x=year,y=True,linetype="北京"),size=1)+   # size尝试1.1和1.2，看哪个更美观
  geom_line(mapping = aes(x=year,y=Predicted,linetype="合成北京"),alpha=1,size=0.7)+
  geom_vline(xintercept=2015,linetype=2,alpha=0.8)  + theme_classic() +
  ylab("金\n融\n相\n对\n就\n业\n人\n数") + 
  xlab("") +
  scale_linetype_manual("", values=c(1,5))+
  scale_x_continuous(breaks=seq(2003,2019,2))+
  theme(axis.text = element_text(color = "black",size = 15), # 坐标轴字体大小
        axis.title.y = element_text(angle=0,  hjust=0, vjust=0.5, size = 15),  #y轴标题位置和大小，vjust调整上下位置
        text = element_text(family = "serif"),
        legend.direction = "horizontal",
        legend.text  = element_text(size = 15), # 图例字体大小
        legend.key.size = unit(3, "lines"),
        legend.position="bottom"
        # legend.position=c(0.5,-0.15),   #图例位置，需要调整第二个数字控制上下
        # plot.margin=unit(c(1,1,2,1),'lines')
  )

g_p

# -- 
{
  g_pp <- ggplot(data=plot.df.tr) +
    geom_vline(xintercept=2015,linetype=2)+ geom_hline(yintercept=0,linetype=2)  + theme_classic() +
    theme(text = element_text(size=10)) +
    ylab("金\n融\n相\n对\n就\n业\n人\n数") +
    xlab("") +
    scale_x_continuous(breaks=seq(2003,2019,2)) +
    theme(axis.text  = element_text(color = "black",size = 15),
          axis.title.y = element_text(angle=0,  hjust=0, vjust=0.5,size = 15),
          text = element_text(family = "serif"))
  g_p2 <- g_pp + geom_line(mapping = aes(x=year,y=True-Predicted),size=1)
  g_p2
}




# -------------------- #
#    placebo           #
# -------------------- #
{
g_p3 = g_pp + geom_line(mapping = aes(x=year,y=True-Predicted),size=1.1)
count = 0

pre_diff.df = data.frame(year=year)
pre_diff.df[,2:(length(city)+1)] = NA
pre_diff.df[,2] =  plot.df.tr$True - plot.df.tr$Predicted 
colnames(pre_diff.df)[-1] = city

RMSPE.df[,"ind"] = 0
}

for (i in 1:length(control)) {
  print(paste0("[",i,"/",length(control),"]"))
  pol.integ2 <- pampe(time.pretr=time.pretr, time.tr=time.tr, treated=control[i],
                     controls=control[-i], data=data_growth.df, nvmax = 8)
  
  plot.df2 = data.frame(year=year,True=pol.integ2$counterfactual[,1],Predicted=pol.integ2$counterfactual[,2])
  pre_diff.df[,i+1] = plot.df2$True - plot.df2$Predicted  
  
  RMSPE.df[i+1,"RMSPE"]      = sqrt(sum((plot.df2$True[1:tr_time-1]-plot.df2$Predicted[1:tr_time-1])^2)/(tr_time-1))
  RMSPE.df[i+1,"RMSPE_post"] = sqrt(sum((plot.df2$True[-1:-tr_time+1]-plot.df2$Predicted[-1:-tr_time+1])^2)/(length(year)-tr_time+1))
  
  if (RMSPE.df[i+1,"RMSPE"]<1) {
    count = count+1
    RMSPE.df[i+1,"ind"]=1
    # g_p3 = g_p3 + geom_line(data = plot.df2, mapping = aes(x=year,y=True-Predicted),linetype = 1,alpha=0.3,size=0.5) #size=粗细，alpha=深浅
    g_p3 = g_p3 + geom_line(data = plot.df2, mapping = aes(x=year,y=True-Predicted),linetype = 2,alpha=0.5,size=0.6) #size=粗细，alpha=深浅
  }
  
  message("Done.")
}

g_p3

