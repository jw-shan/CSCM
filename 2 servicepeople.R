rm(list=ls())
options(warn=-1)

# Required packages
source("CSCM_functions.R")
library(openxlsx)
library(grid)


# # Load data
# data <- read.xlsx("data.xlsx",sheet=2)

## We suggest you load the RData file directly 
load("./RData/servicepeople.RData")


#----------------------------------#
#      choose industry
#----------------------------------#
#       1 -- Science
#       2 -- Finance
#       3 -- Information
#-----------------------------------#
          industry = 1
#-----------------------------------#



# parameters
year = 2003:2019
city = unique(data$city)
Y = c("sciencepeople","financepeople","informationpeople")[industry]
tr_time = 13  #Treatment time point (1997 in real years)
tr_unit = 1   #1 = Beijing

if (industry==2) {
  control = unique(data$ID)[c(-tr_unit,-33)]
}else{
  control = unique(data$ID)[c(-tr_unit)]
}


# ------------------- #
#  Predictors
# --------------------
predictors <- c("ln(pGDP)", "size","asset", "FDI/GDP", "research", "urbanization")
special.predictors <- list(
  list(Y, 2003:2014, "mean"),
  list(Y, 2003, "mean"),
  list(Y, 2008, "mean"),
  list(Y, 2013, "mean")
)

# --------------------- #
# Run the main analysis #
# --------------------- #

road.main.res <- countSynth(data=data,
                            predictors=predictors, 
                            special.predictors=special.predictors,
                            dependent=c(Y),
                            unit.variable="ID",
                            time.variable = "year",
                            treatment.identifier = tr_unit, # 1 = Beijing
                            controls.identifier = control,
                            t_int=tr_time)



# -------------------------- #
# Produce unit weights table #
# -------------------------- #
{
synth.w <- round(road.main.res$unit.weight.SCM, 2)
csynth.w <- round(road.main.res$unit.weight.full.sample, 2)
cunit.names <- unique(data %>% dplyr::filter(ID %in% control) %>% dplyr::select(ID, city))
cunit.ordered <- as.data.frame(cunit.names[order(cunit.names$city),])
cunit.synth = cunit.csynth = cunit.ordered
cunit.synth$Country <- unique(data$city)[control]
cunit.synth$SCM.W <- synth.w
cunit.synth$CSCM.W <- csynth.w

cunit.synth[,3:5]
}

# -------------------- #
# 不带合成控制法       #
# -------------------- #
{
cscm.cf <- road.main.res$dataprep.main$Y0plot %*% as.numeric(road.main.res$unit.weight.full.sample)

pre_df <- as.data.frame(cbind(year,cscm.cf,rep("Predicted",length(year))))
true_df <- as.data.frame(cbind(year,road.main.res$dataprep.main$Y1plot,rep("_True",length(year))))
colnames(pre_df)=colnames(true_df)=c("Year","Y","Method")
cfdf <- rbind(true_df,pre_df)
cfdf <- cfdf %>% mutate(Y = as.numeric(as.character(Y)),
                        Year = as.numeric(as.character(Year)))

plot.df.tr = data.frame(year=year,True=cfdf$Y[1:length(year)],Predicted=cfdf$Y[-1:-length(year)])



#计算RMSPE
{  
  RMSPE.df = data.frame(city=city,RMSPE=NA,RMSPE_post=NA,ind=0)
  RMSPE.df[1,2] = sqrt(sum((plot.df.tr$True[1:tr_time-1]-plot.df.tr$Predicted[1:tr_time-1])^2)/(tr_time-1))
  RMSPE.df[1,3] = sqrt(sum((plot.df.tr$True[-1:-tr_time+1]-plot.df.tr$Predicted[-1:-tr_time+1])^2)/(length(year)-tr_time+1))
  print(paste0("RMSPE=",round(RMSPE.df[1,2],4)))
}



g_p <- ggplot(data=plot.df.tr) +
  geom_line(mapping = aes(x=year,y=True,linetype="北京"),size=0.8)+  
  geom_line(mapping = aes(x=year,y=Predicted,linetype="合成北京"),alpha=1,size=0.7)+
  geom_vline(xintercept=2015,linetype=2,alpha=0.8)  + theme_classic() +
  ylab(c("科\n技\n服\n务\n业\n就\n业\n人\n数\n比",
         "金\n融\n服\n务\n业\n就\n业\n人\n数\n比",
         "信\n息\n服\n务\n业\n就\n业\n人\n数\n比")[industry]) + 
  xlab("") +
  scale_linetype_manual("", values=c(1,5),guide=guide_legend(override.aes=list(lwd=c(1,0.5))))+
  scale_x_continuous(breaks=seq(2003,2019,2))+
  theme(axis.text = element_text(color = "black",size = 18), # 坐标轴字体大小
      axis.title.y = element_text(angle=0,  hjust=0, vjust=0.6, size = 18),  #y轴标题位置和大小，vjust调整上下位置
      text = element_text(family = "serif"),
      legend.direction = "horizontal",
      legend.text  = element_text(size = 18), # 图例字体大小
      legend.key.size = unit(3, "lines"),
      legend.position="bottom"
      # legend.position=c(0.5,-0.15),   #图例位置，需要调整第二个数字控制上下
      # plot.margin=unit(c(1,1,2,1),'lines')
      )


g_p
}

# -- 
{
  g_pp <- ggplot(data=plot.df.tr) +
    geom_vline(xintercept=2015,linetype=2)+ geom_hline(yintercept=0,linetype=2)  + theme_classic() +
    theme(text = element_text(size=18)) +
    ylab(c("科\n技\n服\n务\n业\n就\n业\n人\n数\n比\n差\n值",
           "金\n融\n服\n务\n业\n就\n业\n人\n数\n比\n差\n值",
           "信\n息\n服\n务\n业\n就\n业\n人\n数\n比\n差\n值")[industry]) +
    xlab("") +
    scale_x_continuous(breaks=seq(2003,2019,2)) +
    theme(axis.text  = element_text(color = "black",size = 18),
          axis.title.y = element_text(angle=0,  hjust=0, vjust=0.6,size = 18),
          text = element_text(family = "serif"))
  if (industry==3){g_pp=g_pp+ylim(-1,6)}
  g_p2 <- g_pp + geom_line(mapping = aes(x=year,y=True-Predicted),size=0.8)
  g_p2
}



# -------------------- #
#    placebo           #
# -------------------- #
g_p3 = g_pp + geom_line(mapping = aes(x=year,y=True-Predicted, linetype="北京",alpha="北京"),size=0.8)
count = 0

pre_diff.df = data.frame(year=year)
pre_diff.df[,2:(length(city)+1)] = NA
pre_diff.df[,2] =  plot.df.tr$True - plot.df.tr$Predicted 
colnames(pre_diff.df)[-1] = city

RMSPE.df[,"ind"] = 0

for (i in 1:length(control)) {
  print(paste0("[",i,"/",length(control),"]"))
  road.main.res2 <- countSynth(data=data,
                               predictors=predictors, 
                               special.predictors=special.predictors,
                               dependent=c(Y),
                               unit.variable="ID",
                               time.variable = "year",
                               treatment.identifier = control[i], 
                               controls.identifier = control[-i], 
                               t_int=tr_time) 
  
  cscm.cf2 <- road.main.res2$dataprep.main$Y0plot %*% as.numeric(road.main.res2$unit.weight.full.sample)
  pre_df2 <- as.data.frame(cbind(year,cscm.cf2,rep("Predicted",length(year))))
  true_df2 <- as.data.frame(cbind(year,road.main.res2$dataprep.main$Y1plot,rep("_True",length(year))))
  colnames(pre_df2)=colnames(true_df2)=c("year","Y","Method")
  
  # 
  plot.df2 = data.frame(year=year)
  plot.df2$True = as.numeric(as.character(true_df2$Y))
  plot.df2$Predicted = as.numeric(as.character(pre_df2$Y))
  
  pre_diff.df[,i+1] = plot.df2$True - plot.df2$Predicted  
  
  RMSPE.df[i+1,"RMSPE"]      = sqrt(sum((plot.df2$True[1:tr_time-1]-plot.df2$Predicted[1:tr_time-1])^2)/(tr_time-1))
  RMSPE.df[i+1,"RMSPE_post"] = sqrt(sum((plot.df2$True[-1:-tr_time+1]-plot.df2$Predicted[-1:-tr_time+1])^2)/(length(year)-tr_time+1))
  
  if (RMSPE.df[i+1,"RMSPE"]<1) {
    count = count+1
    RMSPE.df[i+1,"ind"]=1
   
    if (count == 1) {
      g_p3 = g_p3 + geom_line(data = plot.df2, mapping = aes(x=year,y=True-Predicted,linetype = "其他城市",alpha="其他城市"),size=0.5) #size=粗细，alpha=深浅
    }else{
      g_p3 = g_p3 + geom_line(data = plot.df2, mapping = aes(x=year,y=True-Predicted),linetype = 1,alpha=0.2,size=0.5) #size=粗细，alpha=深浅
    }
  }
  
}
g_p4 <- g_p3 + scale_alpha_manual("", breaks=c("北京","其他城市"), values=c(1,0.2))+
  scale_linetype_manual("", breaks=c("北京","其他城市"), values=c(1,1), guide=guide_legend(override.aes=list(lwd=c(1,0.5)))) + 
  theme(  legend.text  = element_text(size = 15), # 图例字体大小
          legend.key.size = unit(3, "lines"),
          legend.position="bottom"
          # legend.position=c(0.15,0.9)
          )
g_p4


# 波动最大是哈尔滨

# # 单个城市画图
# g_pp +geom_line(data = pre_diff.df, mapping = aes(x=year,y=哈尔滨),alpha=0.2,size=0.7)+ geom_hline(yintercept=0,linetype=2)
# 





