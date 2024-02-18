rm(list=ls())
options(warn=-1)

# Required packages
source("CSCM_functions.R")
library(grid)
library(ggplot2)
library(pampe)
library(tidyverse)


# # Load data
load("./RData/me_InnoScore.RData")
mechpanel_all$year = as.numeric(mechpanel_all$year)


Y = "cityscore"
Y_lab <-  "知\n识\n溢\n出\n效\n应"


# parameters
year = 2003:2019
city = unique(mechpanel_all$city)

mechpanel_all[,Y] = as.numeric(mechpanel_all[,Y])
tr_time = 13  #Treatment time point (1997 in real years)
tr_unit = 1   #1 = Beijing
control = sort(unique(mechpanel_all$ID))[c(-tr_unit)]



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

road.main.res <- countSynth(data=mechpanel_all,
                            # predictors=predictors, 
                            # special.predictors=special.predictors,
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
  cunit.names <- unique(mechpanel_all %>% dplyr::filter(ID %in% control) %>% dplyr::select(ID, city))
  cunit.ordered <- as.data.frame(cunit.names[order(cunit.names$city),])
  cunit.synth = cunit.csynth = cunit.ordered
  cunit.synth$Country <- unique(mechpanel_all$city)[control]
  cunit.synth$SCM.W <- synth.w
  cunit.synth$CSCM.W <- csynth.w
  
  cunit.synth[,3:5]
}

# -------------------- #
# 不带合成控制法       #
# -------------------- #

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
  ylab(Y_lab) + 
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


ggsave(
  filename = paste0("./fig/机制-知识溢出效应.png"), # 保存的文件名称。通过后缀来决定生成什么格式的图片
  width = 9.5,             # 宽
  height = 9.5,            # 高
  units = "in",          # 单位
  dpi = 300              # 分辨率DPI
)
