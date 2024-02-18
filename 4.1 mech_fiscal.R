rm(list=ls())
options(warn=-1)

# Required packages
source("CSCM_functions.R")
library(grid)
library(ggplot2)
library(pampe)
library(tidyverse)


# # Load data
load("./RData/me_fiscal_finan.RData")
mechpanel_all$year = as.numeric(mechpanel_all$year)

Y = "fiscal_ratio"
Y_lab <-  "营\n商\n环\n境\n改\n善\n效\n应"





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


# ------------------- #
#  run synth
# --------------------
dataprep.out <- dataprep(
  foo = mechpanel_all,
  predictors=predictors,
  special.predictors=special.predictors,
  dependent=c(Y),
  unit.variable="ID",
  time.variable = "year",
  time.predictors.prior = mechpanel_all$year[1:tr_time-1],
  time.optimize.ssr = mechpanel_all$year[1:tr_time-1],
  time.plot = unique(mechpanel_all$year),
  treatment.identifier = 1, 
  controls.identifier = control,
  unit.names.variable = "city"
)
synth.out <- synth(dataprep.out)


# -------------------------- #
# Produce unit weights table #
# -------------------------- #
{
  synth.w <- round(synth.out$solution.w, 2)
  cunit.names <- unique(mechpanel_all %>% dplyr::filter(ID %in% control) %>% dplyr::select(ID, city))
  cunit.ordered <- as.data.frame(cunit.names[order(cunit.names$city),])
  cunit.synth = cunit.ordered
  cunit.synth$Country <- unique(mechpanel_all$city)[-1]
  cunit.synth$SCM.W <- synth.w
  cunit.synth[,3:4]
}


predictor.y     = dataprep.out$X1   #北京predictors的值
predictor.y.hat = dataprep.out$X0%*%synth.out$solution.w  #合成北京predictors的值



# ------------------- #
#   plot 
# --------------------

# year = seq(2010.25,2020,0.25)
plot.df = data.frame(year=year)
plot.df$True = as.numeric(dataprep.out$Y1plot )
plot.df$Predicted = dataprep.out$Y0plot %*% as.numeric(synth.out$solution.w)

#计算RMSPE
{  
  RMSPE.df = data.frame(city=city,RMSPE=NA,RMSPE_pose=NA)
  RMSPE.df[1,2] = sqrt(sum((plot.df$True[1:tr_time-1]-plot.df$Predicted[1:tr_time-1])^2)/(tr_time-1))
  RMSPE.df[1,3] = sqrt(sum((plot.df$True[-1:-tr_time+1]-plot.df$Predicted[-1:-tr_time+1])^2)/(length(year)-tr_time+1))
  print(paste0("RMSPE=",round(RMSPE.df[1,2],4)))
}

g_p <- ggplot(data=plot.df) +
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
        # legend.direction = "horizontal",
        legend.text  = element_text(size = 15), # 图例字体大小
        legend.key.size = unit(3, "lines"),
        legend.position="bottom") 
g_p 


ggsave(
  filename = paste0("./fig/机制-营商环境改善效应.png"), # 保存的文件名称。通过后缀来决定生成什么格式的图片
  width = 9.5,             # 宽
  height = 9.5,            # 高
  units = "in",          # 单位
  dpi = 300              # 分辨率DPI
)




