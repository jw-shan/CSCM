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


Y = "finan_ratio"
Y_lab <-  "资\n本\n积\n累\n效\n应"

# discard missing data
mechpanel_all <- mechpanel_all %>% filter(!city%in%unique(mechpanel_all$city[is.na(mechpanel_all[,Y])]))



# parameters
year = 2003:2019
city = unique(mechpanel_all$city)
mechpanel_all[,Y] = as.numeric(mechpanel_all[,Y])
tr_time = 13  #Treatment time point (1997 in real years)
tr_unit = 1   #1 = Beijing
control = sort(unique(mechpanel_all$ID))[c(-tr_unit)]


# ------------------- #
#  run regression
# --------------------
time.pretr = 1:12
time.tr = 13:17
control = city[unique(mechpanel_all$ID)!=tr_unit]
# 转换成growth data
data_growth.df <- mechpanel_all %>%
  select("city","year",Y) %>% 
  pivot_wider(names_from = city, values_from = Y) %>% 
  select(!"year") %>% as.data.frame()


pol.integ <- pampe(time.pretr=time.pretr, time.tr=time.tr, treated="北京",
                   controls=control, data=data_growth.df, nvmax = 3)
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
  geom_line(mapping = aes(x=year,y=True,linetype="北京"),size=0.8)+   
  geom_line(mapping = aes(x=year,y=Predicted,linetype="合成北京"),alpha=1,size=0.7)+
  geom_vline(xintercept=2015,linetype=2,alpha=0.8)  + theme_classic() +
  ylab(Y_lab) + 
  xlab("") +
  scale_linetype_manual("", values=c(1,5))+
  scale_x_continuous(breaks=seq(2003,2019,2))+
  theme(axis.text = element_text(color = "black",size = 18), # 坐标轴字体大小
        axis.title.y = element_text(angle=0,  hjust=0, vjust=0.6, size = 18),  #y轴标题位置和大小，vjust调整上下位置
        text = element_text(family = "serif"),
        # legend.direction = "horizontal",
        legend.text  = element_text(size = 15), # 图例字体大小
        legend.key.size = unit(3, "lines"),
        legend.position="bottom"
        # legend.position=c(0.15,0.9)
        # plot.margin=unit(c(1,1,2,1),'lines')
  )

g_p



ggsave(
  filename = paste0("./fig/机制-资本积累效应.png"), # 保存的文件名称。通过后缀来决定生成什么格式的图片
  width = 9.5,             # 宽
  height = 9.5,            # 高
  units = "in",          # 单位
  dpi = 300              # 分辨率DPI
)
