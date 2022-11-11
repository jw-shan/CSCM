rm(list=ls())
options(warn=-1)
library(ggplot2)
library(grid)
library(Synth)
library(openxlsx)
library(dplyr)



# # --- load data from xlsx file -------
# data = read.xlsx("data.xlsx",sheet=1)[,c(1:2,11:21)]
# 
# city = unique(data$city)
# data = data.frame(city=data[,1], ID = rep(1:length(city),each=40),
#                   year=data[,2], year_ID=1:40,data[,-1:-2])


# # 删除不可用城市数据
# data = data[-which(data$city%in%c("石家庄","哈尔滨","西宁","南宁","乌鲁木齐","长春","厦门"
#                                   ,"大连")),]
# city = unique(data$city)


## We suggest you load the RData file directly 
load("./RData/seasondata.RData")


# ======================================================================== #
# ------------------------------------------------------------------------ #
#                         Synthetic Control
# ------------------------------------------------------------------------ #
# ======================================================================== #

# parameters
Y = "INSH"
control = unique(data$ID)[c(-1)]
tr_time = 22


# ------------------- #
#  Predictors
# --------------------
predictors <- c("ln.pGDP.", "size", "asset", "FDI.GDP",   
                "research" ,"urbanization")
special.predictors <- list(
  list(Y, 1:21, "mean"), 
  list(Y, 1, "mean"), 
  list(Y, 3, "mean"), 
  list(Y, 5, "mean"),
  list(Y, 7, "mean"), 
  list(Y, 9, "mean"), 
  list(Y, 11, "mean"), 
  list(Y, 13, "mean"),
  list(Y, 15, "mean"),
  list(Y, 17, "mean"), 
  list(Y, 19, "mean"),
  list(Y, 21, "mean")
)
# --------------------



# ------------------- #
#  run synth
# --------------------
dataprep.out <- dataprep(
  foo = data,
  predictors=predictors,
  special.predictors=special.predictors,
  dependent=c(Y),
  unit.variable="ID",
  time.variable = "year_ID",
  time.predictors.prior = data$year_ID[1:tr_time-1],
  time.optimize.ssr = data$year_ID[1:tr_time-1],
  time.plot = unique(data$year_ID),
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
  cunit.names <- unique(data %>% dplyr::filter(ID %in% control) %>% dplyr::select(ID, city))
  cunit.ordered <- as.data.frame(cunit.names[order(cunit.names$city),])
  cunit.synth = cunit.ordered
  cunit.synth$Country <- unique(data$city)[-1]
  cunit.synth$SCM.W <- synth.w
  cunit.synth[,3:4]
}


predictor.y     = dataprep.out$X1   #北京predictors的值
predictor.y.hat = dataprep.out$X0%*%synth.out$solution.w  #合成北京predictors的值



# ------------------- #
#   plot 
# --------------------
{
  year = seq(2010.25,2020,0.25)
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
 
  
  #   g_p <- ggplot(data=plot.df) +
  #     geom_line(mapping = aes(x=year,y=True,linetype="北京"),size=0.8)+
  #     geom_line(mapping = aes(x=year,y=Predicted,linetype="合成北京"),alpha=1,size=0.7)+
  #     geom_vline(xintercept=2015.5,linetype=2,alpha=0.8)  + theme_classic() +
  #     ylab("产\n业\n结\n构\n高\n级\n化") +
  #     xlab("") +
  #     scale_x_continuous(breaks=2010:2019+1,labels = unique(data$year)[seq(4,40,4)])+
  #     theme(axis.text.x  = element_text(angle=45, hjust=1, vjust=0.9, color = "black"),
  #           axis.text  = element_text(color = "black",size = 18),
  #           axis.title.y = element_text(angle=0,  hjust=0, vjust=0.6,size = 18), #y轴标题位置，vjust调整上下位置
  #           axis.line = element_line(colour = "black"),
  #           text = element_text(family = "serif"),
  #           legend.direction = "horizontal",
  #           legend.text  = element_text(size = 15), # 图例字体大小
  #           legend.key.size = unit(3, "lines"),
  #           legend.position="bottom"
  #           # legend.position=c(0.5,-0.15),
  #           # plot.margin=unit(c(1,1,2,1),'lines')
  #           )
  # 
  #   g_p
  # }

  g_p <- ggplot(data=plot.df) +
    geom_line(mapping = aes(x=year,y=True,linetype="北京"),size=0.8)+
    geom_line(mapping = aes(x=year,y=Predicted,linetype="合成北京"),alpha=1)+
    geom_vline(xintercept=2015.5,linetype=2,alpha=0.8)  + theme_classic() +
    scale_linetype_manual("", values=c(1,5),guide=guide_legend(override.aes=list(lwd=c(1,0.5))))+  # ！！！！value改线类型，1和2分别是实线和虚线！！！！！！！！！
    ylab("产\n业\n结\n构\n高\n级\n化") +
    xlab("") +
    scale_x_continuous(breaks=2010:2019+1,labels = unique(data$year)[seq(4,40,4)])+
    theme(axis.text.x  = element_text(angle=45, hjust=1, vjust=0.9, color = "black"),
          axis.text  = element_text(color = "black",size = 18),
          axis.title.y = element_text(angle=0,  hjust=0, vjust=0.6,size = 18), #y轴标题位置，vjust调整上下位置
          axis.line = element_line(colour = "black"),
          text = element_text(family = "serif"),
          # legend.direction = "horizontal",
          legend.text  = element_text(size = 15), # 图例字体大小
          legend.key.size = unit(3, "lines"),
          legend.position="bottom"
          # legend.position=c(0.15,0.9)
          # plot.margin=unit(c(1,1,2,1),'lines')
          )

  g_p
}

# 差值的图
g_pp <- ggplot(data=plot.df) +
  geom_vline(xintercept=2015.5,linetype=2)+ geom_hline(yintercept=0,linetype=2)  + theme_classic() +
  ylab("产\n业\n结\n构\n高\n级\n化\n差\n值") +
  xlab("") +
  scale_x_continuous(breaks=2010:2019+1,labels = unique(data$year)[seq(4,40,4)])+
  theme(axis.text.x = element_text(angle=45, hjust=1, vjust=0.9, color = "black"),
        axis.text  = element_text(color = "black",size = 18),
        axis.title.y = element_text(angle=0,  hjust=0, vjust=0.6,size = 18),
        text = element_text(family = "serif")
  )
g_p2 <- g_pp + geom_line(mapping = aes(x=year,y=True-Predicted),size=0.8)
g_p2



# -------------------- #
#    placebo           #
# -------------------- #
g_p3 = g_pp + geom_line(mapping = aes(x=year,y=True-Predicted,linetype="北京",alpha="北京"),size=0.8)
count = 0
pre_diff.df = data.frame(year=year)
pre_diff.df[,2:(length(city)+1)] = NA
pre_diff.df[,2] =  plot.df$True - plot.df$Predicted 
colnames(pre_diff.df)[-1] = city

for (i in 1:length(control)) {
  dataprep.out2 <- dataprep(
    foo = data,
    predictors=predictors,
    special.predictors=special.predictors,
    dependent=c(Y),
    unit.variable="ID",
    time.variable = "year_ID",
    time.predictors.prior = data$year_ID[1:tr_time-1],
    time.optimize.ssr = data$year_ID[1:tr_time-1],
    time.plot = unique(data$year_ID),
    treatment.identifier = control[i], 
    controls.identifier = control[-i],
    unit.names.variable = "city"
  )
  synth.out2 <- synth(dataprep.out2)
  
  plot.df2 = data.frame(year=year)
  plot.df2$True = as.numeric(dataprep.out2$Y1plot )
  plot.df2$Predicted = dataprep.out2$Y0plot %*% as.numeric(synth.out2$solution.w)
  
  pre_diff.df[,i+1] = plot.df2$True - plot.df2$Predicted  
  
  RMSPE.df[i+1,2] = sqrt(sum((plot.df2$True[1:tr_time-1]-plot.df2$Predicted[1:tr_time-1])^2)/(tr_time-1))
  RMSPE.df[i+1,3] = sqrt(sum((plot.df2$True[-1:-tr_time+1]-plot.df2$Predicted[-1:-tr_time+1])^2)/(length(year)-tr_time+1))
  
  if (RMSPE.df[i+1,2]<0.06) {
    count = count+1
    
    if (count == 1) {
      g_p3 = g_p3 + geom_line(data = plot.df2, mapping = aes(x=year,y=True-Predicted,linetype = "其他城市",alpha="其他城市"),size=0.5) #size=粗细，alpha=深浅
    }else{
      g_p3 = g_p3 + geom_line(data = plot.df2, mapping = aes(x=year,y=True-Predicted),linetype = 1,alpha=0.15,size=0.5) #size=粗细，alpha=深浅
      
    }
  }
  
}
# 自定义图例
g_p4 <- g_p3 + scale_alpha_manual("", breaks=c("北京","其他城市"), values=c(1,0.15))+
  scale_linetype_manual("", breaks=c("北京","其他城市"), values=c(1,1), guide=guide_legend(override.aes=list(lwd=c(1,0.5)))) +
  theme(  legend.text  = element_text(size = 15), # 图例字体大小
          legend.key.size = unit(3, "lines"),
          legend.position="bottom"
          # legend.position=c(0.15,0.9)
          )
g_p4



# 
# # 单个城市画图
# g_p2 +geom_line(data = pre_diff.df, mapping = aes(x=year,y=青岛),alpha=0.2,size=0.7)+ geom_hline(yintercept=0,linetype=2)+
#   scale_linetype_manual("", values=c(1,2),guide=guide_legend(override.aes=list(lwd=c(1,0.5)))) 
# 
# 
# # 柱状图
# g_p5 <-  ggplot(data=RMSPE.df) +
#   geom_bar(mapping = aes(x = city,y=RMSPE_pose/RMSPE),stat = "identity")+
#   theme(text = element_text(size=15)) +
#   ylab("RMSPE ratios") +
#   xlab("") +
#   theme(axis.text.x = element_text(angle=90, hjust=1, vjust=1,color="black"),
#         axis.line = element_line(colour = "black"),
#         legend.position="bottom")
# g_p5
# 
# RMSPE.df

