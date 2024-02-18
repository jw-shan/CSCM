rm(list=ls())
library(ggplot2)
library(openxlsx)




# load data
# sheet1 = 所有占比
# sheet2 = 重要产业占比
# sheet3 = 所有产值
data <- read.xlsx("22 三产各产业画图.xlsx",sheet=1)


# 数据处理
industry = colnames(data)[-1]
year = data$项.目
data.df = data.frame(year=rep(year,length(industry)),ratio=NA,industry=rep(industry,each=length(year)))
for (i in 1:length(industry)) {
  data.df$ratio[which(data.df$industry==industry[i])] = data[,i+1]
}

 
# 折线图 1橙 2紫 3焦黄 4 5蓝 6 7暗绿 8暗红 9 10 11 12焦黄 13 明蓝
mycolor = c("#66CCCC","#9900ff","#F00000","#990033","#0033ff","#FF9900","#00BFFF",
            "#800000","#333333","#006699","#BB3D00","#ff6600","#66CC99","#3CB371") #自定义14个颜色
ggplot(data=data.df,mapping = aes(year,ratio,color=industry,linetype=industry)) + 
  geom_line(size=1) + theme_classic()+
  ylab("服\n务\n业\n各\n产\n值\n占\n比") +
  xlab("")+
  scale_linetype_manual("",values=1:14)+ #线型,value是14个线型
  scale_color_manual("",values = mycolor)+
  theme(axis.text = element_text(color = "black",size = 18), # 坐标轴字体大小
          axis.title.y = element_text(angle=0,  hjust=0, vjust=0.6, size = 18),  #y轴标题位置和大小，vjust调整上下位置
          # text = element_text(family = "serif"),
          # legend.direction = "horizontal",
          legend.text  = element_text(size = 15), # 图例字体大小
          legend.key.size = unit(2, "lines")
          # plot.margin=unit(c(1,1,2,1),'lines')
    )

ggplot(data=data.df,mapping = aes(year,ratio,color=industry,linetype=industry)) + 
    geom_line(size=1) + theme_classic()+
    ylab("服\n务\n业\n各\n产\n值\n占\n比") +
    xlab("")+
    scale_linetype_manual("",values=1:14)+ #线型,value是14个线型
    scale_color_manual("",values = mycolor)+
    theme(axis.text = element_text(family="Times New Roman", color = "black",size = 18), # 坐标轴字体大小
          axis.title.y = element_text(angle=0,  hjust=0, vjust=0.6, family="serif",size = 18),  #y轴标题位置和大小，vjust调整上下位置
          # text = element_text(family = "serif"),
          # legend.direction = "horizontal",
          legend.text  = element_text(family="serif",size = 15), # 图例字体大小
          legend.key.size = unit(2, "lines")
          # plot.margin=unit(c(1,1,2,1),'lines')
    )




ggsave(
    filename = paste0("./fig/北京各服务业产值占比.png"), # 保存的文件名称。通过后缀来决定生成什么格式的图片
    width = 12,             # 宽
    height = 7,            # 高
    units = "in",          # 单位
    dpi = 300              # 分辨率DPI
)






# 柱状图1
data.plt = data.df[data.df$year%in%c(2019),]
data.plt$year = as.character(data.plt$year)
ggplot(data=data.plt,mapping = aes(y=ratio,x=reorder(industry,ratio))) + geom_bar(stat="identity",fill="grey",color="black") + theme_classic()+
  scale_y_continuous(expand = c(0,0),limits = c(0,0.25) )+
  labs(title=paste0(unique(data.plt$year),"年"), x="", y="在第三产业中占比")+
  theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1),
        plot.title = element_text(hjust = 0.5),
        legend.position="none")



# 柱状图2
data.plt = data.df[data.df$year%in%c(2019),]
data.plt$year = as.character(data.plt$year)
ggplot(data=data.plt,mapping = aes(y=ratio,x=year,fill=industry)) + geom_bar(stat="identity",position="dodge") + theme_classic()+
  labs(title="", x="", y="在第三产业中占比")+
  theme(plot.title = element_text(hjust = 0.5),
        legend.position="bottom")
