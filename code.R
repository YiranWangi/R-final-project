setwd("/Users/wangyiran/Desktop/R语言节课论文")
options(digits=8)
# packages
library(tidyverse)
library(caret)
library(glmnet)
library(viridis)
library(gbm)
library(rpart)
library(rpart.plot)
library(stargazer)
library(ggridges)
library(ggthemes)
library(Hmisc)#describe
library(fBasics)#basicStats
library(GGally)#相关性分析
library(readr)
library(dplyr)
library(showtext)
library(cowplot)
library(ggplot2)
library(ggthemes)
library(hrbrthemes)
library(car)
library(ggpubr)
library(ggsci)



# load  data
NYdata <- read.csv("CleanData.csv",header=TRUE,fileEncoding = 'GBK')
NYdata$社区组 <- factor(NYdata$社区组)
NYdata$社区 <- factor(NYdata$社区)
NYdata$房屋类型 <- factor(NYdata$房屋类型)
summary(NYdata)




describe(NYdata)




basicStats(NYdata$价格)
basicStats(NYdata$最短租房时间)
basicStats(NYdata$纬度)
basicStats(NYdata$经度)



n <- nrow(NYdata)
k <- round(1 + ((10/3) * log10(n)))

labels <- c(
  '      1  |—|  78.125', 
  ' 78.125   —|  156.250', 
  ' 156.250  —|  234.375', 
  ' 234.375  —|  312.500', 
  ' 312.500  —|  390.625', 
  ' 390.625  —|  468.750', 
  ' 468.750  —|  546.875', 
  ' 546.875  —|  625.000', 
  ' 625.000  —|  703.125', 
  ' 703.125  —|  781.250', 
  ' 781.250  —|  859.375', 
  ' 859.375  —|  937.500', 
  ' 937.500  —|  1015.625', 
  ' 1015.625 —|  1093.750', 
  ' 1093.750 —|  1171.875', 
  ' 1171.875 —|  1250')

freq_nights <- cbind(Frequency=table(cut(x = NYdata$最短租房时间, breaks = k, labels = labels, include.lowest=T)), Percent = prop.table(table(cut(x = NYdata$最短租房时间, breaks = k, labels = labels, include.lowest=T))) * 100)
freq_nights

#制作频率分布直方图
tema <- theme(plot.background = element_rect(fill = "white", color = "#FFFAFA"),
              plot.title = element_text(size = 23, hjust = .5),
              axis.text.x = element_text(size = 19, face = "bold"),
              axis.text.y = element_text(size = 19, face = "bold"),
              axis.title.x = element_text(size = 19),
              axis.title.y = element_text(size = 19),
              legend.position = "none")

options(repr.plot.width=14, repr.plot.height=6)
a <- ggplot(data = NYdata, mapping = aes(x = 最短租房时间)) +
  geom_histogram(fill = "#CC99FF", bins = 70, size = 0.5, color = "#666666") +
  theme_minimal() +
  ylab("频数") +
  xlab("最短租房时间") +
  ggtitle("最短租房时间｜Histogram") +
  tema


df <- data.frame(minimum_nights = NYdata["最短租房时间"][NYdata["最短租房时间"] <= 40])
b <- ggplot(data = df, mapping = aes(x = minimum_nights)) +
  geom_histogram(fill = "#6699FF", bins = 70, size = 0.5, color = "#666666") +
  theme_minimal() +
  ylab("频数") +
  xlab("最短租房时间") +
  ggtitle("最短租房时间 <= 40 | Histogram") +
  tema

plot_grid(a, b, ncol=2, nrow=1)






n <- nrow(NYdata)
k <- round(1 + ((10/3) * log10(n)))

labels <- c(
  '       0 |—|  625.000', 
  ' 625.000  —|  1250.00', 
  ' 1250.00  —|  1875.00', 
  ' 1875.00  —|  2500.00', 
  ' 2500.00  —|  3125.00', 
  ' 3125.00  —|  3750.00', 
  ' 3750.00  —|  4375.00', 
  ' 4375.00  —|  5000.00', 
  ' 5000.00  —|  5625.00', 
  ' 5625.00  —|  6250.00', 
  ' 6250.00  —|  6875.00', 
  ' 6875.00  —|  7500.00', 
  ' 7500.00  —|  8125.00', 
  ' 8125.00  —|  8750.00', 
  ' 8750.00  —|  9375.00', 
  ' 9375.00  —|  10000.0')

freq_price <- cbind(Frequency=table(cut(x = NYdata$价格, breaks = k, labels = labels, include.lowest=T)), Percent = prop.table(table(cut(x = NYdata$价格, breaks = k, labels = labels, include.lowest=T))) * 100)
freq_price

#制作频率分布直方图
tema <- theme(plot.background = element_rect(fill = "white", color = "#FFFAFA"),
              plot.title = element_text(size = 23, hjust = .5),
              axis.text.x = element_text(size = 19, face = "bold"),
              axis.text.y = element_text(size = 19, face = "bold"),
              axis.title.x = element_text(size = 19),
              axis.title.y = element_text(size = 19),
              legend.position = "none")

options(repr.plot.width=14, repr.plot.height=6)
a <- ggplot(data = NYdata, mapping = aes(x = 价格)) +
  geom_histogram(fill = "#FFCC66", bins = 70, size = 0.5, color = "#CCCCCC") +
  theme_minimal() +
  ylab("频数") +
  xlab("价格") +
  ggtitle("价格｜Histogram") +
  tema


df <- data.frame(price = NYdata["价格"][NYdata["价格"] <= 1000])
b <- ggplot(data = df, mapping = aes(x = price)) +
  geom_histogram(fill = "#66CC99", bins = 70, size = 0.5, color = "#CCCCCC") +
  theme_minimal() +
  ylab("频数") +
  xlab("价格") +
  ggtitle("价格 <= 1000 | Histogram") +
  tema

plot_grid(a, b, ncol=2, nrow=1)





#1.均值
means <- data.frame(Mean = c(mean(NYdata$价格), mean(NYdata$最短租房时间)))
row.names(means) <- c("价格", "最短租房时间")
means
#2.中位数
medians <- data.frame(Median = c(median(NYdata$价格), median(NYdata$最短租房时间)))
row.names(medians) <- c("价格", "最短租房时间")
medians



Moda <- function(x){
  freq <- table(x)
  return(names(freq)[freq == max(freq)])
}

#模态是测量数据集高度的一种方法，还有平均值和中值。它可以用样本和总体的方式来定义。相对于第一种采样模式，数据集的采样模式处理的是数据集中出现频率最高或最常见的值。
mod <- data.frame(Mode = c(Moda(NYdata$价格), Moda(NYdata$最短租房时间)))
row.names(mod) <- c("价格", "最短租房时间")
mod






tema <- theme(
  plot.title = element_text(size = 17, hjust = .5),
  axis.text.x = element_text(size = 15, face = "bold"),
  axis.text.y = element_text(size = 15, face = "bold"),
  axis.title.x = element_text(size = 15),
  axis.title.y = element_text(size = 15),
  legend.text = element_text(size = 9, face = "bold"))

options(repr.plot.width=15, repr.plot.height=10)
options(warn=-1)
df <- data.frame(price = NYdata["价格"][NYdata["价格"] <= 750])
a <- ggplot(data = df, mapping = aes(x = price)) +
  geom_density(fill = "cyan", size = 1.3, color = "black") +
  geom_vline(xintercept = mean(NYdata$价格), size = 1.5, linetype="dashed", color = "black") +
  geom_vline(xintercept = median(NYdata$价格), size = 1.5, linetype="dashed", color = "red") +
  geom_vline(xintercept = as.numeric(Moda(NYdata$价格)), size = 1.5, linetype="dashed", color = "blue") +
  annotate("text", label="Mean = 165.43", x = 320, y = .0085, color = "black", size=5, fontface = "bold") +
  annotate("text", label="Median = 110", x = 350, y = .0070, color = "red", size=5, fontface = "bold") +
  annotate("text", label="Mode = 150", x = 370, y = .0055, color = "blue", size=5, fontface = "bold") +
  theme_ipsum() +
  ylab("") +
  xlab("价格") +
  ggtitle("价格 <= 750 | Density") +
  tema

df <- data.frame(minimum_nights = NYdata["最短租房时间"][NYdata["最短租房时间"] <= 32])
b <- ggplot(data = df, mapping = aes(x = minimum_nights)) +
  geom_density(fill = "yellow", size = 1.3, color = "black") +
  geom_vline(xintercept = mean(NYdata$最短租房时间), size = 1.5, linetype="dashed", color = "black") +
  geom_vline(xintercept = median(NYdata$最短租房时间+0.1), size = 1.5, linetype="dashed", color = "red") +
  geom_vline(xintercept = as.numeric(Moda(NYdata$最短租房时间)), size = 1.5, linetype="dashed", color = "blue") +
  annotate("text", label="Mean = 22.13", x = 12, y = .33, color = "black", size=5, fontface = "bold") +
  annotate("text", label="Median = 30", x = 14, y = .28, color = "red", size=5, fontface = "bold") +
  annotate("text", label="Mode = 30", x = 15, y = .23, color = "blue", size=5, fontface = "bold") +
  theme_ipsum() +
  ylab("") +
  xlab("最短租房时间") +
  ggtitle("最短租房时间 <= 32 | Density") +
  tema
plot_grid(a, b, ncol=2, nrow=1)






#1.均值
means <- data.frame(Mean = c(mean(NYdata$纬度), mean(NYdata$经度)))
row.names(means) <- c("纬度", "经度")
means
#2.中位数
medians <- data.frame(Median = c(median(NYdata$纬度), median(NYdata$经度)))
row.names(medians) <- c("纬度", "经度")
medians



Moda <- function(x){
  freq <- table(x)
  return(names(freq)[freq == max(freq)])
}

#模态是测量数据集高度的一种方法，还有平均值和中值。它可以用样本和总体的方式来定义。相对于第一种采样模式，数据集的采样模式处理的是数据集中出现频率最高或最常见的值。
mod <- data.frame(Mode = c(Moda(NYdata$纬度), Moda(NYdata$经度)))
row.names(mod) <- c("纬度", "经度")
mod



tema <- theme(
  plot.title = element_text(size = 17, hjust = .5),
  axis.text.x = element_text(size = 15, face = "bold"),
  axis.text.y = element_text(size = 15, face = "bold"),
  axis.title.x = element_text(size = 15),
  axis.title.y = element_text(size = 15),
  legend.text = element_text(size = 9, face = "bold"))

options(repr.plot.width=15, repr.plot.height=10)
options(warn=-1)
df <- data.frame(纬度 = NYdata["纬度"])
a <- ggplot(data = df, mapping = aes(x = 纬度)) +
  geom_density(fill = "#CCFFCC", size = 1.0, color = "#CCCCCC") +
  geom_vline(xintercept = mean(NYdata$纬度), size = 1.5, linetype="dashed", color = "black") +
  geom_vline(xintercept = median(NYdata$纬度), size = 1.5, linetype="dashed", color = "red") +
  geom_vline(xintercept = as.numeric(Moda(NYdata$纬度)), size = 1.5, linetype="dashed", color = "blue") +
  annotate("text", label="Mean = 40.729", x = 40.6, y = 4, color = "black", size=3, fontface = "bold") +
  annotate("text", label="Median = 40.725", x = 40.62, y = 3, color = "red", size=3, fontface = "bold") +
  annotate("text", label="Mode = 40.764", x = 40.65, y = 2, color = "blue", size=3, fontface = "bold") +
  theme_ipsum() +
  ylab("") +
  xlab("纬度") +
  ggtitle("纬度 | Density") +
  tema

df <- data.frame(经度 = NYdata["经度"])
b <- ggplot(data = df, mapping = aes(x = 经度)) +
  geom_density(fill = "#CCFFFF", size = 1.3, color = "#CCCCCC") +
  geom_vline(xintercept = mean(NYdata$经度), size = 1.5, linetype="dashed", color = "black") +
  geom_vline(xintercept = median(NYdata$经度+0.1), size = 1.5, linetype="dashed", color = "red") +
  geom_vline(xintercept = as.numeric(Moda(NYdata$经度)), size = 1.5, linetype="dashed", color = "blue") +
  annotate("text", label="Mean = -73.949", x = -74.15, y = 7, color = "black", size=3, fontface = "bold") +
  annotate("text", label="Median = -73.955", x = -74.13, y = 5, color = "red", size=3, fontface = "bold") +
  annotate("text", label="Mode = -73.994", x = -74.12, y = 3, color = "blue", size=3, fontface = "bold") +
  theme_ipsum() +
  ylab("") +
  xlab("经度") +
  ggtitle("经度 | Density") +
  tema
plot_grid(a, b, ncol=2, nrow=1)




freq_location <- data.frame(cbind(Frequency = table(NYdata$社区组), Percent = prop.table(table(NYdata$社区组)) * 100))
freq_location <- freq_location[order(freq_location$Frequency),]
freq_location

freq_area <- data.frame(cbind(Frequency = table(NYdata$社区), Percent = prop.table(table(NYdata$社区)) * 100))
freq_area <- freq_area[order(freq_area$Frequency),]
tail(freq_area,10)

freq_type <- data.frame(cbind(Frequency = table(NYdata$房屋类型), Percent = prop.table(table(NYdata$房屋类型)) * 100))
freq_type <- freq_type[order(freq_type$Frequency),]
freq_type





mean_room_type <- aggregate(list(average_price = NYdata$价格), list(room_type = NYdata$房屋类型), mean)
mean_room_type$Percent <- prop.table(mean_room_type$average_price) * 100
mean_room_type

tema <- theme(
  plot.title = element_text(size = 23, hjust = .5),
  axis.text.x = element_text(size = 19, face = "bold"),
  axis.text.y = element_text(size = 19, face = "bold"),
  axis.title.x = element_text(size = 19),
  axis.title.y = element_text(size = 19),
  legend.position = "none")


tema1 <- theme(
  plot.title = element_text(size = 23, hjust = .5),
  axis.text.x = element_text(size = 13, face = "bold"),
  axis.text.y = element_text(size = 13, face = "bold"),
  axis.title.x = element_text(size = 13),
  axis.title.y = element_text(size = 13),
  legend.position = "none")

options(repr.plot.width=15, repr.plot.height=6)
options(warn=-1)
a <- ggplot(data = mean_room_type, aes(x=room_type, y=average_price)) +
  coord_flip() +
  geom_segment(aes(xend=room_type, yend=0, color = room_type), size = 2) +
  geom_point(size=7, mapping = aes(color = room_type)) +
  theme_minimal() +
  xlab("") +
  ylab("") +
  ggtitle("Average price per room type") +
  tema
plot_grid(a, ncol=1, nrow=1)




tema1 <- theme(
  plot.title = element_text(size = 17, hjust = .5),
  axis.text.x = element_text(size = 12, face = "bold"),
  axis.text.y = element_text(size = 12, face = "bold"),
  axis.title.x = element_text(size = 13),
  axis.title.y = element_text(size = 13))


df <- data.frame(price = NYdata["价格"][NYdata["价格"] <= 1000], room_type = NYdata["房屋类型"][NYdata["价格"] <= 1000])
c <- ggplot(data = df, mapping = aes(x = price, fill = room_type)) +
  geom_density(mapping = aes(fill = room_type), bins = 70, size = 1.3, color = "black", alpha = .6, size = 1.5) +
  theme_minimal() +
  ylab("频率") +
  xlab("价格") +
  ggtitle("价格 <= 1000 | Histogram") +
  tema1 +
  theme(legend.position="bottom", legend.text = element_text(colour="black", size=20, face="bold"))

plot_grid(c, ncol=1, nrow=1)






tema <- theme(plot.background = element_rect(fill = "#EEE8AA", color = "yellow"),
              plot.title = element_text(size = 23, hjust = .5),
              axis.text.x = element_text(size = 19, face = "bold"),
              axis.text.y = element_text(size = 19, face = "bold"),
              axis.title.x = element_text(size = 19),
              axis.title.y = element_text(size = 19),
              legend.position = "none")

tema1 <- theme(plot.background = element_rect(fill = "#EEE8AA", color = "yellow"),
               plot.title = element_text(size = 23, hjust = .5),
               axis.text.x = element_text(size = 15, face = "bold"),
               axis.text.y = element_text(size = 15, face = "bold"),
               axis.title.x = element_text(size = 15),
               axis.title.y = element_text(size = 15),
               legend.position = "none")

options(repr.plot.width=12, repr.plot.height=6)
a <- ggplot(data = freq_type, mapping = aes(x = Frequency, y = row.names(freq_type))) +
  geom_bar(stat = "identity", mapping = aes(fill = row.names(freq_type), color = row.names(freq_type)), alpha = .7, size = 1.1) +
  geom_label(mapping = aes(label=Frequency), fill = "#006400", size = 5, color = "white", fontface = "bold", hjust=.7) +
  ylab("") +
  ggtitle("房屋类型分布直方图") +
  theme_economist() +
  tema

b <- ggplot(data = freq_type, aes(x = row.names(freq_type), y = Frequency)) +
  geom_bar(stat = "identity", mapping = aes(fill = row.names(freq_type), color = row.names(freq_type)), alpha = .7, size = 1.1) +
  theme_economist() +
  xlab("") +
  ylab("") +
  ggtitle("房屋类型极坐标图") +
  tema1

plot_grid(a, b + coord_polar(), ncol=2, nrow=1)







tema <- theme(plot.title = element_text(size = 23, hjust = .5),
              axis.text.x = element_text(size = 10, angle=15, face = "bold"),
              axis.text.y = element_text(size = 19, angle=10, face = "bold"),
              axis.title.x = element_text(size = 19),
              axis.title.y = element_text(size = 19),
              legend.text = element_text(size = 14, face = "bold"))

df <- data.frame(neighbourhood = row.names(tail(freq_area, 10)), Frequency = tail(freq_area, 10)$Frequency)

options(repr.plot.width=15, repr.plot.height=6)
ggplot(data = df, mapping = aes(x = reorder(neighbourhood,Frequency), y = Frequency)) +
  theme_minimal() + 
  geom_point(size = 6, color = "green") +
  xlab("") +
  ylab("社区") +
  geom_line(color = "black", size = 2, linetype= 16, group = 1, alpha = .5) + 
  geom_bar(stat = "identity", mapping = aes(fill = neighbourhood, color = neighbourhood), alpha = .8, size = 1.5) +   
  tema 






tema <- theme(plot.background = element_rect(fill = "#DCFCE6", color = "#66CDAA"),
              plot.title = element_text(size = 23, hjust = .5),
              axis.text.x = element_text(size = 19, face = "bold"),
              axis.text.y = element_text(size = 19, face = "bold"),
              axis.title.x = element_text(size = 19),
              axis.title.y = element_text(size = 19),
              legend.position = "none")

tema1 <- theme(plot.background = element_rect(fill = "#DCFCE6", color = "#66CDAA"),
               plot.title = element_text(size = 23, hjust = .5),
               axis.text.x = element_text(size = 15, face = "bold"),
               axis.text.y = element_text(size = 15, face = "bold"),
               axis.title.x = element_text(size = 15),
               axis.title.y = element_text(size = 15),
               legend.position = "none")


options(repr.plot.width=12, repr.plot.height=6)
a<- ggplot(data = freq_location, aes(x=row.names(freq_location), y=Frequency)) +
  geom_bar(stat = "identity", mapping = aes(fill = row.names(freq_location), color = row.names(freq_location)), alpha = .7, size = 1.1) +
  geom_label(mapping = aes(label=Frequency), fill = "#006400", size = 5, color = "white", fontface = "bold", hjust=.7) +
  coord_flip() +
  theme_economist() +
  xlab("") +
  ylab("频数") +
  ggtitle("社区组分布直方图") +
  tema

b <- ggplot(data = freq_location, aes(x=row.names(freq_location), y=Frequency)) +
  geom_bar(stat = "identity", mapping = aes(fill = row.names(freq_location), color = row.names(freq_location)), alpha = .7, size = 1.1) +
  theme_economist() +
  xlab("") +
  ylab("") +
  ggtitle("社区组分布极坐标图") +
  tema1

plot_grid(a, b + coord_polar(), ncol=2, nrow=1)



tema <- theme(
  plot.title = element_text(size = 23, hjust = .5),
  axis.text.x = element_text(size = 19, face = "bold"),
  axis.text.y = element_text(size = 19, face = "bold"),
  axis.title.x = element_text(size = 19),
  axis.title.y = element_text(size = 19),
  legend.text = element_text(size = 14, face = "bold"))

options(repr.plot.width = 14, repr.plot.height=10)
a <- ggplot(data = NYdata, aes(y="", x = 价格)) +
  geom_violin(size=1.1, color = "black", fill = "cyan", alpha = .7) +
  geom_vline(xintercept=median(NYdata$价格), size =1.5, color = "black") +
  theme_minimal() +
  coord_flip() +
  ggtitle("价格 | Violin") +
  ylab("") +
  xlab("") +
  tema

b <- ggplot(data = NYdata, mapping = aes(y = "", x = 最短租房时间)) +
  geom_violin(size=1.1, color = "black", fill = "yellow", alpha = .7) +
  geom_vline(xintercept=median(NYdata$最短租房时间), size =1.5, color = "black") +
  coord_flip() +
  theme_minimal() +
  ggtitle("最短租房时间 | Violin") +
  ylab("") +
  xlab("") +
  tema

df <- data.frame(price = NYdata["价格"][NYdata["价格"] <= 750])
c <- ggplot(data = df, mapping = aes(y = "", x = price)) +
  geom_violin(size=1.1, color = "black", fill = "cyan", alpha = .7) +
  geom_vline(xintercept=median(NYdata$价格), size =1.5, color = "black") +
  coord_flip() +
  ggtitle("价格 <= 750 | Violin") +
  theme_minimal() +
  ylab("") +
  xlab("") +
  tema

df <- data.frame(minimum_nights = NYdata["最短租房时间"][NYdata["最短租房时间"] <= 32])
d <- ggplot(data = df, mapping = aes(y = "", x = minimum_nights)) +
  geom_violin(size=1.1, color = "black", fill = "yellow", alpha = .7) +
  geom_vline(xintercept=median(NYdata$最短租房时间), size =1.5, color = "black") +
  coord_flip() +
  ggtitle("最短租房时间 <= 32 | Violin") +
  theme_minimal() +
  ylab("") +
  xlab("") +
  tema

plot_grid(a, b, c, d, ncol=2, nrow=2)




ggpairs(NYdata1, columns = c("纬度","经度","对数价格","最短租房时间"), ggplot2::aes(colour=房屋类型)) #房屋类型进行划分

ggpairs(NYdata1, columns = c("纬度","经度","对数价格","最短租房时间"), ggplot2::aes(colour=社区组)) #社区组进行划分





ggscatterhist(
  NYdata, x ='最短租房时间', y = '价格', shape=21,fill="#7FFFD4",color = "black",size = 3, alpha = 1,
  #palette = c("#00AFBB", "#E7B800", "#FC4E07"),
  margin.params = list( fill="red",color = "blue", size = 0.3,alpha=1),
  margin.plot =  "histogram",
  legend = c(0.8,0.8),
  ggtheme = theme_minimal())

a <- lm(价格~最短租房时间,NYdata)
summary(a)




a <- ggscatterhist(
  NYdata, x ='经度', y = '价格', shape=21,fill="#FF9966",color = "black",size = 3, alpha = 1,
  #palette = c("#00AFBB", "#E7B800", "#FC4E07"),
  margin.params = list( fill="#66FFCC",color = "#FF0066", size = 0.3,alpha=1),
  margin.plot =  "histogram",
  legend = c(0.8,0.8),
  ggtheme = theme_minimal())

b <- ggscatterhist(
  NYdata, x ='纬度', y = '价格', shape=21,fill="#CCFF33",color = "black",size = 3, alpha = 1,
  #palette = c("#00AFBB", "#E7B800", "#FC4E07"),
  margin.params = list( fill="#9999FF",color = "#009933", size = 0.3,alpha=1),
  margin.plot =  "histogram",
  legend = c(0.8,0.8),
  ggtheme = theme_minimal())
plot_grid(a, b , ncol=1, nrow=1)




mean_room_type <- aggregate(list(average_price = NYdata$价格), list(room_type = NYdata$房屋类型), mean)
mean_room_type$Percent <- prop.table(mean_room_type$average_price) * 100
mean_room_type

tema <- theme(
  plot.title = element_text(size = 17, hjust = .5),
  axis.text.x = element_text(size = 10, face = "bold"),
  axis.text.y = element_text(size = 10, face = "bold"),
  axis.title.x = element_text(size = 10),
  axis.title.y = element_text(size = 10),
  legend.position = "none")


tema1 <- theme(
  plot.title = element_text(size = 17, hjust = .5),
  axis.text.x = element_text(size = 10, face = "bold"),
  axis.text.y = element_text(size = 10, face = "bold"),
  axis.title.x = element_text(size = 10),
  axis.title.y = element_text(size = 10),
  legend.position = "none")

options(repr.plot.width=15, repr.plot.height=6)
options(warn=-1)
a <- ggplot(data = mean_room_type, aes(x=room_type, y=average_price)) +
  coord_flip() +
  geom_segment(aes(xend=room_type, yend=0, color = room_type), size = 2) +
  geom_point(size=7, mapping = aes(color = room_type)) +
  theme_minimal() +
  xlab("") +
  ylab("") +
  geom_label(mapping = aes(label=round(average_price,2)),  size = 3., color = "black",fontface = "bold", hjust=1.5) +
  ggtitle("Average price per room type") +
  tema

b <- ggplot(data = mean_room_type, aes(x=room_type, y=average_price)) +
  geom_segment(aes(xend=room_type, yend=0, color = room_type), size = 2) +
  geom_point(size=5, mapping = aes(color = room_type)) +
  theme_minimal() +
  xlab("") +
  ylab("") +
  ggtitle("Average price per room type | Polar") +
  tema1

plot_grid(a, b + coord_polar(), ncol=2, nrow=1)








top_10_neighbourhood <- aggregate(list(NYdata$价格), list(NYdata$社区), mean)
colnames(top_10_neighbourhood) <- c("neighbourhood", "Average_price_per_neighborhood")
top_10_neighbourhood <- top_10_neighbourhood[order(top_10_neighbourhood$Average_price_per_neighborhood),]
top_10_neighbourhood <- tail(top_10_neighbourhood, 12)
top_10_neighbourhood <- head(top_10_neighbourhood, 10)
r <- c()
for(i in 10:1){r <- c(r, i)}
row.names(top_10_neighbourhood) <- r
top_10_neighbourhood


tema <- theme(plot.background = element_rect(fill = "white"),
              plot.title = element_text(size = 21,hjust = .5),
              axis.text.x = element_text(size = 17, face = "bold"),
              axis.text.y = element_text(size = 17, face = "bold"),
              axis.title.x = element_text(size = 19),
              axis.title.y = element_text(size = 19),
              legend.position = "none")

na <- c("Tribeca", "Briarwood", "Flatiron District", "Riverdale", "Todt Hill", "Theater District", "Navy Yard", "Midtown", "SoHo", "Greenwich Village")
df <- data.frame(neighbourhood = NYdata$社区[NYdata$社区 == na], price = NYdata$价格[NYdata$社区 == na])

ggplot(data = df, mapping = aes(x = price, y = neighbourhood)) +
  geom_density_ridges(mapping = aes(fill = neighbourhood), bandwidth = 130, alpha = .6, size = 1.5) +
  theme_economist() +
  xlab("价格") +
  ylab("") +
  ggtitle("价格最高的前十组社区") +
  tema




top_10_neighbourhood <- aggregate(list(NYdata$价格), list(NYdata$社区), mean)
colnames(top_10_neighbourhood) <- c("neighbourhood", "Average_price_per_neighborhood")
top_10_neighbourhood <- top_10_neighbourhood[order(top_10_neighbourhood$Average_price_per_neighborhood),]
top_10_neighbourhood <- tail(top_10_neighbourhood, 12)
top_10_neighbourhood <- head(top_10_neighbourhood, 10)
r <- c()
for(i in 10:1){r <- c(r, i)}
row.names(top_10_neighbourhood) <- r



tema <- theme(plot.title = element_text(size = 20, hjust = .5),
              axis.text.x = element_text(size = 19, angle=15, face = "bold"),
              axis.text.y = element_text(size = 19, angle=10, face = "bold"),
              axis.title.x = element_text(size = 19),
              axis.title.y = element_text(size = 19),
              legend.position = "none")

options(repr.plot.width=15, repr.plot.height=6.5)
a <- ggplot(data = top_10_neighbourhood, mapping = aes(x = reorder(neighbourhood,Average_price_per_neighborhood), y = Average_price_per_neighborhood)) +
  geom_bar(stat = "identity", mapping = aes(fill = neighbourhood, color = neighbourhood), alpha = .8, size = 1) +
  geom_label(mapping = aes(label = round(Average_price_per_neighborhood, 2)), size = 6, fill = "#F5FFFA", fontface = "bold") +
  theme_ipsum() + 
  ggtitle("") +
  xlab("") +
  ylab("") +
  tema
a



top_10_neighbourhood_b <- aggregate(list(NYdata$价格), list(NYdata$社区), mean)
colnames(top_10_neighbourhood_b) <- c("neighbourhood", "Average_price_per_neighborhood")
top_10_neighbourhood_b <- top_10_neighbourhood_b[order(top_10_neighbourhood_b$Average_price_per_neighborhood),]
top_10_neighbourhood_b <- head(top_10_neighbourhood_b, 10)
r <- c()
for(i in 1:10){r <- c(r, i)}
row.names(top_10_neighbourhood_b) <- r
top_10_neighbourhood_b

tema <- theme(plot.title = element_text(size = 20, hjust = .5),
              axis.text.x = element_text(size = 19, angle=15, face = "bold"),
              axis.text.y = element_text(size = 19, angle=10, face = "bold"),
              axis.title.x = element_text(size = 19),
              axis.title.y = element_text(size = 19),
              legend.position = "none")

options(repr.plot.width=15, repr.plot.height=6.5)
a <- ggplot(data = top_10_neighbourhood_b, mapping = aes(x = reorder(neighbourhood,Average_price_per_neighborhood), y = Average_price_per_neighborhood)) +
  geom_bar(stat = "identity", mapping = aes(fill = neighbourhood, color = neighbourhood), alpha = .8, size = 1.5) +
  geom_label(mapping = aes(label = round(Average_price_per_neighborhood, 2)), size = 6, fill = "#F5FFFA", fontface = "bold") +
  theme_ipsum() + 
  ggtitle("") +
  xlab("") +
  ylab("") +
  tema
a




neighbourhood_group <- aggregate(list(NYdata$价格), list(NYdata$社区组), mean)
colnames(neighbourhood_group) <- c("neighbourhood_group", "Average_price_per_neighborhood_group")
neighbourhood_group$Average_price_per_neighborhood_group <- prop.table(neighbourhood_group$Average_price_per_neighborhood_group) * 100
neighbourhood_group 

tema <- theme(
  plot.title = element_text(size = 18, hjust = .5),
  axis.text.x = element_text(size = 14, face = "bold"),
  axis.text.y = element_text(size = 14, face = "bold"),
  axis.title.x = element_text(size = 16),
  axis.title.y = element_text(size = 16),
  legend.position = "none")

tema1 <- theme(
  plot.title = element_text(size = 20, hjust = .5),
  axis.text.x = element_text(size = 10, face = "bold"),
  axis.text.y = element_text(size = 10, face = "bold"),
  axis.title.x = element_text(size = 9),
  axis.title.y = element_text(size = 9),
  legend.position="none")

options(repr.plot.width=12, repr.plot.height=11)
a <- ggplot(data = neighbourhood_group, mapping = aes(x = neighbourhood_group, y = Average_price_per_neighborhood_group)) +
  geom_bar(stat = "identity", mapping = aes(fill = neighbourhood_group, color = neighbourhood_group), alpha = .8, size = 1.5) +
  geom_label(mapping = aes(label = round(Average_price_per_neighborhood_group, 2)), size = 3, fill = "#F5FFFA", fontface = "bold") +
  coord_flip() +
  theme_ipsum() + 
  ggtitle("社区组平均价格") +
  xlab("") +
  ylab("") +
  tema

b <- ggplot(data =  neighbourhood_group, mapping = aes(x = neighbourhood_group, y = Average_price_per_neighborhood_group)) +
  geom_bar(stat = "identity", mapping = aes(fill = neighbourhood_group, color = neighbourhood_group), alpha = .8, size = 1.5) +
  theme_ipsum() + 
  ggtitle("社区组平均价格|极坐标图") +
  xlab("") +
  ylab("") +
  tema1

plot_grid(a, b + coord_polar(), ncol=2, nrow=1)



# load  data
NYdata1 <- read.csv("CleanData.csv",header=TRUE,fileEncoding = 'GBK')
summary(NYdata1)



NYdata1 %>% mutate(对数价格= log(价格+1)) %>% ggplot(mapping = aes(x = 对数价格,y= 房屋类型,fill = 房屋类型)) +
  geom_density_ridges( alpha=0.6,size = 1.3, color = "black")



tema <- theme(
  plot.title = element_text(size = 17, hjust = .5),
  axis.text.x = element_text(size = 15, face = "bold"),
  axis.text.y = element_text(size = 15, face = "bold"),
  axis.title.x = element_text(size = 15),
  axis.title.y = element_text(size = 15),
  legend.text = element_text(size = 9, face = "bold"))

options(repr.plot.width=15, repr.plot.height=10)
options(warn=-1)
df <- data.frame(price = 对数价格)
a <- ggplot(data = df, mapping = aes(x = price)) +
  geom_density(fill = "#53868B",alpha=0.7, size = 1.3, color = "black") +
  theme_ipsum() +
  ylab("") +
  xlab("对数价格") +
  ggtitle("对数价格 | Density") +
  tema
a

qqnorm(对数价格)
qqline(对数价格)




NYdata1 %>% ggplot(aes(x=经度,y=纬度, color=log(价格+1))) +geom_point()+
  scale_colour_gradient(
    low = "orange",
    high = "blue4",
    space = "Lab",
    na.value = "grey50",
    guide = "colourbar",
    aesthetics = "colour"
  )+labs(color="价格（对数）")


NYdata1 %>% ggplot(aes(x=经度,y=纬度, color=log(价格+1))) +geom_point()+
  scale_colour_gradient(
    low = "orange",
    high = "blue4",
    space = "Lab",
    na.value = "grey50",
    guide = "colourbar",
    aesthetics = "colour"
  )+
  facet_wrap(~房屋类型)+labs(color="价格（对数）")




NYdata1 %>% ggplot(aes(x=经度,y=纬度, color=log(价格+1))) +geom_point()+
  scale_colour_gradient(
    low = "orange",
    high = "blue4",
    space = "Lab",
    na.value = "grey50",
    guide = "colourbar",
    aesthetics = "colour"
  )+facet_wrap(~社区组)+labs(color="价格（对数）")



set.seed(1234)
NYdata1 <- NYdata1 %>% mutate(对数价格= log(价格+1)) 
df_mat <- NYdata1[c('对数价格','房屋类型','社区组' , '最短租房时间','经度' ,'纬度')]

train_num <- createDataPartition(df_mat$对数价格,p=0.7)
train_set <- df_mat[train_num$Resample1,]
test_set <- df_mat[-train_num$Resample1,]
dim(train_set)
dim(test_set)



lm1 <- lm(对数价格~房屋类型,data=NYdata1)
summary(lm1)

lm2 <- lm(对数价格~社区组,data=NYdata1)
summary(lm2)

lm3 <- lm(对数价格~房屋类型+社区组,data=NYdata1)
summary(lm3)

stargazer(lm1,lm2,lm3,type = "text")



anova(lm1,lm2,lm3)



lm5 <- lm(对数价格~房屋类型+社区组 + 最短租房时间,data=NYdata1)
summary(lm5)
lm6 <- lm(对数价格~房屋类型+社区组 + 经度 ,data=NYdata1)
summary(lm6)
lm7 <- lm(对数价格~房屋类型+社区组 + 纬度,data=NYdata1)
summary(lm7)
lm8 <- lm(对数价格~房屋类型+社区组 + 最短租房时间+经度 + 纬度,data=NYdata1)
summary(lm8)
lm9 <- lm(对数价格~房屋类型+社区组 + 最短租房时间+经度 + 纬度 + I(纬度^2),data=NYdata1)
summary(lm9)
stargazer(lm5,lm6,lm7,lm8,lm9,type = "text")




anova(lm5,lm6,lm7,lm8,lm9)





# 建模并计算rmse
lm_mod <- lm(对数价格~房屋类型+社区组 + 最短租房时间+经度 + 纬度 + I(纬度^2),data=train_set)





rpart_mod <- rpart(对数价格~.,data=train_set,control=rpart.control(xval=10, minsplit=2))
summary(rpart_mod)
rpart.plot(rpart_mod)




# 筛选最小误差
opt<-which.min(rpart_mod$cptable[,"xerror"])

# 选择最优剪枝点
cp<-rpart_mod$cptable[opt,"CP"]


plot(rpart_mod$cptable[,c(1,4)],type="o")
abline(v=cp)




rpart_imp <- varImp(rpart_prune)
rpart_imp %>% mutate(Feature = row.names(rpart_imp)) %>% ggplot(aes(x= fct_reorder(Feature,Overall),y=Overall)) + geom_bar(stat = 'identity') + coord_flip()+labs(x="",title = "决策树变量重要性排序")



train_set$房屋类型 <- factor(train_set$房屋类型)
train_set$社区组 <- factor(train_set$社区组)
set.seed(1234)
boost_mod <-  gbm(对数价格~.,
                      data = train_set,
                      distribution='gaussian',
                      n.trees = 5000,
                      interaction.depth = 6,
                      shrinkage = 0.01,
                      cv.folds = 5
)
summary(boost_mod)
bi <- gbm.perf(boost_mod,method="cv")
# choose best trees
bi




boost_imp <- varImp(boost_mod,numTrees=bi)
boost_imp %>% mutate(Feature = row.names(rpart_imp)) %>% ggplot(aes(x= fct_reorder(Feature,Overall),y=Overall)) + geom_bar(stat = 'identity') + coord_flip()+labs(x="",title = "boosting变量重要性排序")





# 预测
pred.lm <- predict(lm_mod,newdata=test_set)
rmse.lm <- sqrt(mean((pred.lm - test_set$对数价格)^2))
pred.rpart <- predict(rpart_prune,newdata=test_set)
rmse.rpart <- sqrt(mean((pred.rpart - test_set$对数价格)^2))
pred.boost <- predict(boost_mod,newdata=test_set,n.trees=bi)
rmse.boost <- sqrt(mean((pred.boost - test_set$对数价格)^2))

cop_df <- data.frame(model=c("Linear Regression","Desicion Tree","Boosting"),RMSE_log = c(rmse.lm,rmse.rpart,rmse.boost))
cop_df <- cop_df %>% mutate(rmse_prob = exp(RMSE_log)-1)
cop_df

