## see Data 
library(ggplot2)
library(visdat)
######## read Data ##########
useData <- readRDS("data/Data.rds")
str(useData)
# colnames(useData) <- c('Number',"item",c(colnames(useData)[-c(1:2)]))
### vis_dat #################
visdat::vis_dat(useData,facet = item)
vis_dat(useData, facet = 品項)
visdat::vis_miss(useData,facet = 品項)
### 個品項的銷售店家數量 #############
Data_item <- table(useData$品項) # 每一個品項的店家數量
Data_item <- as.data.frame(Data_item)
ggplot(Data_item,aes(x=Var1,y = Freq))+
  geom_bar(stat = 'identity')+
  geom_text(aes(label = Freq),vjust = -0.8)
 

### 個品項的銷售平均
colnames(useData)
aggregate(useData[,-c(1,2)],by = list(useData$品項), FUN = function(w){
  mean(w,na.rm = TRUE)
})


### 店家的總數量
length(unique(useData$店家代號)) # 1032 
### 店家中銷售的銷售平均
aggregate(useData[,-c(1,2)],by = list("店家代號" = useData$店家代號),FUN = function(w){
  ## 去除月
  # apply(w,1,FUN = function(a) mean(a,na.rm = TRUE))
  ##
  sum(w,na.rm = TRUE)
})
