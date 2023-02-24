## see Data 
library(ggplot2)
library(visdat)
######## read Data ##########
useData <- readRDS("data/Data.rds")
str(useData)
# colnames(useData) <- c('Number',"item",c(colnames(useData)[-c(1:2)]))
### 查看一下 小於0的資料
useData[which(useData < 0,arr.ind = TRUE)]# 小於1的資料比數有78比
useData[which(useData < 0,arr.ind = TRUE)] <- NA # 暫且以NA把小於0的資料接替代掉
############################### Visualization ###################################
### vis_dat #################
## 資料的缺失值
vis_dat(useData, facet = 品項)
visdat::vis_miss(useData,facet = 品項)

### 個品項的銷售店家數量 #############
Data_item <- table(useData$品項) # 每一個品項的店家數量
Data_item <- as.data.frame(Data_item)
ggplot(Data_item,aes(x=Var1,y = Freq))+
  geom_bar(stat = 'identity')+
  geom_text(aes(label = Freq),vjust = -0.8)
 

####### 各品項的銷售平均 ####################
meanOfsale <- aggregate(useData[,-c(1,2)],by = list(useData$品項), FUN = function(w){
  mean(w,na.rm = TRUE)
})
meanOfsale
meanOfsale_PD <- data.frame("Group" = rep(meanOfsale$Group.1,each = dim(meanOfsale)[2]-1))
meanOfsale_PD$Group <- factor(meanOfsale_PD$Group)
meanOfsale_PD$date <- rep(colnames(meanOfsale)[-1],times = dim(meanOfsale)[1]) 
meanOfsale_PD$value <- as.vector(t(meanOfsale[,-1]))
### NaN 換成0
meanOfsale_PD$value[is.nan(meanOfsale_PD$value)] <- 0
meanOfsale_PD %>% 
  ggplot(aes(x = date,y = value, group = Group)) +
  geom_line(aes(colour = Group), size = 2)
###### 各品項的銷售summary #################
summaryOftiem <- aggregate(useData[,-c(1,2)],by = list(useData$品項), FUN = function(w){
  summary(w,na.rm = TRUE)
  })
head(summaryOftiem[2])
# 抓最大值來看
lapply(summaryOftiem[-1],FUN = function(w){
  w[,6]
})

## 有疑問的地方
# 資料中F、G、H接是在2021-10開始才販賣嗎?

### 店家的總數量 ##############
length(unique(useData$店家代號)) # 1032 
### 店家中銷售的銷售平均 (???) #################
# aggregate(useData[,-c(1,2)],by = list("店家代號" = useData$店家代號,'品項' = useData$品項),FUN = function(w){
aggregate(useData[,-c(1,2)],by = list("店家代號" = useData$店家代號),FUN = function(w){
  sum(w,na.rm = TRUE)
})

### 店家中資料NA數量 ##############
# 先針對A品項來看
itemA <- useData$品項 == 'A'
itemA_count <- apply(useData[itemA,-c(1,2)],1,FUN = function(w){
  sum(!is.na(w))
})
itemA_DF <- data.frame('店家代號' = useData[itemA,1],'計數' = itemA_count)
## 先訂一個標準，我只找資料筆數超過一半的店家來看
moreHalf_A <- itemA_DF$計數/(dim(useData)[2]-2) >= 0.5
sum(moreHalf_A) # 有241個店家
itemA_DF[moreHalf_A]
plot(density(itemA_DF$計數))
abline(v = mean(itemA_DF$計數),col = 'red')
abline(v = median(itemA_DF$計數),col = 'blue')
abline(v = 21*0.5,col = 'orange')

## 品項B
itemB <- useData$品項 == 'B'
itemB_count <- apply(useData[itemB,-c(1,2)],1,FUN = function(w){
  sum(!is.na(w))
})
itemB_DF <- data.frame('店家代號' = useData[itemB,1],'計數' = itemB_count)
## 先訂一個標準，我只找資料筆數超過一半的店家來看
moreHalf_B <- itemB_DF$計數/(dim(useData)[2]-2) >= 0.5
sum(moreHalf_B) # 有175個店家

## 品項C
itemC <- useData$品項 == 'C'
itemC_count <- apply(useData[itemC,-c(1,2)],1,FUN = function(w){
  sum(!is.na(w))
})
itemC_DF <- data.frame('店家代號' = useData[itemC,1],'計數' = itemC_count)
## 先訂一個標準，我只找資料筆數超過一半的店家來看
moreHalf_C <- itemC_DF$計數/(dim(useData)[2]-2) >= 0.5
sum(moreHalf_C) # 有195個店家

## 品項D
itemD <- useData$品項 == 'D'
itemD_count <- apply(useData[itemD,-c(1,2)],1,FUN = function(w){
  sum(!is.na(w))
})
itemD_DF <- data.frame('店家代號' = useData[itemD,1],'計數' = itemD_count)
## 先訂一個標準，我只找資料筆數超過一半的店家來看
moreHalf_D <- itemD_DF$計數/(dim(useData)[2]-2) >= 0.5
sum(moreHalf_D) # 有137個店家

## 品項E
itemE <- useData$品項 == 'E'
itemE_count <- apply(useData[itemE,-c(1,2)],1,FUN = function(w){
  sum(!is.na(w))
})
itemE_DF <- data.frame('店家代號' = useData[itemE,1],'計數' = itemE_count)
## 先訂一個標準，我只找資料筆數超過一半的店家來看
moreHalf_E <- itemE_DF$計數/(dim(useData)[2]-2) >= 0.5
sum(moreHalf_E) # 有3個店家

## 品項F
itemF <- useData$品項 == 'F'
itemF_count <- apply(useData[itemF,-c(1,2)],1,FUN = function(w){
  sum(!is.na(w))
})
itemF_DF <- data.frame('店家代號' = useData[itemF,1],'計數' = itemF_count)
## 先訂一個標準，我只找資料筆數超過一半的店家來看
moreHalf_F <- itemF_DF$計數/(dim(useData)[2]-2) >= 0.5
sum(moreHalf_F) # 有0個店家

## 品項G
itemG <- useData$品項 == 'G'
itemG_count <- apply(useData[itemG,-c(1,2)],1,FUN = function(w){
  sum(!is.na(w))
})
itemG_DF <- data.frame('店家代號' = useData[itemG,1],'計數' = itemG_count)
## 先訂一個標準，我只找資料筆數超過一半的店家來看
moreHalf_G <- itemG_DF$計數/(dim(useData)[2]-2) >= 0.5
sum(moreHalf_G) # 有0個店家

## 品項H
itemH <- useData$品項 == 'H'
itemH_count <- apply(useData[itemH,-c(1,2)],1,FUN = function(w){
  sum(!is.na(w))
})
itemH_DF <- data.frame('店家代號' = useData[itemH,1],'計數' = itemH_count)
## 先訂一個標準，我只找資料筆數超過一半的店家來看
moreHalf_H <- itemH_DF$計數/(dim(useData)[2]-2) >= 0.5
sum(moreHalf_H) # 有0個店家