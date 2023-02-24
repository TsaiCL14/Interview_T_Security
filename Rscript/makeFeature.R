#### make feature 
## 針對品項 或 店家 製作他們可以使用的特徵
### 讀取資料
z <- readRDS(file = "data/Data.rds")
# z[which(z<0,arr.ind = TRUE)] <- NA

### 可以製作的特徵值主題 「品項、店家代號、日期」
output_item <- data.frame('type' = unique(z$品項))
output_store <- data.frame('store' = sort(unique(z$店家代號)))
output_date <- data.frame('date' = colnames(z)[-c(1,2)])

### 先個別計算品項跟店家中負數的數量 #####################

Under0OfItem <- aggregate(z[,-c(1,2)],by = list('type' = z$品項), FUN = function(w){
  sum(w[!is.na(w)]<0)
})
output_item$'負數次數' <- apply(Under0OfItem[,-1],1,sum)


######### 品項 特徵 ################
## 品項資料紀錄次數
notNAofitem <- aggregate(z[,-c(1,2)],by = list('type' = z$品項),FUN = function(w){
  sum(!is.na(w))
})
output_item$"資料紀錄筆數" <- apply(notNAofitem[,-1],1,sum)

######### 店家特徵 output_store ################
### 店家負數次數
Under0OfStore <- aggregate(z[,-c(1,2)],by = list('store' = z$店家代號), FUN = function(w){
  sum(w[!is.na(w)]<0)
})
output_store$'負數次數' <- apply(Under0OfStore[,-1],1,sum)
apply(Under0OfStore[,-1],2,FUN = sum) # 不同月份的負數次數

# 把負數的次數分開來看
Under0OfStoreByItem <- aggregate(z[,-c(1,2)],by = list('store' = z$店家代號,'item' = z$品項), FUN = function(w){
  sum(w[!is.na(w)]<0)
})
output_store$'品項A負數次數' <- 0
output_store$'品項B負數次數' <- 0
output_store$'品項C負數次數' <- 0
output_store$'品項D負數次數' <- 0
output_store$'品項E負數次數' <- 0
output_store$'品項F負數次數' <- 0
output_store$'品項G負數次數' <- 0
output_store$'品項H負數次數' <- 0
# Under0OfStoreByItem
for( i in 1:dim(output_store)[1]){
  ## 需要根據要輸出的資料表的店家代號來放
  useSTORE <- output_store$store[i]
  sum(Under0OfStoreByItem[(Under0OfStoreByItem$store == useSTORE)&(Under0OfStoreByItem$item == 'A'),-c(1,2)])
  output_store$'品項A負數次數'[i] <- sum(Under0OfStoreByItem[(Under0OfStoreByItem$store == useSTORE)&(Under0OfStoreByItem$item == 'A'),-c(1,2)])
  output_store$'品項B負數次數'[i] <- sum(Under0OfStoreByItem[(Under0OfStoreByItem$store == useSTORE)&(Under0OfStoreByItem$item == 'B'),-c(1,2)])
  output_store$'品項C負數次數'[i] <- sum(Under0OfStoreByItem[(Under0OfStoreByItem$store == useSTORE)&(Under0OfStoreByItem$item == 'C'),-c(1,2)])
  output_store$'品項D負數次數'[i] <- sum(Under0OfStoreByItem[(Under0OfStoreByItem$store == useSTORE)&(Under0OfStoreByItem$item == 'D'),-c(1,2)])
  output_store$'品項E負數次數'[i] <- sum(Under0OfStoreByItem[(Under0OfStoreByItem$store == useSTORE)&(Under0OfStoreByItem$item == 'E'),-c(1,2)])
  output_store$'品項F負數次數'[i] <- sum(Under0OfStoreByItem[(Under0OfStoreByItem$store == useSTORE)&(Under0OfStoreByItem$item == 'F'),-c(1,2)])
  output_store$'品項G負數次數'[i] <- sum(Under0OfStoreByItem[(Under0OfStoreByItem$store == useSTORE)&(Under0OfStoreByItem$item == 'G'),-c(1,2)])
  output_store$'品項H負數次數'[i] <- sum(Under0OfStoreByItem[(Under0OfStoreByItem$store == useSTORE)&(Under0OfStoreByItem$item == 'H'),-c(1,2)])
}
## 店家資料紀錄筆數：不是缺失值的資料共有幾筆

notNAofstore <- aggregate(z[,-c(1,2)],by = list('store' = z$店家代號,'item' = z$品項),FUN = function(w){
  sum(!is.na(w))
})
head(notNAofstore,20)

######### 日期特徵 output_date ################
### 負數次數
apply(Under0OfItem[,-1],2,sum)
apply(Under0OfStore[,-1],2,sum)
output_date$"負數次數" <- apply(Under0OfStore[,-1],2,sum)
### 有負數的店家數量
apply(Under0OfStore[,-1],2,FUN = function(w){sum(w>0)})
output_date$'負數店家數量' <- apply(Under0OfStore[,-1],2,FUN = function(w){sum(w>0)})
### 有負數的品項種類
output_date$'負數品項種類數量' <- apply(Under0OfItem[,-1],2,FUN = function(w){sum(w>0)})
########################## output data #########################
saveRDS(output_item,'data/FeatureOfItem.rds')
saveRDS(output_store,'data/FeatureOfStore.rds')
saveRDS(output_date,'data/FeatureOfDate.rds')