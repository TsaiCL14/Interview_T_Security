
########## get Data #############
### sheet 1 題目的說明
readxl::read_excel("data/大數據分析應用管理師_測試題目_蔡季倫.xlsx",sheet = 1)

### seet 2 資料內容
z <- readxl::read_excel("data/大數據分析應用管理師_測試題目_蔡季倫.xlsx",sheet = 2)
z <- as.data.frame(z)

head(z) 
########## 資料整理(認識) #############
## 第二列的資料是時間資料(欄位名稱)
## 整理出資料的時間
DateData <- as.POSIXct(as.numeric(z[2,-c(1,2)])*86400,origin = '1900-01-01')
# 我只需要前面1-7的字串
DateData <- substring(DateData,1,7) 

## 1. 第一列的資料是不需要的，那個資料應該是要放在資料的名稱中。
## 2. 第一欄的資料需要補齊。
unique(z[,1])
head(z[-c(1:2),1])
# 2. 第2列為欄位名稱，需要將資料換成date的形式
str(z[,2])
# 確認看看是不是有重複的店家代號 
head(table(z[,2]))# 確認店家代號是會有重複的

######### 把資料放到新的資料表中 命名為 output ############
# 把上面的資料一邊整理，一邊匯出一個新的資料(output)

## 先把後面的銷售數量放到output中
output <- z[-c(1:2),-c(1:2)]
colnames(output) <- DateData
head(output)
## 先把欄位重新排序
output <- output[,order(DateData)]
## (店家代號) 
Number <- as.character(as.numeric(z[-c(1:2),2])) 
# output <- cbind('店家代號' = as.character(as.numeric(z[-c(1:2),2])),output)
## (品項)
item <- character(dim(output)[1])
for(i in unique(z[-c(1:2),1])){
  if(!is.na(i)){
    print(which((z[-c(1:2),1]) == i))
    item[which((z[-c(1:2),1]) == i):length(item)] <- i
  }
}
### 放入output
output <- cbind('店家代號' = Number,'品項' = item ,output)

saveRDS(output,'data/Data.rds')
