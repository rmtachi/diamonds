#### daimond price data assimilation ####
# November 2, 2013


### data prep
data1 <- read.table(file='diamonds1.txt', header=TRUE, fill=TRUE,
                    colClasses=c('numeric',rep('factor',3),'numeric','factor'))

data2 <- read.table(file='diamonds2.txt', header=TRUE, fill=TRUE,
                   colClasses=rep( c('numeric',rep('factor',3)), 2) )

## eliminate sold data
data1 <- data1[data1$certificate != 'sold',]
data2 <- data2[data2$certificate != 'sold',]

## remove cut, fluorescence and size
## cut - all 3EX; fluorescence - not consistant; size - corr with carat
data1$cut <- NULL; data2$cut <- NULL
data2$fluorescence <- NULL; data2$size <- NULL

## combine datasets
data1$store <- 'ryu'; data2$store <- 'takara'
data <- merge(data1,data2,all=TRUE,sort=FALSE)

## correct factors
data$colour <- factor(data$colour, levels=c('J','I','H','G','F','E','D'),
                       ordered=TRUE)

data$clarity <- factor(data$clarity, ordered=TRUE,
  levels=c('SI2','SI1','VS2','VS1','VVS2','VVS1','IF','FL'))

data$store <- as.factor(data$store)

data$certificate <- as.character(data$certificate)
data$certificate[data$certificate != 'GIA'] <- 'none'
data$certificate <- factor(data$certificate, levels=c('none','GIA'),
                           labels=c('no','yes'))

## put cost first
temp <- data$cost
data$cost <- data$clarity; data$clarity <- data$colour
data$colour <- data$carat; data$carat <- temp
colnames(data) <- colnames(data)[c(4,1:3,5:6)]
rm(temp)

## cost in 10,0000 yen ($1000)
data$cost <- data$cost/100000

daimonds <- data
rm(data1,data2, data)