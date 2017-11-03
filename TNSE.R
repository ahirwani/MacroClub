#### T-SNE Dimensionality Reducing

source("C:/R/MAXIM/Tools/FunctionList.R")

#install.packages("Rtsne")
library(Rtsne)
train <- read.csv("C:/R/MAXIM/XLab/train.csv")

str(train)
head(train)

Labels <- train$label

train$label <- as.factor(train$label)

#for plotting
colors = rainbow(length(unique(train$label)))
names(colors) = unique(train$label)

tsne <- Rtsne(train[,-1], dims = 2, perplexity = 30, verbose = T, max_iter = 500)

plot(tsne$Y, t='n', main = "tsne")
text(tsne$Y, labels = train$label, col= colors[train$label])

#### SAMPLE 2
rm(list = ls())
train <- read.table("C:/R/MAXIM/XLab/samplewordembedding.csv", header = T, sep = ",")
#install.packages("tsne")
library(tsne)

#initialize counter to 0
x <- 0
epc <- function(x) {
  x <<- x+1
  filename <- paste("C:/R/MAXIM/Xlab/plot",x,"jpg",sep=".")
  cat("> Plotting TSNE to ",filename," ")
  
  #Plot to local file 2400x1800 dim
  jpeg(filename,width = 2400, height = 1800)
  
  plot(x, t='n', main = "T-SNE")
  text(x, labels=rownames(train.set))
  dev.off()
}

#run TSNE (maximum iterations: 500, callback every 100 epoch, target dimensions = 5)
tsne_data <- tsne(train,k=5, epoch_callback = epc, max_iter=500, epoch =100)


######

fit <- kmeans(train, 20)
library(cluster)
clusplot(train, fit$cluster, color = T, shade = T, labels = 2, lines = 0)

##### K MEANS CLUSTERING


data <- read.csv("C:/R/MAXIM/XLab/Wholesale customers data.csv", header = T)
summary(data)
str(data)
head(data)

top.n.custs <- function(data,cols, n=5) {
  idx.to.remove <- integer(0) #initialize a vector to hold
  for (c in cols){
    col.order <- order(data[,c],decreasing=T) #Sort column
    idx <- head(col.order,n) #Take first n of the sorted column c
    idx.to.remove <- union(idx.to.remove,idx) #combine de-duplicate the row ids that need to be removed
  }
  return(idx.to.remove)}

top.custs <- top.n.custs(data,cols=3:8,n=5)
length(top.custs)

data[top.custs,]
data.rm.top <- data[-c(top.custs),]

k <- kmeans(data.rm.top[-c(1,2)],centers=5) #create 5 clusters, remove columns 1 and 2

k$centers
table(k$cluster)

#Try other values for K
rng <- 2:20
tries <- 100 #Runs k means 100 times
avg.totw.ss <- integer(length(rng)) #set up an empty vector to hold all of points

v<-3
i <- 1
for(v in rng){
  v.totw.ss <- integer(tries)
  for(i in 1:tries){
    k.temp <- kmeans(data.rm.top,centers = v) #run kmeans
    v.totw.ss[i] <- k.temp$tot.withinss #Store the total withnss
  }
  avg.totw.ss[v-1] <- mean(v.totw.ss) #Average the 100 total withinss
}

plot(rng,avg.totw.ss,type="b",ylab="Avg Total WithinSS",xlab = "Value of K")


### TRY YAHOO DATA

snp.tickers <- read.csv("C:/R/MAXIM/XLab/SNPtickers.csv")

head(snp.tickers)

start.date <- "2000-01-01"
end.date <- format(Sys.Date(),"%Y-%m-%d")

full.ticker.list <- snp.tickers$Ticker
todo.ticker.list <- full.ticker.list 
todo.ticker.list <- todo.ticker.list[!todo.ticker.list %in% c("BRK.B","BF.B")]

#Get Blank DataFrame
tmp.stock.data <- data.loading("AAPL",start.date, end.date)
full.data.list <- as.data.table(tmp.stock.data)
full.data.list$Date <- rownames(tmp.stock.data$open)

#Get Data for Each Ticker
done.ticker.list <- NULL

for (itick in todo.ticker.list[1:100]){
  print(paste("trying:",itick))
  
  tmp.stock.data <- data.loading(itick,start.date, end.date)
  print(paste("downloaded:",itick))
  
  stock.data <- as.data.table(tmp.stock.data)
  head(stock.data)
  
  stock.data$dod <- stock.data$close - shift(stock.data$close,1,type="lag")
  stock.data$logprice <- log(stock.data$close)
  stock.data$logdif <- stock.data$logprice - shift(stock.data$logprice,1,type="lag")
  
  colnames(stock.data) <- paste(itick,"-",colnames(stock.data),sep="")
  stock.data$Date <- rownames(tmp.stock.data$open)
  
  full.data.list <- merge(full.data.list,stock.data,by="Date")
  head(full.data.list)
  
  todo.ticker.list <- todo.ticker.list[todo.ticker.list != itick]
  done.ticker.list <- rbind(done.ticker.list,itick)
  
  print(paste("DONE:",itick))
}

#Construct the DataFrame
#Clean if anytime series
full.data.list <- full.data.list[colSums(!is.na(full.data.list)) > 0]
full.data.list <- full.data.list[complete.cases(full.data.list),]
head(full.data.list)

full.data.list <- data.frame(full.data.list)
nrow(full.data.list)

train.set <- NULL
#itick <- done.ticker.list[1]
for(itick in done.ticker.list){
  print(itick)
  logdif <- full.data.list[which(colnames(full.data.list)==paste(itick,".logdif",sep=""))]
  if(length(logdif)>0){
  colnames(logdif) <- itick
  train.set <- rbind(train.set,t(logdif))}
  
  #logprice <- full.data.list[which(colnames(full.data.list)==paste(itick,".logprice",sep=""))]
  #if(length(logprice)>0){
  #colnames(logprice) <- itick
  #train.set <- rbind(train.set,t(logprice))}
  
  #logvol <- full.data.list[which(colnames(full.data.list)==paste(itick,".volume",sep=""))]
  #if(length(logvol)>0){
  #colnames(logvol) <- itick
  #train.set <- rbind(train.set,t(logvol))}
  
  print(paste("DONE:",itick))
}

rownames(train.set) <- gsub(".logdif","", rownames(train.set), fixed = T)

#run TSNE (maximum iterations: 500, callback every 100 epoch, target dimensions = 5)
tsne_data <- tsne(train.set,k=5, epoch_callback = epc, max_iter=500, epoch =100)

dev.off()
par(mfrow=c(1,1))
plot(tsne_data,type='n',main='tsne')
text(tsne_data, labels = rownames(train.set), col= colors[train$label])


tsne <- Rtsne(train.set, dims = 2, perplexity = 10, verbose = T, max_iter = 500)

plot(tsne$Y, t='n', main = "tsne"); 
text(tsne$Y, labels = rownames(train.set), col= colors[train$label])

#ccEMB <- Reduce(function(...) merge(...,all=TRUE, by = "DATE"), list(ccEMBI,snp,usMAC,ccM2,ccVOL, ccLIBOR, usLIBOR, deLIBOR, usVIX, usTRE, ccGDP, usGDP, deGDP, usEPS, usPOL))


stock.data <- data.loading(ticker.list,start.date, end.date)
analyze.data <- as.data.table(stock.data)
#See Last Entry
analyze.data[length(stock.data$close)-1,]

summary(stock.data)



####################### GET NASDAQ STATISTICS

file.etf.opt <- file.etf[file.etf$in_options_list == TRUE,]; nrow(file.etf.opt);
file.etf.non.opt <- file.etf[file.etf$in_options_list == FALSE,]; nrow(file.etf.non.opt);

length(unique(file.groups$groupSymbol))
file.groups.etf <- file.groups[file.groups$ETF == "Y",]
file.groups.stock <- file.groups[file.groups$ETF=="N",]
length(file.groups.etf$Symbol); nrow(file.groups.stock);

###### ANALYSE HOW FULL THE ETF and COMPUSTAT DATA IS

compustat.col.list <- colnames(file.compustat)
etf.col.list <- colnames(file.etf)

etf.stats <- data.frame(Col.Name = character(), Col.Length = double(), Col.Na = double(), Perc.Na = double(), Col.Blank = double(), Perc.Blank = double())
compustat.stats <- data.frame(Col.Name = character(), Col.Length = double(), Col.Na = double(), Perc.Na = double(), Col.Blank = double(), Perc.Blank = double())

icol <- "gvkey"

for(icol in compustat.col.list){
  temp.data <- file.compustat[,which(colnames(file.compustat)==icol)]
  temp.na <- sum(is.na(temp.data))
  temp.blank <- sum(temp.data != "")
  temp.count <- length(temp.data)
  
  temp.add <- data.frame(Col.Name = icol, Col.Length = temp.count, Col.Na = temp.na, Perc.Na = temp.na/temp.count, Col.Blank = temp.err, Perc.Blank = temp.err/temp.count)
  compustat.stats <- rbind(compustat.stats, temp.add)  
}


########################## Groupings with Dan ######################

setwd("C:/Users/Maxim/Documents/University/MBA/Spring 2017/XLab/Grouping")

file.groups <- read.csv("allTickers.csv")
file.compustat <- read.csv("Compustat.csv")
file.etf <- read.csv("ETF_OPT2.csv")

file.bbg <- read.csv("BidAskData_noBBG.csv")

colnames(file.bbg)[1] <- "Symbol"
head(file.bbg)
#Rename to do Merge
library(data.table)
file.bbg <- as.data.table(file.bbg)
file.bbg$BidAsk <- file.bbg$PX_ASK - file.bbg$PX_BID
file.bbg$BidAskPerc <- file.bbg$BidAsk/file.bbg$PX_MID

colnames(file.compustat)[which(colnames(file.compustat)=="tic")] <- "Symbol"

#New Entries to Compare
file.compustat$MarketValue <- file.compustat$at + file.compustat$prcc_c * file.compustat$csho - file.compustat$ceq
file.compustat$DebtToMarket <- file.compustat$dltt/file.compustat$MarketValue
file.compustat$Leverage <- file.compustat$dltt/file.compustat$ceq
file.compustat$AssetTurnover <- file.compustat$revt/file.compustat$at
file.compustat$EPS <- file.compustat$revt/file.compustat$csho
file.compustat$SGARevenue <- file.compustat$xsga/file.compustat$revt

head(file.compustat); colnames(file.compustat)
head(file.etf); colnames(file.etf);

#Unique Groups Determined
group.list <- unique(file.groups$groupSymbol); length(group.list);

#Do Randomization

for (iseed in seq(500,500000,1)){
#set.seed(2413)
set.seed(iseed)

treatment.list <- sample(group.list,length(group.list)/2)
head(treatment.list)
#write.csv(treatment.list,file="Seed2413Treatment.csv")
length(treatment.list)
control.list <- group.list[-which(group.list %in% treatment.list)]
head(control.list)
length(control.list)
#write.csv(control.list,file="Seed2413Control.csv")


file.groups$treatment <- ifelse(file.groups$groupSymbol %in% treatment.list,TRUE,FALSE)

file.merge <- Reduce(function(...) merge(...,all=TRUE, by = "Symbol"), list(file.groups,  file.bbg))
#file.merge <- Reduce(function(...) merge(...,all=TRUE, by = "Symbol"), list(file.groups, file.compustat,file.etf, file.bbg))
#write.csv(file.merge,"Seed2413-All.csv")

head(file.merge)

#Separate the Treatment and Control
treatment.merge <- file.merge[file.merge$treatment ==TRUE,]
head(treatment.merge)
control.merge <- file.merge[file.merge$treatment == FALSE,]
head(control.merge)


source("C:/R/MAXIM/Tools/FunctionList.R")

var.chart <- c("AssetTurnover","SGARevenue","gp","VOLATILITY_30D","CUR_MKT_CAP")

var.BBG <- c("BidAsk","BidAskPerc","VOLUME")

#Pick Metrics that Matter
var.total.list <- c("MarketValue","DebtToMarket","Leverage","AssetTurnover","EPS","SGARevenue","emp","revt","gp",
                    "FUND_NET_ASSET_VAL","VOLATILITY_30D","VOLUME_AVG_20D","CUR_MKT_CAP","in_options_list")

#DataFrame for F-Tests & Kolmogorov Smirnov Tests
df.Random <- data.frame(feat.name = character(), treat.mean = double(), treat.sd = double(), treat.count = double(),
                        treat10p = double(), treat25p = double(), treat50p = double(), treat75p = double(), treat90p = double(),
                        control.mean = double(), control.sd = double(), control.count = double(),
                        contro10p = double(), control25p = double(), control50p = double(), control75p= double(), control90p = double(),
                        t.stat = double(), t.pval = double(), f.sig = logical(), ks.stat = double(), ks.sig = logical())

plot.chart <- FALSE; 
#plot.chart <- TRUE
#par(mfrow=c(2,3))

log.transform <- TRUE; #log.transform <- FALSE
bucket.KS <- TRUE;

#ivar <- "BidAskPerc"

for(ivar in var.BBG){
#for( ivar in var.total.list){

  treat.data <- treatment.merge[,which(colnames(file.merge)==ivar)]
  if(ivar != "in_options_list"){
  treat.data <- clean.data(treat.data,log.transform) } else {treat.data <- clean.data(treat.data,FALSE)}
  head(treat.data)
  
  treat.mean <- mean(treat.data)
  treat.sd <- sd(treat.data)
  treat.qq <- quantile(treat.data,c(0.1,0.25,0.5,0.75,0.9))
  
  control.data <- control.merge[,which(colnames(file.merge)==ivar)]
  if(ivar != "in_options_list"){
  control.data <- clean.data(control.data,log.transform) } else {control.data <- clean.data(control.data,FALSE)}
  head(control.data)
  
  control.mean <- mean(control.data)
  control.sd <- sd(control.data)
  control.qq <- quantile(control.data,c(0.1,0.25,0.5,0.75,0.9))
  
  if(bucket.KS){
    control.hist <- hist(control.data,breaks = 100, plot=FALSE)
    treat.hist <- hist(treat.data,breaks = 100, plot=FALSE)
    dif.test <- ks.test(control.hist$counts,treat.hist$counts)
    ks.stat <- dif.test$p.value
    ks.sig <- abs(ks.stat) < 0.05
    test.val <- round(dif.test$p.value,4)
  }else{
    dif.test <- ks.test(control.data,treat.data)
    ks.stat <- dif.test$p.value
    ks.sig <- abs(ks.stat) < 0.05
    test.val <- round(dif.test$p.value,4)
  }
  
  #Now do F-test
  dif.sd <- sqrt((control.sd^2)/length(control.data)+(treat.sd^2)/length(treat.data))
  dif.mean <- treat.mean - control.mean
  t.stat <- dif.mean/dif.sd
  #Is the dif.stat SIGNIFICANT? p less than 0.05?
  t.pval <- abs(pt(t.stat,length(control.data)))
  f.sig <- abs(pt(t.stat,length(control.data))) < 0.05
  
  if(plot.chart){
    
    #Alt-L to collaps/ Alt-Shift-L to expand
    #Plot Data
    plot(density(control.data),main = paste("Density Charts of ",ivar), col="red",lwd=2)
            #plot(density(control.data), xlim=c(treat.mean-treat.sd*0.5,treat.mean+treat.sd*4),main = paste("Density Charts of ",ivar), col="red",lwd=2)
    lines(density(treat.data), col="blue",lwd=2)
            #lines(density(treat.data), xlim=c(treat.mean-treat.sd*0.5,treat.mean+treat.sd*4), col="blue",lwd=2)
    legend("topleft",c("Treatment","Control"),col=c("blue","red"),bty="n",pch=c(21,21), pt.bg=c("blue","red"))
    legend("topright",c(paste("Kolmogorov Test:",test.val),paste("F-Test:",round(t.pval,5)))
                        ,bty="n",pch=c(21,21,21), pt.bg=c("blue","blue","blue"))
    
    #Now Plot the Smoothed Buckets
    plot(control.hist$mids, control.hist$density, main = paste("HIST Density Charts of ",ivar), col="red",lwd=2, type = "l")
    lines(treat.hist$mids, treat.hist$density,  col="blue",lwd=2)
    legend("bottomleft",c("Treatment","Control"),col=c("blue","red"),bty="n",pch=c(21,21), pt.bg=c("blue","red"))
    legend("bottomright",c(paste("Kolmogorov Test:",test.val),paste("F-Test:",round(t.pval,5)))
           ,bty="n",pch=c(21,21), pt.bg=c("blue","blue"))
    
    if(log.transform == FALSE){
      #Do Log Plot
      plot(control.density$x, log(control.density$y), main = paste("LOG Density Charts of ",ivar), col="red",lwd=2, type = "l")
      #plot(control.density$x, log(control.density$y), xlim=c(treat.mean-treat.sd*0.5,treat.mean+treat.sd*4),main = paste("LOG Density Charts of ",ivar), col="red",lwd=2, type = "l")
      #lines(treat.density$x, log(treat.density$y), xlim=c(treat.mean-treat.sd*0.5,treat.mean+treat.sd*4), col="blue",lwd=2)
      lines(treat.density$x, log(treat.density$y),  col="blue",lwd=2)
      legend("bottomleft",c("Treatment","Control"),col=c("blue","red"),bty="n",pch=c(21,21), pt.bg=c("blue","red"))
      legend("bottomright",c(paste("Kolmogorov Test:",test.val),paste("Mean Control:",round(control.mean,0)," SD.Control:",round(control.sd,0)),
                          paste("Mean Treat:",round(treat.mean,0)," Mean SD:",round(treat.sd,0)))
             ,bty="n",pch=c(21,21,21), pt.bg=c("blue","blue","blue"))
      }
    
  }
  
  df.Row <- data.frame(feat.name = ivar, treat.mean = treat.mean, treat.sd = treat.sd, treat.count = length(treat.data),
                       treat10p = treat.qq[1], treat25p = treat.qq[2], treat50p = treat.qq[3], treat75p = treat.qq[4], treat90p = treat.qq[5],
                          control.mean = control.mean, control.sd = control.sd, control.count = length(control.data),
                       contro10p = control.qq[1], control25p = control.qq[2], control50p = control.qq[3], control75p= control.qq[4], control90p = control.qq[5],
                          t.stat = t.stat, t.pval = t.pval, f.sig = f.sig, ks.stat = ks.stat, ks.sig = ks.sig)
  
  
  
  df.Random <- rbind(df.Random, df.Row)
#plot2dens(treat.data,control.data)
}

df.meansame.f <- sum(df.Random$f.sig == FALSE); df.meandif.f <- sum(df.Random$f.sig == TRUE);
df.distsame.ks <- sum(df.Random$ks.sig == FALSE); df.distdif.ks <- sum(df.Random$ks.sig == TRUE);

if(df.meansame.f == 3 && df.distsame.ks ==3){ break;}

#if(df.meansame.f == 14 && df.distsame.ks ==14){ break;}

}

#Get Names of Same Means
df.meansame <- df.Random[df.Random$f.sig == FALSE,]
df.meansame$feat.name
#Get Names of Diff Means
df.meandif <- df.Random[df.Random$f.sig == TRUE,]
df.meandif$feat.name

#Get Names of Same Distributions
df.distsame <- df.Random[df.Random$ks.sig==FALSE,]
df.distsame$feat.name
#Get Names of Diff Distributions
df.distdif <- df.Random[df.Random$ks.sig==TRUE,]
df.distdif$feat.name


#Counts
df.meansame.f; df.meandif.f; df.distsame.ks; df.distdif.ks;

write.csv(df.Random,file="Seed508Summary.csv")

#length(control.data)

#qf(.975,df1=length(treat.data),df2=length(control.data))


iseed








temp.x <- log(as.numeric(treat.data))
temp.x[!is.finite(temp.x)] <- NA
temp.x <- na.omit(temp.x)
temp.x <- as.numeric(gsub("#N/A",NA,temp.x))
temp.x <- as.numeric(gsub("N/A",NA,temp.x))
temp.x <- na.omit(temp.x)

clean.data <- function(x, log.transform){
  
  #Do Log Transform
  if(log.transform){
    x <- log(as.numeric(x))
    x[!is.finite(x)] <- NA
  }
  x <- na.omit(x)
  x <- as.numeric(gsub("#N/A",NA,x))
  x <- as.numeric(gsub("N/A",NA,x))
  x <- na.omit(x)
  
  return (x);
}