# Get file of Human Activity Recognition Using Smartphones Dataset experiment,
# reading the files from test and trainner data, and create thre table:
# main table combining test train data, a second table with values de mean and 
# average only, and third table with the mean for each value of fisrt table by 
# activity and subject. 

rm(list=ls())
print ("Star...")

getlabel <- function(v,label) {
# Get the label description given the vuale readed in the table.        
        dd <- NULL
        for (i in seq_along(v)) {
                dd <- c(dd,label[v[i]])
        }
        dd
}

adjustlabel <- function(v) {
# Adjust the labels removing undesirable characters          
        dd <- c("subject", "activity")
        for (i in seq_along(v)) {
                t <- gsub("[\\(\\)]", "", v[i])
                dd <- c(dd,gsub("[-,]","",t))
        }
        dd
}

getmeans <- function(tab,lb) {
# Create the table of the means for each value by subject and activity
        df <- NULL
        for (i in 1:30) {
                tabi <- filter(tabla, subject==i)
                for (j in 1:6) {
                        sb <- lb[j]
                        sj <- filter(tabi, activity==sb)
                        meanj <- lapply(sj[3:563], mean)
                        dfj <- data.frame(subject=i,activity=lb[j],meanj)
                        df <- rbind(df,dfj)
                } 
        }
        df
} 

library(dplyr)
library(data.table)

lb <- c("walk", "walkingupstairs", "walking_downstairs", "sitting", "standing", "laying" )

print("Reading files...")

try <- read.table("UCI HAR Dataset//train//y_train.txt", header=F)
trs <- read.table("UCI HAR Dataset//train//subject_train.txt", header=F)
lrs <- getlabel(try[,1], lb)
trx <- read.table("UCI HAR Dataset//train//X_train.txt", header=F)
train <- data.frame("subject"=trs[,1],"activity"=lrs,trx[,1:561])
tsy <- read.table("UCI HAR Dataset//test//Y_test.txt", header=F)
lss <- getlabel(tsy[,1], lb)
tss <- read.table("UCI HAR Dataset//test//subject_test.txt", header=F)
tsx <- read.table("UCI HAR Dataset//test//X_test.txt", header=F)
test <- data.frame("subject"=tss[,1],"activity"=lss,tsx[,1:561])

print("Creating table..")

tabla <- merge(train, test, all = TRUE)
clb <- read.table("UCI HAR Dataset//features.txt")
u <- adjustlabel(clb[,2])
names(tabla) <- u

print("Creating mean and average table")
nms <- grep("mean|avg",names(tabla))
sel <- c(1,2,nms)
tabmeanavg <- tbl_df(tabla[sel])

print("Creating mean table for each column by label and subject")

meantab <- getmeans(tabla, lb)

print("todo ok")
