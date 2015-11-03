setwd("./recommender")
library(data.table); library(reshape2); library(dplyr); library(XLConnect)

# read raw data from the Excel workbook
wb <- loadWorkbook("Assignment 3.xls")
ws <- readWorksheet(wb, 1)

# create an empty matrix
corr <- matrix(nrow = 25, ncol = 25)
row.names(corr) <- names(ws)[-1]
colnames(corr) <- names(ws)[-1]

# create the Pearson correlation table where the diagonals are 1's
for(i in 1:25){
        for(j in 1:25) corr[i,j] = cor(ws[,i+1], ws[,j+1], use = "complete")
}

# check to see if my correlation table is done correctly
sort(corr[colnames(corr) == "X3712",], decreasing = TRUE)[2:6]

# check user X3712's top three movies by taking the correlation-weighted average of ratings

# 1. function to find top five users similar to a given user
neighbors <- function(id = "X3712", corr.table = corr){
        sim <- sort(corr[colnames(corr) == id], decreasing = TRUE)[2:6]
        names(sim) <- names(sort(corr[colnames(corr) == id,], decreasing = TRUE)[2:6])
        sim
}

neighbors()

# 2. funciton to calculate predicted movie ratings for a given user

# function to calculate correlation-weighted averages
corrWeighted <- function(tops, rtgs, avg.id){
        good <- !is.na(rtgs)
        sum(tops[good]*rtgs[good])/sum(tops[good])
}

norScores <- function(tops, rtgs, avg.id){
        good <- !is.na(rtgs)
        avgs <- colMeans(ws[, names(tops)], na.rm = TRUE)
        avg.id + sum((rtgs[good]-avgs[good])*tops[good])/sum(tops[good])
}

pred.tops <- function(id = "X3712", movies = ws, method = corrWeighted){
        # obtain top five users similar to a given user
        tops <- neighbors(id)
        
        # create a small movie ratings table for the top five neighbors of 5 columns
        small <- movies[, names(movies) %in% names(tops)]
        # reorder the columns according to the column order of tops
        small <- small[, c(names(tops))]
        rownames(small) <- movies$MOVIES
        
        avg.id = mean(movies[,id], na.rm = TRUE)
        pred.ratings <- apply(small, MARGIN = 1, function(x){method(tops, x, avg.id)})
        sort(pred.ratings, decreasing = TRUE)[1:5]
}

pred.tops("X3712")
pred.tops("X3525")
pred.tops("X89")
pred.tops("X3867")

pred.tops("X3712", method = norScores)
pred.tops("X3525", method = norScores)
pred.tops("X3867", ws, method = norScores)
pred.tops("X89", ws, method = norScores)
