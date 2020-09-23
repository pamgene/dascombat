library(vsn)
library(bnshiny)
library(bnutil)
library(dplyr)
library(reshape2)
library(data.table)


rm(list = ls())
vsnr = function(df, normalization = TRUE){
  X = acast(df, rowSeq ~ colSeq)
  R = acast(df %>% filter(RefFactor == levels(RefFactor)[1]), rowSeq ~ colSeq)

  if(dim(X)[1] != dim(R)[1]){
    stop("Number of rows in data and reference do not match")
  }

  if(all(rownames(X) == rownames(R))){
    aRef = vsn2(R, calib = ifelse(normalization, "affine", "none"))
    aVsn = vsn2(X, reference = aRef, calib = ifelse(normalization, "affine", "none"))
    result = data.table(vsn = list(aVsn))
  } else {
    stop("IDs of data and reference do not match")
  }
  return(result)
}

vsnh = function(dt){
  H = attr(dt$vsn[[1]], "hx")
  rownames(H) = 1:dim(H)[1]
  aResult = melt(H)
  colnames(aResult) = c("rowSeq", "colSeq", "Hvsn")
  return(aResult)
}

load("./examples/NewX Run2 ETSdata.RData")
bndata = data$data
normalized.bndata = bndata %>%
  mutate(RefFactor = factor(Sample.name)) %>%
  mutate(RefFactor = relevel(RefFactor, ref = "REF")) %>%
  do(vsnr(.)) %>%
  do(vsnh(.))

sv.bndata =  bndata %>%
  mutate(RefFactor = factor(Sample.name)) %>%
  mutate(RefFactor = relevel(RefFactor, ref = "REF")) %>%
  do(shinyvsn::vsnr(.)) %>%
  do(shinyvsn::vsnh(.))


# print(all(normalized.bndata == sv.bndata))
# load("./examples/aCubeDump.RData")
#
# teststk = aCube
# S = acast(teststk, rowSeq ~ colSeq)
#
# bndata = data$data
# normalized.stkdata = teststk %>%
#   mutate(RefFactor = factor(Sample.name)) %>%
#   mutate(RefFactor = relevel(RefFactor, ref = "C3H+NT"  )) %>%
#   do(vsnr(.)) %>%
#   do(vsnh(.))
#
# sv.stkdata=  teststk %>%
#   mutate(RefFactor = factor(Sample.name)) %>%
#   mutate(RefFactor = relevel(RefFactor, ref = "C3H+NT")) %>%
#   do(shinyvsn::vsnr(.)) %>%
#   do(shinyvsn::vsnh(.))
#
# print(all(normalized.stkdata == sv.stkdata))

Href.db = read.delim("./doc/02-software specification/METHODCC REF database_200916085501.txt",skip = 1)
