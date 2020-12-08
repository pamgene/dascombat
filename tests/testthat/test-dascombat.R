library(testthat)
context("test-dascombat")
library(dascombat)
library(bnutil)

library(reshape2)
library(dplyr)


test_that("dascombat::fit vs sva::ComBat", {
  df = dascombat::combat_testdf
  
  Y = acast(df, rowSeq~colSeq, value.var = "value")
  bx = acast(df, rowSeq~colSeq, value.var = "RunID")[1,]
  bx = factor(bx)
    
  sva.cMod = sva::ComBat(Y, bx, mean.only  = TRUE, ref.batch = NULL)
  model = dascombat::fit(Y, bx, mean.only = TRUE)
  cMod = dascombat::applyModel(Y,model)
  expect_true(all(round(cMod,8) - round(sva.cMod,8) == 0))
  
  sva.cMod = sva::ComBat(Y, bx, mean.only  = FALSE, ref.batch = NULL)
  model = dascombat::fit(Y, bx, mean.only = FALSE)
  cMod = dascombat::applyModel(Y,model)
  expect_true(all(round(cMod,8) - round(sva.cMod,8) == 0))
  
  #########################
  
  ref.batch = "1"
  
  sva.cMod = sva::ComBat(Y, bx, mean.only  = TRUE, ref.batch = ref.batch)
  model = dascombat::fit(Y, bx, mean.only = TRUE, ref.batch = ref.batch)
  cMod = dascombat::applyModel(Y,model)
  expect_true(all(round(cMod,8) - round(sva.cMod,8) == 0))
  
  sva.cMod = sva::ComBat(Y, bx, mean.only  = FALSE, ref.batch = ref.batch)
  model = dascombat::fit(Y, bx, mean.only = FALSE, ref.batch = ref.batch)
  cMod = dascombat::applyModel(Y,model)
  expect_true(all(round(cMod,8) - round(sva.cMod,8) == 0))
})

test_that("fit.ref.batch.not.found", {
  df = dascombat::combat_testdf
  
  Y = acast(df, rowSeq~colSeq, value.var = "value")
  bx = acast(df, rowSeq~colSeq, value.var = "RunID")[1,]
  bx = factor(bx)
  
  ref.batch.error = "something missing"
  
  expect_false(ref.batch.error %in% bx)
  
  expect_error(dascombat::fit(Y, bx, mean.only = TRUE, ref.batch = ref.batch.error),
               message="fit.ref.batch.not.found")
  
})

test_that("apply with new batch variable", {
  df = dascombat::combat_testdf
  
  Y = acast(df, rowSeq~colSeq, value.var = "value")
  bx = acast(df, rowSeq~colSeq, value.var = "RunID")[1,]
  bx = factor(bx)
   
  model = dascombat::fit(Y, bx, mean.only = TRUE)
  
  bx=sample(bx)
   
  dascombat::applyModel(Y,model,bx=bx)
    
  bx = as.character(bx)
  bx[1] = "something missing"
  bx = factor(bx)
  
  expect_error(dascombat::applyModel(Y,model,bx=bx),
               message="apply.bad.batch.variable")
  
  
   
})


