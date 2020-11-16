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
  
  sva.cMod = sva::ComBat(Y, bx, mean.only  = TRUE, ref.batch = "1")
  model = dascombat::fit(Y, bx, mean.only = TRUE, ref.batch = "1")
  cMod = dascombat::applyModel(Y,model)
  expect_true(all(round(cMod,8) - round(sva.cMod,8) == 0))

  sva.cMod = sva::ComBat(Y, bx, mean.only  = FALSE, ref.batch = "1")
  model = dascombat::fit(Y, bx, mean.only = FALSE, ref.batch = "1")
  cMod = dascombat::applyModel(Y,model)
  expect_true(all(round(cMod,8) - round(sva.cMod,8) == 0))
})

 


