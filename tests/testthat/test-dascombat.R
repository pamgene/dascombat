library(testthat)
context("test-dascombat")
library(dascombat)
library(reshape2)
  
test_that("fit input validation", {
  
  df = dascombat::combat_testdf
  
  Y = acast(df, rowSeq~colSeq, value.var = "value")
  bx = acast(df, rowSeq~colSeq, value.var = "RunID")[1,]
  bx = factor(bx)
  
  Y[1,1] <- NaN
  
  expect_error(dascombat::fit(Y, bx, mean.only = TRUE), "Missing values are not allowed")
  
  Y = acast(df, rowSeq~colSeq, value.var = "value")
  # remove first
  bx = acast(df, rowSeq~colSeq, value.var = "RunID")[1,-1]
  bx = factor(bx)
   
  expect_error(dascombat::fit(42, bx, mean.only = TRUE),"A matrix is required")
   
  expect_error(dascombat::fit(Y, bx, mean.only = TRUE),"Bad length")
})

test_that("apply input validation", {
  
  df = dascombat::combat_testdf
  
  Y = acast(df, rowSeq~colSeq, value.var = "value")
  bx = acast(df, rowSeq~colSeq, value.var = "RunID")[1,]
  bx = factor(bx)
  model = dascombat::fit(Y, bx, mean.only = TRUE)
  cMod = dascombat::applyModel(Y,model,model$bx)
  
  expect_error(dascombat::applyModel(42,model,model$bx),"A matrix is required")
  
  Y[1,1] <- NaN
  
  expect_error(dascombat::applyModel(Y,model,model$bx),"Missing values are not allowed")
  
  Y = acast(df, rowSeq~colSeq, value.var = "value")
  # remove first
  bx = acast(df, rowSeq~colSeq, value.var = "RunID")[1,-1]
  expect_error(dascombat::fit(Y, bx, mean.only = TRUE),"Bad length")
  
  
})

test_that("test applyModel output size", {
  
  df = dascombat::combat_testdf
  
  Y = acast(df, rowSeq~colSeq, value.var = "value")
  bx = acast(df, rowSeq~colSeq, value.var = "RunID")[1,]
  bx = factor(bx)
   
  model = dascombat::fit(Y, bx, mean.only = TRUE)
  cMod = dascombat::applyModel(Y,model,model$bx)
  
  expect_true(nrow(cMod) == nrow(Y))
  expect_true(ncol(cMod) == ncol(Y))
})

test_that("dascombat::fit vs sva::ComBat", {
  
  load("sva.test.data.Rdata" )
  expect_true(is.list(sva.test.data))
  
  df = dascombat::combat_testdf
  
  Y = acast(df, rowSeq~colSeq, value.var = "value")
  bx = acast(df, rowSeq~colSeq, value.var = "RunID")[1,]
  bx = factor(bx)
    
  sva.cMod = sva.test.data$sva.mean.only.t
  model = dascombat::fit(Y, bx, mean.only = TRUE)
  cMod = dascombat::applyModel(Y,model,model$bx)
  expect_true(all(round(cMod,8) - round(sva.cMod,8) == 0))
    
  sva.cMod = sva.test.data$sva.mean.only.f
  model = dascombat::fit(Y, bx, mean.only = FALSE)
  cMod = dascombat::applyModel(Y,model,model$bx)
  expect_true(all(round(cMod,8) - round(sva.cMod,8) == 0))
  
  #########################
  
  ref.batch = "1"
  
  sva.cMod = sva.cMod = sva.test.data$sva.mean.only.t.ref
  model = dascombat::fit(Y, bx, mean.only = TRUE, ref.batch = ref.batch)
  cMod = dascombat::applyModel(Y,model,model$bx)
  expect_true(all(round(cMod,8) - round(sva.cMod,8) == 0))
  
  sva.cMod = sva.cMod = sva.test.data$sva.mean.only.f.ref
  model = dascombat::fit(Y, bx, mean.only = FALSE, ref.batch = ref.batch)
  cMod = dascombat::applyModel(Y,model,model$bx)
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

test_that("apply null", {
  df = dascombat::combat_testdf
  
  Y = acast(df, rowSeq~colSeq, value.var = "value")
  bx = acast(df, rowSeq~colSeq, value.var = "RunID")[1,]
  bx = factor(bx)
  
  model = dascombat::fit(Y, bx, mean.only = TRUE)
    
  expect_error(dascombat::applyModel(Y,model,bx=NULL),
               message="apply.bad.batch.variable")
   
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


