library(dascombat)
library(testthat)

df = dascombat::combat_testdf

Y = acast(df, rowSeq~colSeq, value.var = "value")
bx = acast(df, rowSeq~colSeq, value.var = "RunID")[1,]
bx = factor(bx)

ref.batch = "1"

sva.test.data = list(
  sva.mean.only.t = sva::ComBat(Y, bx, mean.only  = TRUE, ref.batch = NULL),
  sva.mean.only.f = sva::ComBat(Y, bx, mean.only  = FALSE, ref.batch = NULL),
  sva.mean.only.t.ref = sva::ComBat(Y, bx, mean.only  = TRUE, ref.batch = ref.batch),
  sva.mean.only.f.ref = sva::ComBat(Y, bx, mean.only  = FALSE, ref.batch = ref.batch)
)

save(sva.test.data, file="./tests/testthat/sva.test.data.Rdata" )

 