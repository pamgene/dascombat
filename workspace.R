library(dascombat)
library(reshape2)
library(dplyr)
library(testthat)

df = dascombat::combat_testdf

expect_true(is.data.frame(df))

Y = acast(df, rowSeq~colSeq, value.var = "value")
bx = acast(df, rowSeq~colSeq, value.var = "RunID")[1,]
bx = factor(bx)
 
cMod = dascombat::LS.NoRef(Y, bx, mean.only = TRUE)
cMod = dascombat::LS.NoRef(Y, bx, mean.only = F)

plot(Y)
plot(cMod)


sva.cMod = sva::ComBat(Y, bx, mean.only  = TRUE, ref.batch = NULL)
expect_true(all(round(cMod,8) - round(sva.cMod,8) == 0))

cMod = dascombat::LS.Ref(Y, bx, mean.only = TRUE, ref.batch = "1")
sva.cMod = sva::ComBat(Y, bx, mean.only  = TRUE, ref.batch = "1")
expect_true(all(round(cMod,8) - round(sva.cMod,8) == 0))


ref.batch = "1"

bx = relevel(factor(bx), ref = ref.batch) # ref will be the first level of bx
bx

lvbx = levels(bx)
nObsPerBatch = summary(bx)
nObsPerBatch
nObs = sum(nObsPerBatch)
nObs

Y = t(Y)

Y
# scaling based on ref.batch
B = model.matrix(~bx)
lamb.hat = solve( t(B)%*% B, t(B) %*% Y)
alpha_g = lamb.hat[1,]
siggsq  = t(t((Y[bx == lvbx[1], ] - (B[bx == lvbx[1],] %*% lamb.hat))^2) %*% rep(1/nObsPerBatch[1], nObsPerBatch[1]))

siggsq  = t(t((Y - (B %*% lamb.hat))^2) %*% rep(1/nObs, nObs))

Z = scale(Y, center = alpha_g, scale = sqrt(siggsq))

lambda.hat = solve( t(B)%*% B, t(B) %*% Z) # unadjusted location

df = dascombat::combat_testdf

data = df %>% group_by(RunID) %>% do({
   
  data.frame(groupRowIndex=group_rows(.))
})


 
class(df %>% group_by("RunID"))
length(df %>% group_by("RunID"))

df %>% group_by(RunID) %>% summarise(groupRowIndex=list(group_rows(.)))

df %>% group_by(RunID) %>% group_data() %>% summarise(groupRowIndex=max(.$.rows))

df <- tibble(x = c(1,1,2,2,1))

gf <- group_by(df, x)
group_vars(gf)
 
group_rows(gf)

ccc = c(1,1,1,1,2,2,5)
ccc[as.integer(list(1,2))]
ccc[as.integer(list(1,2))]


