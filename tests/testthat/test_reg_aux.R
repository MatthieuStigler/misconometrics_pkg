context("reg_aux")
library(misconometrics)

## Fit models
model_full_1 <- lm(y ~ lag.quarterly.revenue + price.index + income.level + market.potential, data=freeny)

res_lm_gen <- reg_aux.default(object=model_full_1, var_main = "lag.quarterly.revenue")
res_lm <- reg_aux(object=model_full_1, var_main = "lag.quarterly.revenue")
res_lmf <- reg_aux(object=model_full_1, var_main = "lag.quarterly.revenue", method = "update_lmfit", add_vcov = TRUE)
res_sweep <- reg_aux.lm(model_full_1, var_main = "lag.quarterly.revenue", method = "sweep", add_vcov = TRUE)


res_li <-  list(res_lm= res_lm,
                res_lmf = res_lmf,
                res_sweep = res_sweep)



test_that("reg_aux lm: same coefs", {
  expect_equal(coef(res_lmf), sapply(res_lm_gen, coef))
  expect_equal(coef(res_lmf), coef(res_sweep))
  expect_equal(coef(res_lm), coef(res_sweep))
  expect_equal(coef(summary(res_lmf)), coef(summary(res_sweep)))
})

test_that("reg_aux lm: vcov", {
  expect_equal(vcov(res_lmf), vcov(res_sweep))
  expect_equal(vcov(res_lm), vcov(res_sweep))
})




