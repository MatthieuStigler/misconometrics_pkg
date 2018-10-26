context("reg_aux")
library(misconometrics)

## Fit models
model_full_1 <- lm(y ~ lag.quarterly.revenue + price.index + income.level + market.potential, data=freeny)

res_lm_gen <- misconometrics:::reg_aux.default(object=model_full_1, var_main = "lag.quarterly.revenue")
res_lm <- reg_aux(object=model_full_1, var_main = "lag.quarterly.revenue")
res_lmf <- reg_aux(object=model_full_1, var_main = "lag.quarterly.revenue", method = "update_lmfit", add_vcov = TRUE)
res_sweep <- reg_aux(model_full_1, var_main = "lag.quarterly.revenue", method = "sweep", add_vcov = TRUE)


## 2 main
res2_lm_gen <- misconometrics:::reg_aux.default(object=model_full_1, var_main = c("lag.quarterly.revenue", "income.level"))
res2_lm <- reg_aux(object=model_full_1, var_main = c("lag.quarterly.revenue", "income.level"))
res2_lmf <- reg_aux(object=model_full_1, var_main = c("lag.quarterly.revenue", "income.level"), method = "update_lmfit", add_vcov = TRUE)
res2_sweep <- reg_aux(model_full_1, var_main = c("lag.quarterly.revenue", "income.level"), method = "sweep", add_vcov = TRUE)


##
res_li <-  list(res_lm= res_lm,
                res_lmf = res_lmf,
                res_sweep = res_sweep)



test_that("reg_aux lm: same coefs", {
  expect_equal(coef(res_lmf), sapply(res_lm_gen, coef))
  expect_equal(coef(res_lmf), coef(res_sweep))
  expect_equal(coef(res_lm), coef(res_sweep))
  expect_equal(coef(summary(res_lmf)), coef(summary(res_sweep)))

})

test_that("reg_aux lm: same coefs, 2 main vars", {
  expect_equal(coef(res2_lmf), sapply(res2_lm_gen, coef))
  expect_equal(coef(res2_lmf), coef(res2_lm))
  expect_equal(coef(res2_lm), coef(res2_sweep))
  expect_equal(coef(summary(res2_lmf)), coef(summary(res2_sweep)))

})

test_that("reg_aux lm: vcov", {
  expect_equal(vcov(res_lmf), vcov(res_sweep))
  expect_equal(vcov(res_lm), vcov(res_sweep))

  expect_equal(vcov(res2_lmf), vcov(res2_sweep))
  expect_equal(vcov(res2_lm), vcov(res2_sweep))
})

## felm
skip_if_not_installed("tidyverse")
skip_if_not_installed("lfe")
library(tidyverse)
library(lfe)
x <- rnorm(1000)
x2 <- rnorm(length(x))
x3 <- rnorm(length(x))
x4 <- rnorm(length(x))
u <- rnorm(length(x))

## individual and firm
id <- factor(sample(20,length(x), replace=TRUE))
firm <- factor(sample(13,length(x), replace=TRUE))
id.eff <- rnorm(nlevels(id))
firm.eff <- rnorm(nlevels(firm))


y <- x + 0.5*x2 + 0.2 * x3 -0.3 * x4 + id.eff[id] + firm.eff[firm] + rnorm(length(x))
model_felm <- felm(y ~ x + x2 + x3 + x4 |id + firm)

## 1 main var
res_felm_def <- misconometrics:::reg_aux.default(object=model_felm, var_main = "x")
res_felm_upd <- reg_aux(object=model_felm, var_main = "x")
res_felm_swp <- reg_aux(object=model_felm, var_main = "x", method = "sweep", add_vcov = TRUE)

## 2 main vars
res2_felm_def <- misconometrics:::reg_aux.default(object=model_felm, var_main = c("x", "x2"))
res2_felm_upd <- reg_aux(object=model_felm, var_main = c("x", "x2"))
res2_felm_swp <- reg_aux(object=model_felm, var_main = c("x", "x2"), method = "sweep", add_vcov = TRUE)


test_that("reg_aux felm: same coefs", {
  expect_equal(map_dbl(res_felm_def, ~coef(.)), coef(res_felm_upd)[1,])
  expect_equal(map_df(res2_felm_def, ~coef(.)), coef(res2_felm_upd))
  expect_equal(coef(res_felm_upd)[1,], coef(res_felm_swp))
  expect_equal(coef(res2_felm_upd), coef(res2_felm_swp))
})


test_that("reg_aux felm: same summary/se", {
  expect_equal(map_dfr(list("x2", "x3", "x4"), ~coef(summary(res_felm_upd, lhs = .)) %>%  as.data.frame),
               map_df(res_felm_def, ~summary(.) %>%  coef() %>%  as.data.frame))
  expect_equal(coef(misconometrics:::summary.reg_aux_lm(res_felm_swp)) %>%  as.data.frame,
               map_df(res_felm_def, ~summary(.) %>%  coef() %>%  as.data.frame), check.attributes = FALSE)
  expect_equal(map_dfr(list("x3", "x4"), ~coef(summary(res2_felm_upd, lhs = .)) %>%  as.data.frame),
               map_df(res2_felm_def, ~summary(.) %>%  coef() %>%  as.data.frame))
  expect_equal(coef(misconometrics:::summary.reg_aux_lm(res2_felm_swp)) %>%  as.data.frame,
               map_df(res2_felm_def, ~summary(.) %>%  coef() %>%  as.data.frame), check.attributes = FALSE)

})


test_that("reg_aux felm: same vcov", {
  expect_equal(vcov(res_felm_upd, lhs = "x2"), res_felm_swp$vcov[1, 1], check.attributes = FALSE)
  expect_equal(vcov(res_felm_upd, lhs = "x3"), res_felm_swp$vcov[2, 2], check.attributes = FALSE)

  expect_equal(vcov(res2_felm_upd, lhs = "x3"), res2_felm_swp$vcov[1:2, 1:2], check.attributes = FALSE)
  expect_equal(vcov(res2_felm_upd, lhs = "x4"), res2_felm_swp$vcov[3:4, 3:4], check.attributes = FALSE)
})


# res_felm_upd_clust <- reg_aux.felm(object=model_felm_clust, var_main = "pcap")
# res_felm_swp_clust <- reg_aux.felm(object=model_felm_clust, var_main = "pcap", method = "sweep", add_vcov = TRUE)

