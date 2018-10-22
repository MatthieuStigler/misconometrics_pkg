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


res_felm_upd <- reg_aux(object=model_felm, var_main = "x")
res_felm_swp <- reg_aux(object=model_felm, var_main = "x", method = "sweep", add_vcov = TRUE)

# res_felm_upd_clust <- reg_aux.felm(object=model_felm_clust, var_main = "pcap")
# res_felm_swp_clust <- reg_aux.felm(object=model_felm_clust, var_main = "pcap", method = "sweep", add_vcov = TRUE)


map_dfr(list("x2", "x3", "x4"), ~coef(summary(res_felm_upd, lhs = .)) %>%  as.data.frame)
coef(summary.reg_aux_lm(res_felm_swp))



# test_that("reg_aux felm: same coefs", {
#   expect_output(all.equal(map_dfr(list("x2", "x3", "x4"), ~coef(summary(res_felm_upd, lhs = .)) %>%  as.data.frame),
#                  coef(summary.reg_aux_lm(res_felm_swp)) %>%  as.data.frame),
#                  # check.attributes = FALSE
#                  "Error: map_dfr(...) not equal to coef(summary.reg_aux_lm(res_felm_swp)) %>% as.data.frame.
#                  Component “Std. Error”: Mean relative difference: 0.002074691
#                  Component “t value”: Mean relative difference: 0.002070396
#                  Component “Pr(>|t|)”: Mean relative difference: 0.001608762")
# })
#
# ## compare
# all.equal(coef(res_lm)[2,], coef(res_sweep))
# all.equal(vcov(res_lm), res_sweep$vcov, check.attributes = FALSE)
#
#
# coef(summary(object = res_lm))
# coef(summary(object = res_sweep))
#
