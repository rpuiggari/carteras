library(testthat)
source("carteras.R")

test_that("amort_tbl principal sums to capital", {
  cap <- 1000
  tab <- amort_tbl(capital = cap, tasa_anual = 0.1, n_cuotas = 12, iva_rate = 0.21)
  expect_equal(sum(tab$Pago_principal), cap, tolerance = 1e-8)
  expect_equal(tail(tab$Saldo_inicial, 1) - tail(tab$Pago_principal, 1), 0, tolerance = 1e-8)
})
