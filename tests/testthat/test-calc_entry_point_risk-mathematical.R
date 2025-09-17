test_that("TV-001 : sigmoid and inv_sigmoid functions correctly", {
  # s(1) = 0.462
  expect_equal(sigmoid(1), 0.462, tolerance = 0.01)

  # s⁻¹(0.154) = 0.310
  expect_equal(inv_sigmoid(0.154), 0.310, tolerance = 0.01)
  # Test de cohérence : s(s⁻¹(x)) = x
  expect_equal(sigmoid(inv_sigmoid(1)), 1)
})
test_that("TV-002 : fundamental properties", {

  f <- scale_entry_points
  # Monotonie : f(2,1) ≤ f(3,1) et f(2,1) ≤ f(2,2)
  expect_true(f(2, 1) <= f(3, 1))
  expect_true(f(2, 1) <= f(2, 2))

  xc <- seq(0, 10)
  xn <- seq(0, 10)
  # Bornes : 0 = f(0, 0) ≤ f(xc,xu) ≤ f(10, 10) ≤ 100 pour tous xc,xu ≥ 0
  expect_true(f(0, 0) == 0)
  expect_true(f(10, 10) <= 100)
  expect_true(all(0 <= f(xc, xn)))
  expect_true(all(100 >= f(xc, xn)))

  # Continuité : Pas de discontinuités
  xc <- seq(0, 10, 0.01)
  xn <- seq(0, 10, 0.01)
  res <- f(xc, xn)
  expect_true(all(is.finite(res)))
})

test_that("TV-003 : Asymptotic convergence", {
  f <- scale_entry_points
  # f(0, 10) = 99.99 ≈ M (avec alpha = 1, M = 100)
  expect_equal(f(0, 10), 100, tolerance = 0.0001)

  # f(10, 0) = 33.33 ≈ M/λ (alpha = beta = 1, lambda = 3, M = 100)
  expect_equal(f(10, 0), 100/3, tolerance = 0.0001)
})
test_that("TV-004 : Univariate equivalence", {
  f <- scale_entry_points
  # xc = 2.31 équivaut à x̂u = 0.56
  # Vérifier : f(2.31, 0) ≈ f(0, 0.56) = 27.31
  expect_equal(f(2.31, 0), f(0, 0.56), tolerance = 0.001)
})

test_that("TP-001 : Variation of lambda (ratio of impact controlled/non-controlled points)", {
  # Test avec xc = 2, xu = 1 :
  #   Plus grand λ → plus faible importance des points controlés → moins de risque
  # •	λ = 1.5 → 78.55 (faible différence types)
  # •	λ = 3 → 64.08 (différence standard)
  # •	λ = 5 → 57.40 (forte différence types)
  # •	λ = 10 → 52.00 (très forte différence)
  f <- scale_entry_points
  expect_equal(f(2, 1, illegal_factor = 1.5), 78.55, tolerance = 0.001)
  expect_equal(f(2, 1, illegal_factor = 3),   64.08, tolerance = 0.001)
  expect_equal(f(2, 1, illegal_factor = 5),   57.40, tolerance = 0.001)
  expect_equal(f(2, 1, illegal_factor = 10),  52.00, tolerance = 0.001)
})


test_that("TP-002 : Variation of alpha and beta (convergence speed towards range limits)", {
  # Test avec xc = 2, xu = 1
  # •	α=β=1 → 64.08 (standard)
  # •	α=0.5, β=2 → 81.95 (convergence plus lente contrôlés)
  # •	α=2, β=0.5 → 52.49 (convergence plus rapide contrôlés)
  f <- scale_entry_points
  expect_equal(f(2, 1, coef_legal = 1,   coef_illegal = 1),   64.08, tolerance = 0.001)
  expect_equal(f(2, 1, coef_legal = 0.5, coef_illegal = 2),   81.95, tolerance = 0.001)
  expect_equal(f(2, 1, coef_legal = 2,   coef_illegal = 0.5), 52.49, tolerance = 0.001)
})



test_that("TP-003 : Varition of M (max score range)", {
  # Test avec xc = 2, xu = 1, alpha = beta = 1, lambda = 3 :
  #   •	M = 50 → 32.04
  #   •	M = 100 → 64.08
  #   •	M = 200 → 128.16
  f <- scale_entry_points
  expect_equal(f(2, 1, max_risk = 50), 32.04, tolerance = 0.001)
  expect_equal(f(2, 1, max_risk = 100), 64.08, tolerance = 0.001)
  expect_equal(f(2, 1, max_risk = 200), 128.16, tolerance = 0.001)
})

test_that("TL-001 : Test lots of points", {
  # Test avec 1000 points :
  #   •	500 contrôlés, 500 non contrôlés
  # •	Exposition moyenne = 0.5 par point
  # •	xc = xu = 250
  xc <- rep(250, 1000)
  xu <- rep(250, 1000)

  before <- Sys.time()
  res <- scale_entry_points(xc, xu)
  after <- Sys.time()

  # •	Résultat attendu : ≈ 100 (saturation)
  expect_equal(res, rep(100, 1000), tolerance = 0.01)

  # •	Performance attendue : < 1 seconde
  duration <- after - before
  expect_true(duration < 60)
})

test_that("TL-002 : Numeric precision", {
  # TL-002 : Précision numérique
  # Test expositions très faibles : xc = 0.001, xu = 0.001 → 0.06
  # Test expositions élevées : xc = 100, xu = 100 → 100.0
  # Stabilité numérique vérifiée

  expect_equal(scale_entry_points(0.001, 0.001),0.066666, tolerance = 0.00001)
  expect_equal(scale_entry_points(100, 100),100, tolerance = 0.00001)
})





