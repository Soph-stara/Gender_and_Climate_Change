#### HAUSARBEIT KLIMAWANDEL BEHNKE ####
# Seminar paper: "Inwieweit übt Geschlecht einen Einfluss auf die
# Zahlungsbereitschaft zur Abschwächung des Klimawandels aus?"
# Zeppelin University, BA Sociology, Politics & Economics, 2020
#
# Revised version (2026). Changes compared to the original 2020 script:
#   1. Reverse-coding of CFC item v_274 corrected: 6 - x (keeps the 1–5 range).
#      The original formula (x - max) * -1 produced a 0–4 range.
#   2. Mean/SD of the WTP index are computed after the index is created
#      (the original script called willpay before defining it).
#   3. Typo "lire_selbt_dichotom" fixed (those lines returned NaN).
#   4. Missing values (-77) are recoded to NA item by item before building
#      indices, instead of catching negative sums afterwards.
#   5. No attach(): all variables are created inside the data frame.
#   6. Section 7 reproduces the CFC results as reported in the paper,
#      which were computed before v_274 was reverse-coded (see README).
#
# Data: Umfrage_Corona_Klimawandel.csv (N = 788), codebook: Fragebogen mit Variablennamen.docx

#### 0. Packages ####
# Base R is sufficient. sjPlot is only used for an optional formatted cross table.
use_sjplot <- requireNamespace("sjPlot", quietly = TRUE)

#### 1. Load data ####
dat <- read.csv2("Umfrage_Corona_Klimawandel.csv")

# helper: recode the missing code -77 (and any other negative code) to NA
na_neg <- function(x) { x[x < 0] <- NA; x }

#### 2. Sociodemographics ####
  #### Gender (v_382: 1 = male, 2 = female) ####
  dat$mann <- ifelse(dat$v_382 == 1, 1, 0)          # 1 = male, 0 = female
  table(dat$v_382)
  prop.table(table(dat$mann))

  #### Age (v_1033) ####
  dat$alter <- dat$v_1033
  dat$alter[dat$alter == 5] <- NA                    # implausible value (age 5)
  mean(dat$alter, na.rm = TRUE)
  hist(dat$alter, main = "Age", xlab = "Age")

  dat$age_group <- cut(dat$alter, breaks = c(16, 30, 50, 86),
                       labels = c("bis 30", "bis 50", "bis 86"))
  prop.table(table(dat$age_group))

  #### Education (v_384) ####
  table(dat$v_384)
  prop.table(table(dat$v_384))
  # share with at least "Mittlere Reife" (codes 4-7)
  mean(dat$v_384 %in% 4:7)

  #### Income (v_318, deciles 1-10) ####
  dat$einkommen <- na_neg(dat$v_318)
  mean(dat$einkommen, na.rm = TRUE)

#### 3. Political orientation (v_235: 1 = left ... 11 = right) ####
  dat$lire_selbst <- na_neg(dat$v_235)
  mean(dat$lire_selbst, na.rm = TRUE)
  sd(dat$lire_selbst, na.rm = TRUE)
  barplot(table(dat$lire_selbst), main = "Left-right self-placement")

  #### Dichotomisation: 1-5 = more liberal, 6-11 = more conservative ####
  dat$lire_selbst_dichotom <- factor(
    ifelse(dat$lire_selbst <= 5, "liberaler", "konservativer"),
    levels = c("liberaler", "konservativer"))
  table(dat$lire_selbst_dichotom)

#### 4. Willingness to pay (thermometer scales 0-100) ####
  dat$wp_steuern        <- na_neg(dat$v_221)   # higher taxes
  dat$wp_konsum         <- na_neg(dat$v_222)   # higher prices for consumer goods
  dat$wp_lebensstandard <- na_neg(dat$v_223)   # cuts in living standard
  dat$wp_benzin         <- na_neg(dat$v_224)   # higher fuel prices

  par(mfrow = c(2, 2))
  hist(dat$wp_steuern,        xlim = c(0, 100), ylim = c(0, 300), main = "Taxes")
  hist(dat$wp_konsum,         xlim = c(0, 100), ylim = c(0, 300), main = "Consumer goods")
  hist(dat$wp_lebensstandard, xlim = c(0, 100), ylim = c(0, 300), main = "Living standard")
  hist(dat$wp_benzin,         xlim = c(0, 100), ylim = c(0, 300), main = "Fuel")
  par(mfrow = c(1, 1))

  #### WTP index (mean of the four items) ####
  dat$willpay <- with(dat, (wp_steuern + wp_konsum + wp_lebensstandard + wp_benzin) / 4)
  mean(dat$willpay, na.rm = TRUE)
  sd(dat$willpay, na.rm = TRUE)
  hist(dat$willpay, xlim = c(0, 100), main = "Willingness to pay (index)")

#### 5. Environmental values (Likert 1-5) ####
  # v_268: protection of environment and nature
  # v_275: unity of humans and nature
  # v_343: respect for the planet and other species
  dat$env_values <- with(dat, (na_neg(v_268) + na_neg(v_275) + na_neg(v_343)) / 3)
  mean(dat$env_values, na.rm = TRUE)
  sd(dat$env_values, na.rm = TRUE)

#### 6. Consideration of Future Consequences (CFC, shortened, Likert 1-5) ####
  # v_269, v_273, v_342, v_346: future-oriented items
  # v_274: "My behaviour is only influenced by immediate outcomes" -> reverse-coded
  dat$v_274_rev <- 6 - na_neg(dat$v_274)            # 1->5, 2->4, 3->3, 4->2, 5->1
  table(dat$v_274, dat$v_274_rev)                    # check recoding

  dat$cfc <- with(dat, (na_neg(v_269) + na_neg(v_273) + v_274_rev +
                        na_neg(v_342) + na_neg(v_346)) / 5)
  mean(dat$cfc, na.rm = TRUE)
  sd(dat$cfc, na.rm = TRUE)
  hist(dat$cfc, xlim = c(1, 5), main = "CFC (index)")

#### 7. Hypothesis tests ####
  #### H: WTP ~ gender + political orientation + environmental values (multiple regression) ####
  m1 <- lm(willpay ~ v_382 + lire_selbst + env_values, data = dat)
  summary(m1)

  #### H: political orientation x gender (chi-squared test) ####
  tab_pol_gender <- table(dat$v_382, dat$lire_selbst_dichotom)
  tab_pol_gender
  prop.table(tab_pol_gender, margin = 1)            # share liberal / conservative by gender
  chisq.test(tab_pol_gender)
  if (use_sjplot) sjPlot::sjt.xtab(dat$lire_selbst_dichotom, dat$v_382, show.col.prc = TRUE)

  #### H: environmental values by political orientation (t-test) ####
  tapply(dat$env_values, dat$lire_selbst_dichotom, mean, na.rm = TRUE)
  t.test(env_values ~ lire_selbst_dichotom, data = dat, var.equal = TRUE)

  #### H: WTP by political orientation (t-test) ####
  tapply(dat$willpay, dat$lire_selbst_dichotom, mean, na.rm = TRUE)
  t.test(willpay ~ lire_selbst_dichotom, data = dat, var.equal = TRUE)

  #### H: CFC by gender (t-test) ####
  tapply(dat$cfc, dat$v_382, mean, na.rm = TRUE)
  t.test(cfc ~ v_382, data = dat, var.equal = TRUE)

  #### H: WTP ~ CFC (bivariate regression) ####
  m2 <- lm(willpay ~ cfc, data = dat)
  summary(m2)

  set.seed(1)                                       # reproducible jitter
  plot(jitter(dat$cfc, amount = 0.15), dat$willpay,
       main = "Zusammenhang von CFC und WTP",
       xlab = "Sorge um zukünftige Konsequenzen",
       ylab = "Zahlungsbereitschaft")
  abline(m2, col = "red")

#### 8. Reproduction of the CFC results as reported in the 2020 paper ####
  # In the paper, the CFC t-test and regression were computed with v_274
  # NOT yet reverse-coded. This section reproduces those reported values
  # (men 3.197, women 3.154, p = 0.2655; slope 19.93) for transparency.
  dat$cfc_2020 <- with(dat, (na_neg(v_269) + na_neg(v_273) + na_neg(v_274) +
                             na_neg(v_342) + na_neg(v_346)) / 5)
  tapply(dat$cfc_2020, dat$v_382, mean, na.rm = TRUE)
  t.test(cfc_2020 ~ v_382, data = dat, var.equal = TRUE)
  coef(lm(willpay ~ cfc_2020, data = dat))
