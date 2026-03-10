<img src="https://r2cdn.perplexity.ai/pplx-full-logo-primary-dark%402x.png" style="height:64px;margin-right:32px"/>

# i have the coefficients and random standard deviation that I want to provide to R to calculate predictions. how can i write out the code to do this

You can do this by explicitly building the linear predictor from your design matrices and plugging in your own fixed and random terms (and, if desired, simulating random effects from the supplied SDs).

Below are two common situations; you can adapt them to your exact setup.

***

## 1. Linear model, fixed effects only

If you just have a vector of fixed-effect coefficients $\beta$ and a dataset `newdat`:

```r
# Suppose:
# beta is a named vector: c("(Intercept)" = 1.2, x1 = 0.5, x2 = -0.3)
# newdat has columns x1, x2, etc.

X <- model.matrix(~ x1 + x2, data = newdat)
# Make sure names(X) match names(beta)
lp <- as.vector(X %*% beta)   # linear predictor
pred <- lp                    # for identity link
```

This is essentially what `predict.lm()` does internally when given a coefficients vector.[^1_1]

***

## 2. Mixed model with random intercepts

Say your model is $y = X\beta + Zb + \varepsilon$, with a random intercept per group, and you have:

- `beta`: fixed-effect coefficients (named vector).
- `sigma_b`: random intercept SD.
- `sigma_eps`: residual SD (optional; only needed for simulation).
- `newdat`: a data frame with the predictors and the grouping variable `group`.


### 2.1. Point predictions with known random effects

If you already *know* the random effect for each group (e.g., from previous estimation), store them in a named vector `b_hat`:

```r
# b_hat: named vector of random intercepts, names are group levels
# e.g., b_hat <- c(g1 = 1.0, g2 = -0.3, g3 = 0.5)

X <- model.matrix(~ x1 + x2, data = newdat)
lp_fixed <- as.vector(X %*% beta)

# match group to its random effect (0 if group not present in b_hat)
b_i <- b_hat[as.character(newdat$group)]
b_i[is.na(b_i)] <- 0

lp <- lp_fixed + b_i
pred <- lp
```

This replicates what `predict.merMod(..., re.form = ~(1|group))` would do when you supply the random effects yourself. [^1_2][^1_3]

### 2.2. Simulate random effects from SDs

If you only have the SD of the random effect (no realized `b_i` yet), you can *draw* random effects and then generate predictions:

```r
set.seed(123)

# Inputs:
# beta      : fixed effects (named vector)
# sigma_b   : random intercept SD for group
# sigma_eps : residual SD (optional)
# newdat    : data frame with x1, x2, group

# Build fixed-effect part
X <- model.matrix(~ x1 + x2, data = newdat)
lp_fixed <- as.vector(X %*% beta)

# Draw one random effect per group
groups <- unique(newdat$group)
b_draw <- rnorm(length(groups), mean = 0, sd = sigma_b)
names(b_draw) <- groups

# Match random draws back to rows
b_i <- b_draw[as.character(newdat$group)]

# Linear predictor and outcome draws
lp <- lp_fixed + b_i
pred_mean <- lp  # conditional mean given random effects

# If you want full simulated outcomes including residual noise:
y_sim <- rnorm(nrow(newdat), mean = lp, sd = sigma_eps)
```

Here we use the standard mixed-model representation $Y = X\beta + Zb + \varepsilon$, with $b \sim N(0, \sigma_b^2)$ and $\varepsilon \sim N(0, \sigma_\varepsilon^2)$.[^1_4][^1_5]

***

## 3. General helper function

You can wrap this into a function that takes coefficients and SDs as inputs:

```r
manual_predict_lmm <- function(newdat,
                               fixed_formula,  # e.g. ~ x1 + x2
                               group_var,      # e.g. "group"
                               beta,
                               sigma_b = NULL,
                               b_hat = NULL,
                               sigma_eps = NULL,
                               simulate = FALSE) {
  # fixed design
  X <- model.matrix(fixed_formula, data = newdat)
  lp_fixed <- as.vector(X %*% beta)

  # random intercepts
  if (!is.null(b_hat)) {
    b_i <- b_hat[as.character(newdat[[group_var]])]
    b_i[is.na(b_i)] <- 0
  } else if (!is.null(sigma_b)) {
    groups <- unique(newdat[[group_var]])
    b_draw <- rnorm(length(groups), 0, sigma_b)
    names(b_draw) <- groups
    b_i <- b_draw[as.character(newdat[[group_var]])]
  } else {
    b_i <- 0
  }

  lp <- lp_fixed + b_i

  if (simulate && !is.null(sigma_eps)) {
    y <- rnorm(nrow(newdat), lp, sigma_eps)
    return(list(lp = lp, y = y))
  } else {
    return(lp)
  }
}
```

This follows the standard mixed-model decomposition and lets you choose between using supplied random effects (`b_hat`), simulating from a supplied SD (`sigma_b`), or ignoring random effects entirely.[^1_6][^1_4]

***

If you paste a small example of your coefficients (including their names), the random SD(s), and the model form (e.g., `(1|plot)` vs `(1 + x|plot)`), I can tailor the code to exactly match your design matrices.
<span style="display:none">[^1_10][^1_7][^1_8][^1_9]</span>

<div align="center">⁂</div>

[^1_1]: https://stackoverflow.com/questions/25695565/predict-with-arbitrary-coefficients-in-r

[^1_2]: https://optimumsportsperformance.com/blog/making-predictions-from-a-mixed-model-using-r/

[^1_3]: https://stats4nr.com/linear-mixed-models

[^1_4]: https://www.geeksforgeeks.org/r-language/how-linear-mixed-model-works-in-r/

[^1_5]: https://www.r-bloggers.com/2021/01/random-effects-model-from-scratch/

[^1_6]: https://stat.ethz.ch/pipermail/r-sig-mixed-models/2010q1/003445.html

[^1_7]: https://cran.r-project.org/web/packages/glmmrBase/glmmrBase.pdf

[^1_8]: https://cran.r-project.org/web/packages/lme4breeding/vignettes/lmebreed.summaries.html

[^1_9]: https://stackoverflow.com/questions/8526681/extract-random-effect-variances-from-lme4-mer-model-object

[^1_10]: https://forum.posit.co/t/estimate-manually-random-effects-and-predict-a-new-variable-using-a-mixed-model-with-lme4/187468

