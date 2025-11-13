library(cmdstanr)
library(rethinking)
library(ggplot2)
library(dplyr)
library(readxl)
library(data.table)
library(patchwork)
library(tidyr)
set.seed(123)

final_plot_data <- read.csv("tree_plot_data_final_carbon.csv")
final_plot_data <- final_plot_data %>% 
  filter(ownership != "New River Gorge")
ownership_index <- unique(final_plot_data$ownership) 


selected_variables <- c("cpa", "qmd", "baph", "tph", "mean_dia", "mean_actualht", "mean_HD", "slope", "aspect", "sdi")
selected_corr_data <- final_plot_data[,selected_variables]

hist_data <- selected_corr_data %>% gather() %>% head()
ggplot(gather(selected_corr_data), aes(value)) + 
  geom_histogram(bins = 10) + 
  facet_wrap(~key, scales = 'free_x')
#final_plot_data[,c("sdi", "tph")] <- scale(final_plot_data[,c("sdi", "tph")])

i = 1

final_plot_data_subset <- final_plot_data %>% 
  filter(ownership == ownership_index[i])
final_plot_data_subset[,c("sdi", "tph")] <- scale(final_plot_data_subset[,c("sdi", "tph")])
# maybe take the log of cpa to not produce negative values? Removed it from the original formula copied from class

# Specify model
mod1 = alist(
  cpa ~ dnorm(mu),
  mu <- beta1 + alpha[fortypcd] + beta_qmd*qmd + beta_baph*baph + beta_tph*tph + beta_dia*mean_dia + 
    beta_ht*mean_actualht + beta_HD*mean_HD + beta_slope*slope + beta_aspect*aspect + beta_sdi+sdi,
  beta1 ~ dnorm(0, 1),
  alpha[location_id] ~ dnorm(0, sigma_loc),
  c(beta_qmd, beta_baph, beta_tph, beta_dia, beta_dia, beta_ht, beta_HD, beta_slope, beta_aspect, beta_sdi) ~ dnorm(0, 3),
  sigma_loc ~ dhalfnorm(0, 1)
)

mod1 = alist(
  cpa ~ dnorm(mu),
  mu <- beta1 + alpha[fortypcd] + beta_qmd*qmd + beta_baph*baph + beta_tph*tph + beta_dia*mean_dia + 
    beta_ht*mean_actualht + beta_HD*mean_HD + beta_slope*slope + beta_aspect*aspect + beta_sdi+sdi,
  beta1 ~ dnorm(0, 1),
  alpha[location_id] ~ dnorm(0, sigma_loc),
  c(beta_qmd, beta_baph, beta_tph, beta_dia, beta_dia, beta_ht, beta_HD, beta_slope, beta_aspect, beta_sdi) ~ dnorm(0, 3),
  sigma_loc ~ dhalfnorm(0, 1)
)

# Fit with ulam
stan_data = final_plot_data_subset[, c("fortypcd", "cpa", "qmd", "baph", "tph", "mean_dia", "mean_actualht", "mean_HD", "slope", "aspect", "sdi")]
mod1_fit = ulam(mod1, data=stan_data, iter=2000,
                warmup=500, chains=4, cores=4, log_lik=TRUE)

precis(mod1_fit, depth=2)
