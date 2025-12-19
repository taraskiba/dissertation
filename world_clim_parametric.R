#Load packages

set.seed(123)
library(MASS);library(nlme);library(glmmLasso);library(ggplot2)
library(dplyr)
library(readxl)
library(foreach)
library(doParallel)
library(ranger)
library(lme4)
library(stringr)
library(caret)
library(ForestFit)
library(MASS)
library(mgcv)
library(abdiv)
library(writexl)
library(mgcv)
library(GGally)
library(tidyr)

#Complementary file: Data_process_carbon.R
#Set directory
#setwd("C:/Users/tara/Desktop/carbon_project_1/")

# used in conjunction with Data_process_carbon.R
# dataset to use: final_plot_data

#Import data
# ff_cond_clean <- read.csv("ff_cond_clean.csv")
# ff_tree3 <- read.csv("ff_tree3.csv")
final_plot_data <- read.csv("tree_plot_data_final_carbon.csv")
final_plot_data <- final_plot_data %>% 
  filter(ownership != "New River Gorge")
ownership_index <- unique(final_plot_data$ownership) 

# histogram from stand data
selected_variables <- c("cpa", "qmd", "baph", "tph", "mean_dia", "mean_actualht", "mean_HD", "diversity", "slope", "aspect", "sdi")
selected_corr_data <- final_plot_data[,selected_variables]

hist_data <- selected_corr_data %>% gather() %>% head()
ggplot(gather(selected_corr_data), aes(value)) + 
  geom_histogram(bins = 10) + 
  facet_wrap(~key, scales = 'free_x')
#ggpairs(selected_corr_data)

plot_var <- c("cpa", "dayl", "prcp", "srad", "swe", "tmax", "tmin", "vp")
#gg_data <- final_plot_data[,plot_var]
#ggpairs(gg_data)

beta = 1000 # no. of iterations
no_cores <- detectCores() - 1
cl <- makeCluster(no_cores, type = "PSOCK")
registerDoParallel(cl)
#set.seed(123)

# forest type group####
# final_plot_data$forest_type_group <- ifelse(final_plot_data$fortypcd >= 100 & final_plot_data$fortypcd < 120,
#                                                  "White/red/jack pine group",
#                                                  ifelse(final_plot_data$fortypcd >= 120 & final_plot_data$fortypcd < 140,
#                                                         "Spruce/fir group",
#                                                         ifelse(final_plot_data$fortypcd >= 160 & final_plot_data$fortypcd < 170,
#                                                                "Loblolly/shortleaf pine group",
#                                                                ifelse(final_plot_data$fortypcd >= 400 & final_plot_data$fortypcd < 500,
#                                                                       "Oak/pine group",
#                                                                       ifelse(final_plot_data$fortypcd >= 500 & final_plot_data$fortypcd < 600,
#                                                                              "Oak/hickory group",
#                                                                              ifelse(final_plot_data$fortypcd >= 800 & final_plot_data$fortypcd < 900,
#                                                                                     "Maple/beech/birch group",
#                                                                                     ifelse(final_plot_data$fortypcd >= 960 & final_plot_data$fortypcd < 970,
#                                                                                            "Other hardwoods group",
#                                                                                            ifelse(final_plot_data$fortypcd >= 990 & final_plot_data$fortypcd < 999,
#                                                                                                   "Exotic hardwoods group",
#                                                                                                   "Nonstocked"))))))))
#### end forest type group ####
# ENVIRONMENTAL VARIABLES ####

beta = 1000

i = 8

## Daymet dataset ####
daymet <- as.data.frame(read.csv("nasa_ornl_daymet_v4-19800101-19980101.csv"))
daymet <- daymet[,-c(1,3:4)]
daymet <- unique(daymet)
daymet <- daymet %>%
  group_by(plot_id) %>%
  dplyr::summarise_all(.funs = c(~quantile(., probs = 0.5)),
                       na.rm = T)

# full dataset w environmental and stand data
env_data <- final_plot_data
env_data$plot_ID <- paste(env_data$STATECD, 
                          env_data$COUNTYCD, 
                          env_data$PLOT, 
                          #tree_data$SUBP, 
                          #tree_data$CONDID, 
                          #tree_data$TREE,
                          sep = "_")
# plot_env <- as.data.frame(env_data$plot_ID)
# plot_daymet <- as.data.frame(daymet$plot_ID)
env_data <- right_join(env_data, daymet, by = "plot_ID")
### SCALE VARIABLES (ENV + STAND)  ####
env_scaled <- env_data
stand_daymet_variables <- c("qmd", "baph", "mean_dia", "mean_actualht", "mean_HD", "diversity", "tph", "slope", "aspect", "sdi", "dayl", "prcp", "srad", "swe", "tmax", "tmin", "vp")
env_scaled[stand_daymet_variables] <- scale(env_scaled[, stand_daymet_variables])
# SCALE VARIABLES (ENV + STAND)  ####
env_scaled <- env_data
env_variables <- c("qmd", "baph", "mean_dia", "mean_actualht", "mean_HD", "tph", "slope", "aspect", "sdi", colnames(worldclim[,-1]))

write.csv(env_scaled, "stand_env_data.csv")

## Terraclimate ####
#terraclimate <- read.csv("IDAHO_EPSCOR_TERRACLIMATE.csv")
#terraclimate <- terraclimate[,-c(17,19)]

## WORLDCLIM_V1_BIO ####

worldclim <- as.data.frame(read.csv("WORLDCLIM_V1_BIO.csv"))
worldclim <- worldclim[,-c(20:22, 24)]
worldclim <- unique(worldclim)
worldclim <- worldclim %>%
  group_by(plot_ID) %>%
  dplyr::summarise_all(.funs = c(~quantile(., probs = 0.5)),
                       na.rm = T)


# full dataset w environmental and stand data
env_data <- final_plot_data
env_data$plot_ID <- paste(env_data$STATECD, 
                          env_data$COUNTYCD, 
                          env_data$PLOT, 
                          #tree_data$SUBP, 
                          #tree_data$CONDID, 
                          #tree_data$TREE,
                          sep = "_")
# plot_env <- as.data.frame(env_data$plot_ID)
# plot_daymet <- as.data.frame(daymet$plot_ID)
env_data <- right_join(env_data, worldclim, by = "plot_ID")

### subset data if needed 

env_final_plot_data_subset <- env_data %>%
  filter(ownership == ownership_index[i])
unique(env_final_plot_data_subset$ownership)

# ENV VAR ####
### LMER ONLY ####
Env_stats <- data.frame()
env_model <- foreach(1:beta, .errorhandling = 'remove', .packages = c("lme4", "ranger", "dplyr")) %:%
  foreach(i= 1:length(ownership_index))%dopar% {
    
    final_plot_data_subset <- env_data %>% 
      filter(ownership == ownership_index[i])
    
    plotIDs <- unique(final_plot_data_subset$uniq_spl_unit_id)
    smp_size <- floor(0.7*length(plotIDs)) # switch to 0.8 for 80/20 split
    plot_ind <- as.data.frame(sample(plotIDs, size = smp_size))
    colnames(plot_ind) <- "uniq_spl_unit_id"
    
    boot_data <- inner_join(final_plot_data_subset, plot_ind)
    boot_data_test <- anti_join(final_plot_data_subset, boot_data)
    
    # Run model
    # daymet 
    #mixed_model_1 <- lmer(cpa ~ dayl + prcp + srad + swe + tmax + tmin + vp + (1|fortypcd), data = boot_data) 
    
    # worldclim
    mixed_model_1 <- lmer(cpa ~ bio01+bio02+bio03+bio04+bio05+bio06+bio07+bio08+bio09+bio10+bio11+bio12+bio13+bio14+bio15+bio16+bio17+bio18+bio19+ (1|fortypcd), data = boot_data)
    
    boot_data$residuals <- resid(mixed_model_1)
    
    model.summary <- summary(mixed_model_1)
    model.coef <- data.frame(t(model.summary$coefficients[,1]))
    random.stddev <- sqrt(model.summary$varcor$fortypcd[1])
    
    #prediction
    boot_data$predictions <- predict(mixed_model_1, newdata = boot_data)
    
    # prediction for training dataset
    mb <- as.data.frame(mean(boot_data$cpa - boot_data$predictions, na.rm = TRUE))
    names(mb) <- "mb"
    rmse <- as.data.frame(sqrt(mean((boot_data$cpa - boot_data$predictions)^2, na.rm = T)))
    names(rmse) <- "rmse"
    r2 <- 1 - (sum((boot_data$cpa-boot_data$predictions)^2)/sum((boot_data$cpa-mean(boot_data$cpa))^2))
    names(r2) <- "r2"
    
    # prepare the data for ranger
    # Daymet
    #variables <- c("dayl", "prcp", "srad", "swe", "tmax", "tmin", "vp")
    
    # World Clim
    variables <- colnames(worldclim[,-1])
    
    env_data_test <- boot_data_test[, variables]
    env_data_test$cpa <- boot_data_test$cpa
    env_data_test_fortypcd <- env_data_test
    env_data_test_fortypcd$fortypcd <- boot_data_test$fortypcd
    
    # testing dataset: get predictions
    predicted_mm_test <- predict(mixed_model_1, newdata = env_data_test_fortypcd, allow.new.levels = TRUE) # predictions from lme
    
    mb_test <- as.data.frame(mean(env_data_test$cpa - predicted_mm_test, na.rm = T))
    names(mb_test) <- "mb_test"
    rmse_test <- as.data.frame(sqrt(mean((env_data_test$cpa - predicted_mm_test)^2, na.rm = T)))
    names(rmse_test) <- "rmse_test"
    r2_test <- 1 - (sum((env_data_test$cpa-predicted_mm_test)^2)/sum((env_data_test$cpa-mean(predicted_mm_test))^2))
    names(r2_test) <- "r2_test"
    aic <- AIC(mixed_model_1)
    names(aic) <- "AIC"
    
    
    stat <- cbind(mb, rmse, r2, mb_test, rmse_test, r2_test, aic, random.stddev)
    stat$ownership <- ownership_index[i]
    #stat <- cbind(stat, model.coef)
    
    return(stat)
    
  }


env_model1 <- do.call(rbind, env_model)
env_model1 <- do.call(rbind, env_model1)

# Model fit statistics
env_model_stats <- env_model1 %>%
  dplyr::group_by(ownership) %>% 
  dplyr::summarise_all(.funs = c("low"=~quantile(., probs = 0.025),
                                 "med"=~quantile(., probs = 0.5),
                                 "up"=~quantile(., probs = 0.975)),
                       na.rm = T)
env_model_stats=env_model_stats[,order(colnames(env_model_stats))]

Env_stats <- rbind(Env_stats, env_model_stats)
clipr::write_last_clip()
print(Env_stats)


### ENV RF ####

env_RF_stats <- data.frame()

env_RF_MODEL <- foreach(1:beta, .errorhandling = 'remove', .packages = c("lme4", "ranger", "dplyr")) %:%
  foreach(i= 1:length(ownership_index))%dopar% {
    final_plot_data_subset <- env_data %>% 
      filter(ownership == ownership_index[i])
    plotIDs <- unique(final_plot_data_subset$uniq_spl_unit_id)
    smp_size <- floor(0.7*length(plotIDs)) # switch to 0.8 for 80/20 split
    plot_ind <- as.data.frame(sample(plotIDs, size = smp_size))
    colnames(plot_ind) <- "uniq_spl_unit_id"
    
    boot_data <- inner_join(final_plot_data_subset, plot_ind)
    boot_data_test <- anti_join(final_plot_data_subset, boot_data)
    
    # Define the control for cross-validation
    mixed_model_1 <- lmer(cpa ~ bio01+bio02+bio03+bio04+bio05+bio06+bio07+bio08+bio09+bio10+bio11+bio12+bio13+bio14+bio15+bio16+bio17+bio18+bio19+ (1|fortypcd), data = boot_data) #formula is the one you use. For example, H~ D + DI + (1|subplot)
    # Extract residuals
    boot_data$residuals <- resid(mixed_model_1)
    
    # Prepare the data for ranger
    # Daymet
    #variables <- c("dayl", "prcp", "srad", "swe", "tmax", "tmin", "vp")
    # World Clim
    variables <- colnames(worldclim[,-1])
    
    rf_data <- boot_data[, variables]
    rf_data$residuals <- boot_data$residuals
    
    # Define the control for cross-validation
    train_control <- trainControl(method = "cv",
                                  number = 5,
                                  verboseIter = TRUE,
                                  returnData = FALSE,
                                  allowParallel = TRUE)
    
    # Train the random forest model using ranger with cross-validation
    rf_model <- train(residuals ~ .,
                      data = rf_data,
                      method = "ranger",
                      trControl = train_control,
                      importance = 'permutation')
    
    #Prediction
    predictions <- predict(mixed_model_1, newdata=boot_data)
    predictions_rf <- predict(rf_model, newdata = rf_data)
    rf_data$final_predictions <- predictions + predictions_rf
    rf_data$cpa <- boot_data$cpa
    
    # # Extract variable importance - no longer works since it's just on residuals from lme?
    # importance_matrix <- as.data.frame(varImp(rf_model)$importance)
    # importance_matrix <- data.frame(Feature = rownames(importance_matrix), Importance = importance_matrix[, 1])
    # importance_matrix$rank <- rank(-importance_matrix$Importance)
    
    # Prediction for training dataset
    mb <- as.data.frame(mean(rf_data$cpa - rf_data$final_predictions, na.rm = T))
    names(mb) <- "mb"
    rmse <- as.data.frame(sqrt(mean((rf_data$cpa - rf_data$final_predictions)^2, na.rm = T)))
    names(rmse) <- "rmse"
    R2 <- 1 - (sum((rf_data$cpa-rf_data$final_predictions)^2)/sum((rf_data$cpa-mean(rf_data$cpa))^2))
    names(R2) <- "R2"
    
    rf_data_test <- boot_data_test[, variables]
    rf_data_test$cpa <- boot_data_test$cpa
    rf_data_test_fortypcd <- rf_data_test
    rf_data_test_fortypcd$fortypcd <- boot_data_test$fortypcd
    
    # testing dataset: get predictions
    predicted_mm_test <- predict(mixed_model_1, newdata = rf_data_test_fortypcd, allow.new.levels = TRUE) # predictions from lme
    rf_data_test$residuals <- predicted_mm_test - rf_data_test$cpa # get residuals from lme
    predicted_rf_test <- predict(rf_model, newdata = rf_data_test) # predictions from RF model using residuals of lme model 
    rf_data_test$predicted <- predicted_mm_test + predicted_rf_test # sum up two predictions from lme and rf
    
    mb_test <- as.data.frame(mean(rf_data_test$cpa - rf_data_test$predicted, na.rm = T))
    names(mb_test) <- "mb_test"
    rmse_test <- as.data.frame(sqrt(mean((rf_data_test$cpa - rf_data_test$predicted)^2, na.rm = T)))
    names(rmse_test) <- "rmse_test"
    R2_test <- 1 - (sum((rf_data_test$cpa-rf_data_test$predicted)^2)/sum((rf_data_test$cpa-mean(rf_data_test$cpa))^2))
    names(R2_test) <- "R2_test"
    
    
    stat <- cbind(mb, rmse, R2, mb_test, rmse_test, R2_test)
    stat$ownership <- ownership_index[i]
    return(stat)
  }

env_RF_MODEL1 <- do.call(rbind, env_RF_MODEL)
env_RF_MODEL1 <- do.call(rbind, env_RF_MODEL1)

env_RF_model_stats <- env_RF_MODEL1 %>% 
  dplyr::group_by(ownership) %>% 
  dplyr::summarise_all(.funs = c("low"=~quantile(., probs = 0.025),
                                 "med"=~quantile(., probs = 0.5),
                                 "up"=~quantile(., probs = 0.975)),
                       na.rm = T)
env_RF_model_stats=env_RF_model_stats[,order(colnames(env_RF_model_stats))]
env_RF_stats <- rbind(env_RF_stats, env_RF_model_stats)
clipr::write_last_clip()
print(env_RF_stats)


## LASSO ####

lasso_env_stats <- data.frame()

lasso_env_MODEL <- foreach(1:20, .errorhandling = 'remove', .packages = c("MASS", "nlme", "glmmLasso", "ranger", "dplyr")) %:%
  foreach(i= 1:length(ownership_index))%dopar% {
    
    final_plot_data_subset <- env_scaled %>% 
      filter(ownership == ownership_index[i])
    
    columns <- c("cpa", "uniq_spl_unit_id", "fortypcd", colnames(worldclim[,-1]))
    final_plot_data_subset <- final_plot_data_subset[, columns]
    variables <- colnames(worldclim[,-1])
    
    ## center all metric variables so that also the 
    ## starting values with glmmPQL are in the correct scaling
    final_plot_data_subset[, variables] <- scale(final_plot_data_subset[, variables], center=T, scale=T)
    final_plot_data_subset <- data.frame(final_plot_data_subset)
    
    plotIDs <- unique(final_plot_data_subset$uniq_spl_unit_id)
    smp_size <- floor(0.7*length(plotIDs)) # switch to 0.8 for 80/20 split
    plot_ind <- as.data.frame(sample(plotIDs, size = smp_size))
    colnames(plot_ind) <- "uniq_spl_unit_id"
    
    boot_data <- inner_join(final_plot_data_subset, plot_ind)
    boot_data <- boot_data[,-2]
    boot_data_test <- anti_join(final_plot_data_subset, boot_data)
    boot_data_test <- boot_data_test[,-2]
    boot_data_test$fortypcd <- factor(boot_data_test$fortypcd)
    boot_data$fortypcd <- factor(boot_data$fortypcd)
    
    lambda <- seq(500,0,by=-5)
    family <- gaussian(link = "identity")
    N<-dim(boot_data)[1]
    ind<-sample(N,N)
    
    kk<-5
    nk <- floor(N/kk)
    
    Devianz_ma<-matrix(Inf,ncol=kk,nrow=length(lambda))
    
    ## first fit good starting model
    PQL <- glmmPQL(cpa~1,random = ~1|fortypcd,family=family,data=boot_data)
    
    Delta.start <- as.matrix(t(c(as.numeric(PQL$coef$fixed),rep(0,19),as.numeric(t(PQL$coef$random$fortypcd)))))
    Q.start <- as.numeric(VarCorr(PQL)[1,1])
    
    
    ## loop over the folds  
    for (k in 1:kk)
    {
      print(paste("CV Loop ", k ,sep=""))
      
      if (k < kk)
      {
        indi <- ind[(k-1)*nk+(1:nk)]
      }else{
        indi <- ind[((k-1)*nk+1):N]
      }
      
      boot_data.train<-boot_data[-indi,]
      boot_data.test<-boot_data[indi,]
      Delta.temp <- Delta.start
      Q.temp <- Q.start
      
      ## loop over lambda grid
      for(l in 1:length(lambda))
      {
        #print(paste("Lambda Iteration ", j,sep=""))
        
        glm4 <- try(glmmLasso(cpa ~ bio01+bio02+bio03+bio04+bio05+bio06+bio07+bio08+bio09+bio10+bio11+bio12+bio13+bio14+bio15+bio16+bio17+bio18+bio19, rnd = list(fortypcd=~1),  
                              family = family, data = boot_data.train, lambda=lambda[l],switch.NR=FALSE,final.re=FALSE,
                              control=list(start=Delta.temp[l,],q_start=Q.temp[l]))
                    ,silent=TRUE) 
        if(!inherits(glm4, "try-error"))
        {  
          y.hat<-predict(glm4,boot_data.test)    
          Delta.temp<-rbind(Delta.temp,glm4$Deltamatrix[glm4$conv.step,])
          Q.temp<-c(Q.temp,glm4$Q_long[[glm4$conv.step+1]])
          
          Devianz_ma[l,k]<-sum(family$dev.resids(boot_data.test$cpa,y.hat,wt=rep(1,length(y.hat))))
        }
      }
    }
    
    Devianz_vec<-apply(Devianz_ma,1,sum)
    opt4<-which.min(Devianz_vec)
    
    ## now fit full model until optimal lambda (which is at opt4)
    for(o in 1:opt4)
    {  
      glm4.big <- glmmLasso(cpa ~ bio01+bio02+bio03+bio04+bio05+bio06+bio07+bio08+bio09+bio10+bio11+bio12+bio13+bio14+bio15+bio16+bio17+bio18+bio19, rnd = list(fortypcd=~1),  
                            family = family, data = boot_data, lambda=lambda[o],switch.NR=FALSE,final.re=FALSE,
                            control=list(start=Delta.start[o,],q_start=Q.start[o]))
      Delta.start<-rbind(Delta.start,glm4.big$Deltamatrix[glm4.big$conv.step,])
      Q.start<-c(Q.start,glm4.big$Q_long[[glm4.big$conv.step+1]])
    }
    
    glm4_final <- glm4.big
    
    summary(glm4_final)
    
    model.summary <- summary(glm4_final)
    model.coef <- data.frame(t(model.summary$coefficients[,1]))
    random.stddev <- model.summary$StdDev[1]
    
    #prediction
    boot_data$predictions <- predict(glm4_final, newdata = boot_data)
    
    # prediction for training dataset
    mb <- as.data.frame(mean(boot_data$cpa - boot_data$predictions, na.rm = T))
    names(mb) <- "mb"
    rmse <- as.data.frame(sqrt(mean((boot_data$cpa - boot_data$predictions)^2, na.rm = T)))
    names(rmse) <- "rmse"
    r2 <- 1 - (sum((boot_data$cpa-boot_data$predictions)^2)/sum((boot_data$cpa-mean(boot_data$cpa))^2))
    names(r2) <- "r2"
    
    para_data_test <- boot_data_test[, variables]
    para_data_test$cpa <- boot_data_test$cpa
    para_data_test_fortypcd <- para_data_test
    para_data_test_fortypcd$fortypcd <- boot_data_test$fortypcd
    
    # testing dataset: get predictions
    predicted_mm_test <- predict(glm4_final, newdata = para_data_test_fortypcd, allow.new.levels = TRUE) # predictions from lme
    
    mb_test <- as.data.frame(mean(para_data_test$cpa - predicted_mm_test, na.rm = T))
    names(mb_test) <- "mb_test"
    rmse_test <- as.data.frame(sqrt(mean((para_data_test$cpa - predicted_mm_test)^2, na.rm = T)))
    names(rmse_test) <- "rmse_test"
    r2_test <- 1 - (sum((para_data_test$cpa-predicted_mm_test)^2)/sum((para_data_test$cpa-mean(predicted_mm_test))^2))
    names(r2_test) <- "r2_test"
    aic <- glm4_final$aic
    names(aic) <- "aic"
    bic <- glm4_final$bic
    names(bic) <- "bic"
    
    
    stat <- cbind(mb, rmse, r2, mb_test, rmse_test, r2_test, aic, bic)
    stat <- cbind(stat, model.coef)
    stat$ownership = ownership_index[i]
    
    return(stat)
  }

lasso_env_stats <- do.call(rbind, lasso_env_MODEL)
lasso_env_stats <- do.call(rbind, lasso_env_stats)
lasso_env_model_stats <- lasso_env_stats %>% 
  dplyr::group_by(ownership) %>% 
  dplyr::summarise_all(.funs = c("low"=~quantile(., probs = 0.025),
                                 "med"=~quantile(., probs = 0.5),
                                 "up"=~quantile(., probs = 0.975)),
                       na.rm = T)
lasso_env_model_stats=lasso_env_model_stats[,order(colnames(lasso_env_model_stats))]
clipr::write_last_clip()
print(lasso_env_model_stats)

## Lasso and RF ####
lasso_RF_env_stats <- data.frame()                                                                                                                                                                         

lasso_RF_env_MODEL <- foreach(1:500, .errorhandling = 'remove', .packages = c("MASS", "nlme", "glmmLasso", "ranger", "dplyr")) %:%
  foreach(i= 1:length(ownership_index) )%dopar% {
    
    final_plot_data_subset <- env_data %>% 
      filter(ownership == ownership_index[i])
    unique(final_plot_data_subset$ownership)
    
    columns <- c("cpa", "uniq_spl_unit_id", "fortypcd", "dayl", "prcp", "srad", "swe", "tmax", "tmin", "vp")
    final_plot_data_subset <- final_plot_data_subset[, columns]
    variables <- c("dayl", "prcp", "srad", "swe", "tmax", "tmin", "vp")
    final_plot_data_subset_unscaled <- final_plot_data_subset
    ## center all metric variables so that also the 
    ## starting values with glmmPQL are in the correct scaling
    final_plot_data_subset[, variables] <- scale(final_plot_data_subset[, variables], center=T, scale=T) # THIS IS CAUSING ISSUES 
    final_plot_data_subset <- data.frame(final_plot_data_subset)
    
    plotIDs <- unique(final_plot_data_subset$uniq_spl_unit_id)
    smp_size <- floor(0.7*length(plotIDs)) # switch to 0.8 for 80/20 split
    plot_ind <- as.data.frame(sample(plotIDs, size = smp_size))
    colnames(plot_ind) <- "uniq_spl_unit_id"
    
    boot_data <- inner_join(final_plot_data_subset, plot_ind)
    boot_data <- boot_data[,-2]
    boot_data_test <- anti_join(final_plot_data_subset, boot_data)
    boot_data_test <- boot_data_test[,-2]
    boot_data_test$fortypcd <- factor(boot_data_test$fortypcd)
    boot_data$fortypcd <- factor(boot_data$fortypcd)
    
    lambda <- seq(500,0,by=-5)
    family <- gaussian(link = "identity")
    N<-dim(boot_data)[1]
    ind<-sample(N,N)
    
    kk<-5
    nk <- floor(N/kk)
    
    Devianz_ma<-matrix(Inf,ncol=kk,nrow=length(lambda))
    
    ## first fit good starting model
    PQL <- glmmPQL(cpa~1,random = ~1|fortypcd,family=family,data=boot_data)
    
    Delta.start <- as.matrix(t(c(as.numeric(PQL$coef$fixed),rep(0,9),as.numeric(t(PQL$coef$random$fortypcd)))))
    Q.start <- as.numeric(VarCorr(PQL)[1,1])
    
    
    ## loop over the folds  
    for (k in 1:kk)
    {
      print(paste("CV Loop ", k ,sep=""))
      
      if (k < kk)
      {
        indi <- ind[(k-1)*nk+(1:nk)]
      }else{
        indi <- ind[((k-1)*nk+1):N]
      }
      
      boot_data.train<-boot_data[-indi,]
      boot_data.test<-boot_data[indi,]
      Delta.temp <- Delta.start
      Q.temp <- Q.start
      
      ## loop over lambda grid
      for(l in 1:length(lambda))
      {
        #print(paste("Lambda Iteration ", j,sep=""))
        
        glm4 <- try(glmmLasso(cpa ~  dayl + prcp + srad + swe + tmax + tmin + vp, rnd = list(fortypcd=~1),  
                              family = family, data = boot_data.train, lambda=lambda[l],switch.NR=FALSE,final.re=FALSE,
                              control=list(start=Delta.temp[l,],q_start=Q.temp[l]))
                    ,silent=TRUE) 
        if(!inherits(glm4, "try-error"))
        {  
          y.hat<-predict(glm4,boot_data.test)    
          Delta.temp<-rbind(Delta.temp,glm4$Deltamatrix[glm4$conv.step,])
          Q.temp<-c(Q.temp,glm4$Q_long[[glm4$conv.step+1]])
          
          Devianz_ma[l,k]<-sum(family$dev.resids(boot_data.test$cpa,y.hat,wt=rep(1,length(y.hat))))
        }
      }
    }
    
    Devianz_vec<-apply(Devianz_ma,1,sum)
    opt4<-which.min(Devianz_vec)
    
    ## now fit full model until optimal lambda (which is at opt4)
    for(o in 1:opt4)
    {  
      glm4.big <- glmmLasso(cpa ~  dayl + prcp + srad + swe + tmax + tmin + vp, rnd = list(fortypcd=~1),  
                            family = family, data = boot_data, lambda=lambda[o],switch.NR=FALSE,final.re=FALSE,
                            control=list(start=Delta.start[o,],q_start=Q.start[o]))
      Delta.start<-rbind(Delta.start,glm4.big$Deltamatrix[glm4.big$conv.step,])
      Q.start<-c(Q.start,glm4.big$Q_long[[glm4.big$conv.step+1]])
    }
    
    glm4_final <- glm4.big
    
    glm4_sum <- summary(glm4_final)
    
    
    #prediction
    boot_data$residual <-  resid(glm4_final)
    boot_data_temp <- boot_data
    boot_data_temp$fitted <- predict(glm4_final, boot_data_temp)
    boot_data_temp$residual <- boot_data_temp$cpa - boot_data_temp$fitted
    boot_data$residual <- boot_data_temp$residual
    
    # Prepare the data for ranger
    variables <- c("dayl", "prcp", "srad", "swe", "tmax", "tmin", "vp")
    
    rf_data <- boot_data[, variables]
    rf_data$residuals <- boot_data$residual
    
    # Define the control for cross-validation
    train_control <- trainControl(method = "cv",
                                  number = 5,
                                  verboseIter = TRUE,
                                  returnData = FALSE,
                                  allowParallel = TRUE)
    
    # Train the random forest model using ranger with cross-validation
    rf_model <- train(residuals ~ .,
                      data = rf_data,
                      method = "ranger",
                      trControl = train_control,
                      importance = 'permutation')
    
    #Prediction
    predictions <- predict(rf_model, newdata = rf_data)
    rf_data$final_predictions <- boot_data_temp$fitted + predictions
    rf_data$cpa <- boot_data$cpa
    
    
    # # Extract variable importance - no longer works since it's just on residuals from lme?
    # importance_matrix <- as.data.frame(varImp(rf_model)$importance)
    # importance_matrix <- data.frame(Feature = rownames(importance_matrix), Importance = importance_matrix[, 1])
    # importance_matrix$rank <- rank(-importance_matrix$Importance)
    
    # Prediction for training dataset
    mb <- as.data.frame(mean(rf_data$cpa - rf_data$final_predictions, na.rm = T))
    names(mb) <- "mb"
    rmse <- as.data.frame(sqrt(mean((rf_data$cpa - rf_data$final_predictions)^2, na.rm = T)))
    names(rmse) <- "rmse"
    R2 <- 1 - (sum((rf_data$cpa-rf_data$final_predictions)^2)/sum((rf_data$cpa-mean(rf_data$cpa))^2))
    names(R2) <- "R2"
    
    rf_data_test <- boot_data_test[, variables]
    rf_data_test$cpa <- boot_data_test$cpa
    rf_data_test_fortypcd <- rf_data_test
    rf_data_test_fortypcd$fortypcd <- boot_data_test$fortypcd
    
    # testing dataset: get predictions
    predicted_mm_test <- predict(glm4_final, newdata = rf_data_test_fortypcd, allow.new.levels = TRUE) # predictions from lme
    rf_data_test$residuals <- predicted_mm_test - rf_data_test$cpa # get residuals from lme
    predicted_rf_test <- predict(rf_model, newdata = rf_data_test) # predictions from RF model using residuals of lme model 
    rf_data_test$predicted <- predicted_mm_test + predicted_rf_test # sum up two predictions from lme and rf
    
    mb_test <- as.data.frame(mean(rf_data_test$cpa - rf_data_test$predicted, na.rm = T))
    names(mb_test) <- "mb_test"
    rmse_test <- as.data.frame(sqrt(mean((rf_data_test$cpa - rf_data_test$predicted)^2, na.rm = T)))
    names(rmse_test) <- "rmse_test"
    R2_test <- 1 - (sum((rf_data_test$cpa-rf_data_test$predicted)^2)/sum((rf_data_test$cpa-mean(rf_data_test$cpa))^2))
    names(R2_test) <- "R2_test"
    
    
    stat <- cbind(mb, rmse, R2, mb_test, rmse_test, R2_test)
    stat$ownership = ownership_index[i]
    return(stat)
    
  }

lasso_RF_env_stats <- do.call(rbind, lasso_RF_env_MODEL)
lasso_RF_env_stats <- do.call(rbind, lasso_RF_env_stats)
lasso_RF_env_model_stats <- lasso_RF_env_stats %>% 
  dplyr::group_by(ownership) %>% 
  dplyr::summarise_all(.funs = c("low"=~quantile(., probs = 0.025),
                                 "med"=~quantile(., probs = 0.5),
                                 "up"=~quantile(., probs = 0.975)),
                       na.rm = T)
lasso_RF_env_model_stats=lasso_RF_env_model_stats[,order(colnames(lasso_RF_env_model_stats))]
#lasso_RF_model_stats$ownership <- ownership_index[i]
#lasso_RF_stats <- rbind(lasso_RF_stats, lasso_RF_model_stats)
clipr::write_last_clip()
print(lasso_RF_env_model_stats)




# STAND-LEVEL VARIABLES ####
### - not updated to new variables ####
### LMER ONLY #### 
Para_stats <- data.frame()

beta = 1000

i = 8

# final_plot_data_subset <- final_plot_data %>% 
#   filter(ownership == ownership_index[i])
# unique(final_plot_data_subset$ownership)

para_model <-  foreach(1:beta, .errorhandling = 'remove', .packages = c("lme4", "ranger", "dplyr")) %:%
  foreach(i= 1:length(ownership_index))%dopar% {
    
    final_plot_data_subset <- env_data %>% 
      filter(ownership == ownership_index[i])
    scaling <- c("dayl", "swe", "vp", "prcp")
    final_plot_data_subset[scaling] <- scale(final_plot_data_subset[scaling])
    
    plotIDs <- unique(final_plot_data_subset$uniq_spl_unit_id)
    smp_size <- floor(0.7*length(plotIDs)) # switch to 0.8 for 80/20 split
    plot_ind <- as.data.frame(sample(plotIDs, size = smp_size))
    colnames(plot_ind) <- "uniq_spl_unit_id"
    
    boot_data <- inner_join(final_plot_data_subset, plot_ind)
    boot_data_test <- anti_join(final_plot_data_subset, boot_data)
    
    # Run model
    mixed_model_1 <- lmer(cpa ~ qmd + baph + mean_dia + mean_actualht + mean_HD + tph + slope + aspect + sdi + (1|fortypcd), data = boot_data) #formula is the one you use. For example, H~ D + DI + (1|subplot)
    boot_data$residuals <- resid(mixed_model_1)
    
    model.summary <- summary(mixed_model_1)
    model.coef <- data.frame(t(model.summary$coefficients[,1]))
    random.stddev <- sqrt(model.summary$varcor$fortypcd[1])
    
    #prediction
    boot_data$predictions <- predict(mixed_model_1, newdata = boot_data)
    
    # prediction for training dataset
    mb <- as.data.frame(mean(boot_data$cpa - boot_data$predictions, na.rm = TRUE))
    names(mb) <- "mb"
    rmse <- as.data.frame(sqrt(mean((boot_data$cpa - boot_data$predictions)^2, na.rm = T)))
    names(rmse) <- "rmse"
    r2 <- 1 - (sum((boot_data$cpa-boot_data$predictions)^2)/sum((boot_data$cpa-mean(boot_data$cpa))^2))
    names(r2) <- "r2"
    
    # prepare the data for ranger
    variables <- c("qmd", "baph", "mean_dia", "mean_actualht", "mean_HD", "tph", "aspect", "slope", "sdi")
    
    para_data_test <- boot_data_test[, variables]
    para_data_test$cpa <- boot_data_test$cpa
    para_data_test_fortypcd <- para_data_test
    para_data_test_fortypcd$fortypcd <- boot_data_test$fortypcd
    
    # testing dataset: get predictions
    predicted_mm_test <- predict(mixed_model_1, newdata = para_data_test_fortypcd, allow.new.levels = TRUE) # predictions from lme
    
    mb_test <- as.data.frame(mean(para_data_test$cpa - predicted_mm_test, na.rm = T))
    names(mb_test) <- "mb_test"
    rmse_test <- as.data.frame(sqrt(mean((para_data_test$cpa - predicted_mm_test)^2, na.rm = T)))
    names(rmse_test) <- "rmse_test"
    r2_test <- 1 - (sum((para_data_test$cpa-predicted_mm_test)^2)/sum((para_data_test$cpa-mean(predicted_mm_test))^2))
    names(r2_test) <- "r2_test"
    aic <- AIC(mixed_model_1)
    names(aic) <- "AIC"
    
    
    stat <- cbind(mb, rmse, r2, mb_test, rmse_test, r2_test, aic, random.stddev)
    stat <- cbind(stat, model.coef)
    stat$ownership <- ownership_index[i]
    return(stat)
    
  }


para_model1 <- do.call(rbind, para_model)
para_model1 <- do.call(rbind, para_model1)
# Model fit statistics
para_model_stats <- para_model1  %>% 
  dplyr::group_by(ownership) %>%
  dplyr::summarise_all(.funs = c("low"=~quantile(., probs = 0.025),
                                 "med"=~quantile(., probs = 0.5),
                                 "up"=~quantile(., probs = 0.975)),
                       na.rm = T)
para_model_stats=para_model_stats[,order(colnames(para_model_stats))]
Para_stats <- rbind(Para_stats, para_model_stats)
clipr::write_last_clip()
print(Para_stats)


### RF MODEL####
RF_stats <- data.frame()

RF_MODEL <- foreach(1:beta, .errorhandling = 'remove', .packages = c("lme4", "ranger", "dplyr")) %:%
  foreach(i= 1:length(ownership_index))%dopar% {
    final_plot_data_subset <- final_plot_data %>% 
      filter(ownership == ownership_index[i])
    unique(final_plot_data_subset$ownership)
    plotIDs <- unique(final_plot_data_subset$uniq_spl_unit_id)
    smp_size <- floor(0.7*length(plotIDs)) # switch to 0.8 for 80/20 split
    plot_ind <- as.data.frame(sample(plotIDs, size = smp_size))
    colnames(plot_ind) <- "uniq_spl_unit_id"
    
    boot_data <- inner_join(final_plot_data_subset, plot_ind)
    boot_data_test <- anti_join(final_plot_data_subset, boot_data)
    
    # Define the control for cross-validation
    mixed_model_1 <- lmer(cpa ~ qmd + baph + mean_dia + mean_actualht + mean_HD  + tph + slope + aspect + sdi + (1|fortypcd), data = boot_data) #formula is the one you use. For example, H~ D + DI + (1|subplot)
    # Extract residuals
    boot_data$residuals <- resid(mixed_model_1)
    
    # Prepare the data for ranger
    variables <- c("qmd", "baph", "mean_dia", "mean_actualht", "mean_HD", "tph", "slope", "aspect", "sdi")
    
    rf_data <- boot_data[, variables]
    rf_data$residuals <- boot_data$residuals
    
    # Define the control for cross-validation
    train_control <- trainControl(method = "cv",
                                  number = 5,
                                  verboseIter = TRUE,
                                  returnData = FALSE,
                                  allowParallel = TRUE)
    
    # Train the random forest model using ranger with cross-validation
    rf_model <- train(residuals ~ .,
                      data = rf_data,
                      method = "ranger",
                      trControl = train_control,
                      importance = 'permutation')
    
    #Prediction
    predictions <- predict(mixed_model_1, newdata=boot_data)
    predictions_rf <- predict(rf_model, newdata = rf_data)
    rf_data$final_predictions <- predictions + predictions_rf
    rf_data$cpa <- boot_data$cpa
    
    # # Extract variable importance - no longer works since it's just on residuals from lme?
    # importance_matrix <- as.data.frame(varImp(rf_model)$importance)
    # importance_matrix <- data.frame(Feature = rownames(importance_matrix), Importance = importance_matrix[, 1])
    # importance_matrix$rank <- rank(-importance_matrix$Importance)
    
    # Prediction for training dataset
    mb <- as.data.frame(mean(rf_data$cpa - rf_data$final_predictions, na.rm = T))
    names(mb) <- "mb"
    rmse <- as.data.frame(sqrt(mean((rf_data$cpa - rf_data$final_predictions)^2, na.rm = T)))
    names(rmse) <- "rmse"
    R2 <- 1 - (sum((rf_data$cpa-rf_data$final_predictions)^2)/sum((rf_data$cpa-mean(rf_data$cpa))^2))
    names(R2) <- "R2"
    
    rf_data_test <- boot_data_test[, variables]
    rf_data_test$cpa <- boot_data_test$cpa
    rf_data_test_fortypcd <- rf_data_test
    rf_data_test_fortypcd$fortypcd <- boot_data_test$fortypcd
    
    # testing dataset: get predictions
    predicted_mm_test <- predict(mixed_model_1, newdata = rf_data_test_fortypcd, allow.new.levels = TRUE) # predictions from lme
    rf_data_test$residuals <- predicted_mm_test - rf_data_test$cpa # get residuals from lme
    predicted_rf_test <- predict(rf_model, newdata = rf_data_test) # predictions from RF model using residuals of lme model 
    rf_data_test$predicted <- predicted_mm_test + predicted_rf_test # sum up two predictions from lme and rf
    
    mb_test <- as.data.frame(mean(rf_data_test$cpa - rf_data_test$predicted, na.rm = T))
    names(mb_test) <- "mb_test"
    rmse_test <- as.data.frame(sqrt(mean((rf_data_test$cpa - rf_data_test$predicted)^2, na.rm = T)))
    names(rmse_test) <- "rmse_test"
    R2_test <- 1 - (sum((rf_data_test$cpa-rf_data_test$predicted)^2)/sum((rf_data_test$cpa-mean(rf_data_test$cpa))^2))
    names(R2_test) <- "R2_test"
    
    
    stat <- cbind(mb, rmse, R2, mb_test, rmse_test, R2_test)
    stat$ownership <- ownership_index[i]
    return(stat)
  }

RF_MODEL1 <- do.call(rbind, RF_MODEL)
RF_MODEL1 <- do.call(rbind, RF_MODEL1)

RF_model_stats <- RF_MODEL1 %>% 
  dplyr::group_by(ownership) %>% 
  dplyr::summarise_all(.funs = c("low"=~quantile(., probs = 0.025),
                                 "med"=~quantile(., probs = 0.5),
                                 "up"=~quantile(., probs = 0.975)),
                       na.rm = T)
RF_model_stats=RF_model_stats[,order(colnames(RF_model_stats))]
RF_stats <- rbind(RF_stats, RF_model_stats)
clipr::write_last_clip()
print(RF_stats)

## LASSO only ####
lasso_stats <- data.frame()

lasso_stats <- foreach(1:beta, .errorhandling = 'remove', .packages = c("MASS", "nlme", "glmmLasso", "dplyr")) %:%
  #for(j in 1:beta){
  # print(paste("J Loop ", j ,sep="")) 
  foreach(i= 1:length(ownership_index) )%dopar% {
    
    final_plot_data_subset <- final_plot_data %>% 
      filter(ownership == ownership_index[i])
    print(paste("Ownership ", ownership_index[i] ,sep=""))
    columns <- c("cpa", "uniq_spl_unit_id", "fortypcd", "qmd", "baph", "mean_dia", "mean_actualht", "mean_HD", "tph", "slope", "aspect", "sdi")
    final_plot_data_subset <- final_plot_data_subset[, columns]
    variables <- c("qmd", "baph", "mean_dia", "mean_actualht", "mean_HD", "tph", "slope", "aspect", "sdi")
    
    ## center all metric variables so that also the 
    ## starting values with glmmPQL are in the correct scaling
    final_plot_data_subset[, variables] <- scale(final_plot_data_subset[, variables], center=T, scale=T) 
    final_plot_data_subset <- data.frame(final_plot_data_subset)
    
    
    plotIDs <- unique(final_plot_data_subset$uniq_spl_unit_id)
    smp_size <- floor(0.7*length(plotIDs)) # switch to 0.8 for 80/20 split
    plot_ind <- as.data.frame(sample(plotIDs, size = smp_size))
    colnames(plot_ind) <- "uniq_spl_unit_id"
    
    boot_data <- inner_join(final_plot_data_subset, plot_ind)
    boot_data <- boot_data[,-2]
    boot_data_test <- anti_join(final_plot_data_subset, boot_data)
    boot_data_test <- boot_data_test[,-2]
    boot_data_test$fortypcd <- factor(boot_data_test$fortypcd)
    boot_data$fortypcd <- factor(boot_data$fortypcd)
    
    lambda <- seq(500,0,by=-5)
    family <- gaussian(link = "identity")
    N<-dim(boot_data)[1]
    ind<-sample(N,N)
    
    kk<-5
    nk <- floor(N/kk)
    
    Devianz_ma<-matrix(Inf,ncol=kk,nrow=length(lambda))
    
    ## first fit good starting model
    PQL <- glmmPQL(cpa~1,random = ~1|fortypcd,family=family,data=boot_data)
    
    Delta.start <- as.matrix(t(c(as.numeric(PQL$coef$fixed),rep(0,9),as.numeric(t(PQL$coef$random$fortypcd)))))
    Q.start <- as.numeric(VarCorr(PQL)[1,1])
    
    
    ## loop over the folds  
    for (k in 1:kk)
    {
      print(paste("CV Loop ", k ,sep=""))
      
      if (k < kk)
      {
        indi <- ind[(k-1)*nk+(1:nk)]
      }else{
        indi <- ind[((k-1)*nk+1):N]
      }
      
      boot_data.train<-boot_data[-indi,]
      boot_data.test<-boot_data[indi,]
      
      Delta.temp <- Delta.start
      Q.temp <- Q.start
      
      
      ## loop over lambda grid
      for(l in 1:length(lambda))
      {
        #print(paste("Lambda Iteration ", j,sep=""))
        #print(paste("Check glm4")) 
        glm4 <- try(glmmLasso(cpa ~ qmd + baph + mean_dia + mean_actualht + mean_HD + tph + slope + aspect + sdi, rnd = list(fortypcd=~1),  
                              family = family, data = boot_data.train, lambda=lambda[l],switch.NR=FALSE,final.re=FALSE,
                              control=list(start=Delta.temp[l,],q_start=Q.temp[l]))
                    ,silent=TRUE) 
        
        
        if(!inherits(glm4, "try-error"))
        {  
          y.hat<-predict(glm4,boot_data.test)    
          Delta.temp<-rbind(Delta.temp,glm4$Deltamatrix[glm4$conv.step,])
          Q.temp<-c(Q.temp,glm4$Q_long[[glm4$conv.step+1]])
          
          Devianz_ma[l,k]<-sum(family$dev.resids(boot_data.test$cpa,y.hat,wt=rep(1,length(y.hat))))
        }
      }
    }
    
    Devianz_vec<-apply(Devianz_ma,1,sum)
    opt4<-which.min(Devianz_vec)
    
    ## now fit full model until optimal lambda (which is at opt4)
    for(o in 1:opt4)
    {  
      glm4.big <- glmmLasso(cpa ~ qmd + baph + mean_dia + mean_actualht + mean_HD + tph + aspect + slope + sdi, rnd = list(fortypcd=~1),  
                            family = family, data = boot_data, lambda=lambda[o],switch.NR=FALSE,final.re=FALSE,
                            control=list(start=Delta.start[o,],q_start=Q.start[o]))
      Delta.start<-rbind(Delta.start,glm4.big$Deltamatrix[glm4.big$conv.step,])
      Q.start<-c(Q.start,glm4.big$Q_long[[glm4.big$conv.step+1]])
      
      # print(paste("Check 3"))
    }
    
    glm4_final <- glm4.big
    
    summary(glm4_final)
    
    model.summary <- summary(glm4_final)
    model.coef <- data.frame(t(model.summary$coefficients[,1]))
    random.stddev <- model.summary$StdDev[1]
    
    #prediction
    boot_data$predictions <- predict(glm4_final, newdata = boot_data)
    
    # prediction for training dataset
    mb <- as.data.frame(mean(boot_data$cpa - boot_data$predictions, na.rm = T))
    names(mb) <- "mb"
    rmse <- as.data.frame(sqrt(mean((boot_data$cpa - boot_data$predictions)^2, na.rm = T)))
    names(rmse) <- "rmse"
    r2 <- 1 - (sum((boot_data$cpa-boot_data$predictions)^2)/sum((boot_data$cpa-mean(boot_data$cpa))^2))
    names(r2) <- "r2"
    
    # prepare the data for ranger
    variables <- c("qmd", "baph", "mean_dia", "mean_actualht", "mean_HD", "tph", "aspect", "slope", "sdi")
    
    para_data_test <- boot_data_test[, variables]
    para_data_test$cpa <- boot_data_test$cpa
    para_data_test_fortypcd <- para_data_test
    para_data_test_fortypcd$fortypcd <- boot_data_test$fortypcd
    
    # testing dataset: get predictions
    predicted_mm_test <- predict(glm4_final, newdata = para_data_test_fortypcd, allow.new.levels = TRUE) # predictions from lme
    
    mb_test <- as.data.frame(mean(para_data_test$cpa - predicted_mm_test, na.rm = T))
    names(mb_test) <- "mb_test"
    rmse_test <- as.data.frame(sqrt(mean((para_data_test$cpa - predicted_mm_test)^2, na.rm = T)))
    names(rmse_test) <- "rmse_test"
    r2_test <- 1 - (sum((para_data_test$cpa-predicted_mm_test)^2)/sum((para_data_test$cpa-mean(predicted_mm_test))^2))
    names(r2_test) <- "r2_test"
    
    
    stat <- cbind(mb, rmse, r2, mb_test, rmse_test, r2_test, random.stddev)
    stat <- cbind(stat, model.coef)
    stat$ownership = ownership_index[i]
    
    
    return(stat)
  }


lasso_stats1 <- do.call(rbind, lasso_stats)
lasso_stats1 <- do.call(rbind, lasso_stats1)

var_stats_summary <- lasso_stats1 %>%
  group_by(ownership) %>% 
  dplyr::summarise_all(.funs = c("low"=~quantile(., probs = 0.025),
                                 "med"=~quantile(., probs = 0.5),
                                 "up"=~quantile(., probs = 0.975)),
                       na.rm = T)
var_stats_summary=var_stats_summary[,order(colnames(var_stats_summary))]

clipr::write_last_clip()
print(var_stats_summary)

## LASSO and RF ####
# is ready to run! :)
lasso_RF_stats <- data.frame()

lasso_RF_MODEL <- foreach(1:beta, .errorhandling = 'remove', .packages = c("MASS", "nlme", "glmmLasso", "ranger", "dplyr")) %:%
  foreach(i= 1:length(ownership_index) )%dopar% {
  
    final_plot_data_subset <- final_plot_data %>% 
      filter(ownership == ownership_index[i])
    unique(final_plot_data_subset$ownership)
    
    columns <- c("cpa", "uniq_spl_unit_id", "fortypcd", "qmd", "baph", "mean_dia", "mean_actualht", "mean_HD", "tph", "slope", "aspect", "sdi")
    final_plot_data_subset <- final_plot_data_subset[, columns]
    variables <- c("qmd", "baph", "mean_dia", "mean_actualht", "mean_HD", "tph", "slope", "aspect", "sdi")
    final_plot_data_subset_unscaled <- final_plot_data_subset
    ## center all metric variables so that also the 
    ## starting values with glmmPQL are in the correct scaling
    final_plot_data_subset[, variables] <- scale(final_plot_data_subset[, variables], center=T, scale=T) # THIS IS CAUSING ISSUES 
    final_plot_data_subset <- data.frame(final_plot_data_subset)
    
    
    plotIDs <- unique(final_plot_data_subset$uniq_spl_unit_id)
    smp_size <- floor(0.7*length(plotIDs)) # switch to 0.8 for 80/20 split
    plot_ind <- as.data.frame(sample(plotIDs, size = smp_size))
    colnames(plot_ind) <- "uniq_spl_unit_id"
    
    boot_data <- inner_join(final_plot_data_subset, plot_ind)
    boot_data <- boot_data[,-2]
    boot_data_test <- anti_join(final_plot_data_subset, boot_data)
    boot_data_test <- boot_data_test[,-2]
    boot_data_test$fortypcd <- factor(boot_data_test$fortypcd)
    boot_data$fortypcd <- factor(boot_data$fortypcd)
    
    lambda <- seq(500,0,by=-5)
    family <- gaussian(link = "identity")
    N<-dim(boot_data)[1]
    ind<-sample(N,N)
    
    kk<-5
    nk <- floor(N/kk)
    
    Devianz_ma<-matrix(Inf,ncol=kk,nrow=length(lambda))
    
    ## first fit good starting model
    PQL <- glmmPQL(cpa~1,random = ~1|fortypcd,family=family,data=boot_data)
    
    Delta.start <- as.matrix(t(c(as.numeric(PQL$coef$fixed),rep(0,9),as.numeric(t(PQL$coef$random$fortypcd)))))
    Q.start <- as.numeric(VarCorr(PQL)[1,1])
    
    
    ## loop over the folds  
    for (k in 1:kk)
    {
      print(paste("CV Loop ", k ,sep=""))
      
      if (k < kk)
      {
        indi <- ind[(k-1)*nk+(1:nk)]
      }else{
        indi <- ind[((k-1)*nk+1):N]
      }
      
      boot_data.train<-boot_data[-indi,]
      boot_data.test<-boot_data[indi,]
      Delta.temp <- Delta.start
      Q.temp <- Q.start
      
      ## loop over lambda grid
      for(l in 1:length(lambda))
      {
        #print(paste("Lambda Iteration ", j,sep=""))
        
        glm4 <- try(glmmLasso(cpa ~ qmd + baph + mean_dia + mean_actualht + mean_HD + tph + slope + aspect + sdi, rnd = list(fortypcd=~1),  
                              family = family, data = boot_data.train, lambda=lambda[l],switch.NR=FALSE,final.re=FALSE,
                              control=list(start=Delta.temp[l,],q_start=Q.temp[l]))
                    ,silent=TRUE) 
        if(!inherits(glm4, "try-error"))
        {  
          y.hat<-predict(glm4,boot_data.test)    
          Delta.temp<-rbind(Delta.temp,glm4$Deltamatrix[glm4$conv.step,])
          Q.temp<-c(Q.temp,glm4$Q_long[[glm4$conv.step+1]])
          
          Devianz_ma[l,k]<-sum(family$dev.resids(boot_data.test$cpa,y.hat,wt=rep(1,length(y.hat))))
        }
      }
    }
    
    Devianz_vec<-apply(Devianz_ma,1,sum)
    opt4<-which.min(Devianz_vec)
    
    ## now fit full model until optimal lambda (which is at opt4)
    for(o in 1:opt4)
    {  
      glm4.big <- glmmLasso(cpa ~ qmd + baph + mean_dia + mean_actualht + mean_HD + tph + slope + aspect + sdi, rnd = list(fortypcd=~1),  
                            family = family, data = boot_data, lambda=lambda[o],switch.NR=FALSE,final.re=FALSE,
                            control=list(start=Delta.start[o,],q_start=Q.start[o]))
      Delta.start<-rbind(Delta.start,glm4.big$Deltamatrix[glm4.big$conv.step,])
      Q.start<-c(Q.start,glm4.big$Q_long[[glm4.big$conv.step+1]])
    }
    
    glm4_final <- glm4.big
    
    glm4_sum <- summary(glm4_final)
    
    model.summary <- summary(glm4_final)
    model.coef <- data.frame(t(model.summary$coefficients[,1]))
    random.stddev <- model.summary$StdDev[1]
    
    #prediction
    boot_data$residual <-  resid(glm4_final)
    boot_data_temp <- boot_data
    boot_data_temp$fitted <- predict(glm4_final, boot_data_temp)
    boot_data_temp$residual <- boot_data_temp$cpa - boot_data_temp$fitted
    boot_data$residual <- boot_data_temp$residual
    
    # Prepare the data for ranger
    variables <- c("qmd", "baph", "mean_dia", "mean_actualht", "mean_HD", "tph", "slope", "aspect", "sdi")
    
    rf_data <- boot_data[, variables]
    rf_data$residuals <- boot_data$residual
    
    # Define the control for cross-validation
    train_control <- trainControl(method = "cv",
                                  number = 5,
                                  verboseIter = TRUE,
                                  returnData = FALSE,
                                  allowParallel = TRUE)
    
    # Train the random forest model using ranger with cross-validation
    rf_model <- train(residuals ~ .,
                      data = rf_data,
                      method = "ranger",
                      trControl = train_control,
                      importance = 'permutation')
    
    #Prediction
    predictions <- predict(rf_model, newdata = rf_data)
    rf_data$final_predictions <- boot_data_temp$fitted + predictions
    rf_data$cpa <- boot_data$cpa
    
    
    # # Extract variable importance - no longer works since it's just on residuals from lme?
    # importance_matrix <- as.data.frame(varImp(rf_model)$importance)
    # importance_matrix <- data.frame(Feature = rownames(importance_matrix), Importance = importance_matrix[, 1])
    # importance_matrix$rank <- rank(-importance_matrix$Importance)
    
    # Prediction for training dataset
    mb <- as.data.frame(mean(rf_data$cpa - rf_data$final_predictions, na.rm = T))
    names(mb) <- "mb"
    rmse <- as.data.frame(sqrt(mean((rf_data$cpa - rf_data$final_predictions)^2, na.rm = T)))
    names(rmse) <- "rmse"
    R2 <- 1 - (sum((rf_data$cpa-rf_data$final_predictions)^2)/sum((rf_data$cpa-mean(rf_data$cpa))^2))
    names(R2) <- "R2"
    
    rf_data_test <- boot_data_test[, variables]
    rf_data_test$cpa <- boot_data_test$cpa
    rf_data_test_fortypcd <- rf_data_test
    rf_data_test_fortypcd$fortypcd <- boot_data_test$fortypcd
    
    # testing dataset: get predictions
    predicted_mm_test <- predict(glm4_final, newdata = rf_data_test_fortypcd, allow.new.levels = TRUE) # predictions from lme
    rf_data_test$residuals <- predicted_mm_test - rf_data_test$cpa # get residuals from lme
    predicted_rf_test <- predict(rf_model, newdata = rf_data_test) # predictions from RF model using residuals of lme model 
    rf_data_test$predicted <- predicted_mm_test + predicted_rf_test # sum up two predictions from lme and rf
    
    mb_test <- as.data.frame(mean(rf_data_test$cpa - rf_data_test$predicted, na.rm = T))
    names(mb_test) <- "mb_test"
    rmse_test <- as.data.frame(sqrt(mean((rf_data_test$cpa - rf_data_test$predicted)^2, na.rm = T)))
    names(rmse_test) <- "rmse_test"
    R2_test <- 1 - (sum((rf_data_test$cpa-rf_data_test$predicted)^2)/sum((rf_data_test$cpa-mean(rf_data_test$cpa))^2))
    names(R2_test) <- "R2_test"
    
    
    stat <- cbind(mb, rmse, R2, mb_test, rmse_test, R2_test, random.stddev)
    stat$ownership = ownership_index[i]
    stat <- cbind(stat, model.coef)
    return(stat)
    
  }

lasso_RF_stats <- do.call(rbind, lasso_RF_MODEL)
lasso_RF_stats <- do.call(rbind, lasso_RF_stats)
lasso_RF_model_stats <- lasso_RF_stats %>% 
  dplyr::group_by(ownership) %>% 
  dplyr::summarise_all(.funs = c("low"=~quantile(., probs = 0.025),
                                 "med"=~quantile(., probs = 0.5),
                                 "up"=~quantile(., probs = 0.975)),
                       na.rm = T)
lasso_RF_model_stats=lasso_RF_model_stats[,order(colnames(lasso_RF_model_stats))]
#lasso_RF_model_stats$ownership <- ownership_index[i]
#lasso_RF_stats <- rbind(lasso_RF_stats, lasso_RF_model_stats)
clipr::write_last_clip()
print(lasso_RF_model_stats)
lasso_RF_model_stats_50_samp <- lasso_RF_model_stats



## error handling for lambda ####
## loop over lambda grid
for (k in 1:kk)
{
  print(paste("CV Loop ", k ,sep=""))
  
  if (k < kk)
  {
    indi <- ind[(k-1)*nk+(1:nk)]
  }else{
    indi <- ind[((k-1)*nk+1):N]
  }
  
  boot_data.train<-boot_data[-indi,]
  boot_data.test<-boot_data[indi,]
  
  Delta.temp <- Delta.start
  Q.temp <- Q.start
  
  ## loop over lambda grid
  for(l in 1:length(lambda))
  {
    #print(paste("Lambda Iteration ", j,sep=""))
    
    glm4 <- try(glmmLasso(cpa ~ qmd + baph + mean_dia + mean_actualht + mean_HD + diversity + tph + slope + aspect, rnd = list(fortypcd=~1),  
                          family = family, data = boot_data.train, lambda=lambda[l],switch.NR=FALSE,final.re=FALSE,
                          control=list(start=Delta.temp[l,],q_start=Q.temp[l]))
                ,silent=TRUE) 
    
    if(!inherits(glm4, "try-error"))
    {  
      y.hat<-predict(glm4,boot_data.test)    
      Delta.temp<-rbind(Delta.temp,glm4$Deltamatrix[glm4$conv.step,])
      Q.temp<-c(Q.temp,glm4$Q_long[[glm4$conv.step+1]])
      
      Devianz_ma[l,k]<-sum(family$dev.resids(boot_data.test$cpa,y.hat,wt=rep(1,length(y.hat))))
    }
  }
}

# STAND + ENV ####
### LMER ONLY #### 
mixed_Para_stats <- data.frame()

beta = 1000


mixed_para_model <- foreach(1:beta, .errorhandling = 'remove', .packages = c("lme4", "ranger", "dplyr")) %:%
  foreach(i= 1:length(ownership_index))%dopar% {
    
    final_plot_data_subset <- env_data %>% 
      filter(ownership == ownership_index[i])
    
    plotIDs <- unique(final_plot_data_subset$uniq_spl_unit_id)
    smp_size <- floor(0.7*length(plotIDs)) # switch to 0.8 for 80/20 split
    plot_ind <- as.data.frame(sample(plotIDs, size = smp_size))
    colnames(plot_ind) <- "uniq_spl_unit_id"
    
    boot_data <- inner_join(final_plot_data_subset, plot_ind)
    boot_data_test <- anti_join(final_plot_data_subset, boot_data)
    
    # Run model
    mixed_model_1 <- lmer(cpa ~ bio01+bio02+bio03+bio04+bio05+bio06+bio07+bio08+bio09+bio10+bio11+bio12+bio13+bio14+bio15+bio16+bio17+bio18+bio19+
                            qmd + baph + mean_dia + mean_actualht + mean_HD + tph + slope + aspect + sdi + (1|fortypcd), data = boot_data) #formula is the one you use. For example, H~ D + DI + (1|subplot)
    boot_data$residuals <- resid(mixed_model_1)
    
    model.summary <- summary(mixed_model_1)
    model.coef <- data.frame(t(model.summary$coefficients[,1]))
    random.stddev <- sqrt(model.summary$varcor$fortypcd[1])
    
    #prediction
    boot_data$predictions <- predict(mixed_model_1, newdata = boot_data)
    
    # prediction for training dataset
    mb <- as.data.frame(mean(boot_data$cpa - boot_data$predictions, na.rm = TRUE))
    names(mb) <- "mb"
    rmse <- as.data.frame(sqrt(mean((boot_data$cpa - boot_data$predictions)^2, na.rm = T)))
    names(rmse) <- "rmse"
    r2 <- 1 - (sum((boot_data$cpa-boot_data$predictions)^2)/sum((boot_data$cpa-mean(boot_data$cpa))^2))
    names(r2) <- "r2"
    
    # prepare the data for ranger
    variables <- env_variables
    para_data_test <- boot_data_test[, variables]
    para_data_test$cpa <- boot_data_test$cpa
    para_data_test_fortypcd <- para_data_test
    para_data_test_fortypcd$fortypcd <- boot_data_test$fortypcd
    
    # testing dataset: get predictions
    predicted_mm_test <- predict(mixed_model_1, newdata = para_data_test_fortypcd, allow.new.levels = TRUE) # predictions from lme
    
    mb_test <- as.data.frame(mean(para_data_test$cpa - predicted_mm_test, na.rm = T))
    names(mb_test) <- "mb_test"
    rmse_test <- as.data.frame(sqrt(mean((para_data_test$cpa - predicted_mm_test)^2, na.rm = T)))
    names(rmse_test) <- "rmse_test"
    r2_test <- 1 - (sum((para_data_test$cpa-predicted_mm_test)^2)/sum((para_data_test$cpa-mean(predicted_mm_test))^2))
    names(r2_test) <- "r2_test"
    aic <- AIC(mixed_model_1)
    names(aic) <- "AIC"
    
  
    stat <- cbind(mb, rmse, r2, mb_test, rmse_test, r2_test, aic, random.stddev)
    #stat <- cbind(stat, model.coef)
    stat$ownership <- ownership_index[i]
    
    return(stat)
    
  }

mixed_para_model1 <- do.call(rbind, mixed_para_model)
mixed_para_model1 <- do.call(rbind, mixed_para_model1)

# Model fit statistics
mixed_para_model_stats <- mixed_para_model1 %>%
  dplyr::group_by(ownership) %>% 
  dplyr::summarise_all(.funs = c("low"=~quantile(., probs = 0.025),
                                 "med"=~quantile(., probs = 0.5),
                                 "up"=~quantile(., probs = 0.975)),
                       na.rm = T)

mixed_Para_stats <- rbind(mixed_Para_stats, mixed_para_model_stats)
clipr::write_last_clip()
print(mixed_Para_stats)


### RF MODEL####
RF_stats <- data.frame()

RF_MODEL <- foreach(1:5, .errorhandling = 'remove', .packages = c("lme4", "ranger", "dplyr")) %:%
  foreach(i= 9)%dopar% { #1:length(ownership_index)
    
    final_plot_data_subset <- env_data_ %>% 
      filter(ownership == ownership_index[i])
    # scaling <- c("dayl", "swe", "vp", "prcp")
    # final_plot_data_subset[scaling] <- scale(final_plot_data_subset[scaling])
    
    plotIDs <- unique(final_plot_data_subset$uniq_spl_unit_id)
    smp_size <- floor(0.7*length(plotIDs)) # switch to 0.8 for 80/20 split
    plot_ind <- as.data.frame(sample(plotIDs, size = smp_size))
    colnames(plot_ind) <- "uniq_spl_unit_id"
    
    boot_data <- inner_join(final_plot_data_subset, plot_ind)
    boot_data_test <- anti_join(final_plot_data_subset, boot_data)
    
    # Define the control for cross-validation
    mixed_model_1 <- lmer(cpa ~ bio01+bio02+bio03+bio04+bio05+bio06+bio07+bio08+bio09+bio10+bio11+bio12+bio13+bio14+bio15+bio16+bio17+bio18+bio19+ qmd + baph + mean_dia + mean_actualht + mean_HD + tph + slope + aspect + sdi + (1|fortypcd), data = boot_data) #formula is the one you use. For example, H~ D + DI + (1|subplot)
    # Extract residuals
    boot_data$residuals <- resid(mixed_model_1)
    
    # Prepare the data for ranger
    variables <- env_variables
    
    rf_data <- boot_data[, variables]
    rf_data$residuals <- boot_data$residuals
    
    # Define the control for cross-validation
    train_control <- trainControl(method = "cv",
                                  number = 5,
                                  verboseIter = TRUE,
                                  returnData = FALSE,
                                  allowParallel = TRUE)
    
    # Train the random forest model using ranger with cross-validation
    rf_model <- train(residuals ~ .,
                      data = rf_data,
                      method = "ranger",
                      trControl = train_control,
                      importance = 'permutation')
    
    #Prediction
    predictions <- predict(mixed_model_1, newdata=boot_data)
    predictions_rf <- predict(rf_model, newdata = rf_data)
    rf_data$final_predictions <- predictions + predictions_rf
    rf_data$cpa <- boot_data$cpa
    
    # # Extract variable importance - no longer works since it's just on residuals from lme?
    # importance_matrix <- as.data.frame(varImp(rf_model)$importance)
    # importance_matrix <- data.frame(Feature = rownames(importance_matrix), Importance = importance_matrix[, 1])
    # importance_matrix$rank <- rank(-importance_matrix$Importance)
    
    # Prediction for training dataset
    mb <- as.data.frame(mean(rf_data$cpa - rf_data$final_predictions, na.rm = T))
    names(mb) <- "mb"
    rmse <- as.data.frame(sqrt(mean((rf_data$cpa - rf_data$final_predictions)^2, na.rm = T)))
    names(rmse) <- "rmse"
    R2 <- 1 - (sum((rf_data$cpa-rf_data$final_predictions)^2)/sum((rf_data$cpa-mean(rf_data$cpa))^2))
    names(R2) <- "R2"
    
    rf_data_test <- boot_data_test[, variables]
    rf_data_test$cpa <- boot_data_test$cpa
    rf_data_test_fortypcd <- rf_data_test
    rf_data_test_fortypcd$fortypcd <- boot_data_test$fortypcd
    
    # testing dataset: get predictions
    predicted_mm_test <- predict(mixed_model_1, newdata = rf_data_test_fortypcd, allow.new.levels = TRUE) # predictions from lme
    rf_data_test$residuals <- predicted_mm_test - rf_data_test$cpa # get residuals from lme
    predicted_rf_test <- predict(rf_model, newdata = rf_data_test) # predictions from RF model using residuals of lme model 
    rf_data_test$predicted <- predicted_mm_test + predicted_rf_test # sum up two predictions from lme and rf
    
    mb_test <- as.data.frame(mean(rf_data_test$cpa - rf_data_test$predicted, na.rm = T))
    names(mb_test) <- "mb_test"
    rmse_test <- as.data.frame(sqrt(mean((rf_data_test$cpa - rf_data_test$predicted)^2, na.rm = T)))
    names(rmse_test) <- "rmse_test"
    R2_test <- 1 - (sum((rf_data_test$cpa-rf_data_test$predicted)^2)/sum((rf_data_test$cpa-mean(rf_data_test$cpa))^2))
    names(R2_test) <- "R2_test"
    
    
    stat <- cbind(mb, rmse, R2, mb_test, rmse_test, R2_test)
    stat$ownership <- ownership_index[i]
    return(stat)
  }

RF_MODEL1 <- do.call(rbind, RF_MODEL)
RF_MODEL1 <- do.call(rbind, RF_MODEL1)

RF_model_stats <- RF_MODEL1 %>% 
  dplyr::group_by(ownership) %>% 
  dplyr::summarise_all(.funs = c("low"=~quantile(., probs = 0.025),
                                 "med"=~quantile(., probs = 0.5),
                                 "up"=~quantile(., probs = 0.975)),
                       na.rm = T)
RF_model_stats=RF_model_stats[,order(colnames(RF_model_stats))]
RF_stats <- rbind(RF_stats, RF_model_stats)
clipr::write_last_clip()
print(RF_stats)

## LASSO only ####
mixed_lasso_stats <- data.frame()

mixed_lasso_stats <- for(j in 1:beta){#foreach(1:2, .errorhandling = 'remove', .packages = c("MASS", "nlme", "glmmLasso", "dplyr")) %:%
  #for(j in 1:beta){
  # print(paste("J Loop ", j ,sep="")) 
  for(i in 1:length(ownership_index)){  # foreach(i= 1:length(ownership_index) )%dopar% {
    
    final_plot_data_subset <- env_scaled %>% 
      filter(ownership == ownership_index[i])
    print(paste("Ownership ", ownership_index[i] ,sep=""))
    columns <- c("cpa", "uniq_spl_unit_id", "fortypcd", "qmd", "baph", "mean_dia", "mean_actualht", "mean_HD", "tph", "slope", "aspect", "sdi", colnames(worldclim[,-1]))
    final_plot_data_subset <- final_plot_data_subset[, columns]
    variables <- env_variables
    
    ## center all metric variables so that also the 
    ## starting values with glmmPQL are in the correct scaling
    final_plot_data_subset[, variables] <- scale(final_plot_data_subset[, variables], center=T, scale=T) # THIS IS CAUSING ISSUES 
    final_plot_data_subset <- data.frame(final_plot_data_subset)
    
    
    plotIDs <- unique(final_plot_data_subset$uniq_spl_unit_id)
    smp_size <- floor(0.7*length(plotIDs)) # switch to 0.8 for 80/20 split
    plot_ind <- as.data.frame(sample(plotIDs, size = smp_size))
    colnames(plot_ind) <- "uniq_spl_unit_id"
    
    boot_data <- inner_join(final_plot_data_subset, plot_ind)
    boot_data <- boot_data[,-2]
    boot_data_test <- anti_join(final_plot_data_subset, boot_data)
    boot_data_test <- boot_data_test[,-2]
    boot_data_test$fortypcd <- factor(boot_data_test$fortypcd)
    boot_data$fortypcd <- factor(boot_data$fortypcd)
    
    lambda <- seq(500,0,by=-5)
    family <- gaussian(link = "identity")
    N<-dim(boot_data)[1]
    ind<-sample(N,N)
    
    kk<-5
    nk <- floor(N/kk)
    
    Devianz_ma<-matrix(Inf,ncol=kk,nrow=length(lambda))
    
    ## first fit good starting model
    PQL <- glmmPQL(cpa~1,random = ~1|fortypcd,family=family,data=boot_data)
    
    Delta.start <- as.matrix(t(c(as.numeric(PQL$coef$fixed),rep(0,9),as.numeric(t(PQL$coef$random$fortypcd)))))
    Q.start <- as.numeric(VarCorr(PQL)[1,1])

    ## loop over the folds  
    for (k in 1:kk)
    {
      print(paste("CV Loop ", k ,sep=""))
      
      if (k < kk)
      {
        indi <- ind[(k-1)*nk+(1:nk)]
      }else{
        indi <- ind[((k-1)*nk+1):N]
      }
      
      boot_data.train<-boot_data[-indi,]
      boot_data.test<-boot_data[indi,]
      
      Delta.temp <- Delta.start
      Q.temp <- Q.start
      
      ## loop over lambda grid
      for(l in 1:length(lambda))
      {
        #print(paste("Lambda Iteration ", j,sep=""))
        print(paste("Check glm4")) 
        glm4 <- try(glmmLasso(cpa ~ bio01+bio02+bio03+bio04+bio05+bio06+bio07+bio08+bio09+bio10+bio11+bio12+bio13+bio14+bio15+bio16+bio17+bio18+bio19+ qmd + baph + mean_dia + mean_actualht + mean_HD + tph + slope + aspect + sdi, rnd = list(fortypcd=~1),  
                              family = family, data = boot_data.train, lambda=lambda[l],switch.NR=FALSE,final.re=FALSE,
                              control=list(start=Delta.temp[l,],q_start=Q.temp[l]))
                    ,silent=TRUE) 
        
        
        if(!inherits(glm4, "try-error"))
        {  
          y.hat<-predict(glm4,boot_data.test)    
          Delta.temp<-rbind(Delta.temp,glm4$Deltamatrix[glm4$conv.step,])
          Q.temp<-c(Q.temp,glm4$Q_long[[glm4$conv.step+1]])
          
          Devianz_ma[l,k]<-sum(family$dev.resids(boot_data.test$cpa,y.hat,wt=rep(1,length(y.hat))))
        }
      }
    }
    
    Devianz_vec<-apply(Devianz_ma,1,sum)
    opt4<-which.min(Devianz_vec)
    
    ## now fit full model until optimal lambda (which is at opt4)
    for(o in 1:opt4)
    {  
      glm4.big <- glmmLasso(cpa ~ bio01+bio02+bio03+bio04+bio05+bio06+bio07+bio08+bio09+bio10+bio11+bio12+bio13+bio14+bio15+bio16+bio17+bio18+bio19 + qmd + baph + mean_dia + mean_actualht + mean_HD + tph + aspect + slope + sdi, rnd = list(fortypcd=~1),  
                            family = family, data = boot_data, lambda=lambda[o],switch.NR=FALSE,final.re=FALSE,
                            control=list(start=Delta.start[o,],q_start=Q.start[o]))
      Delta.start<-rbind(Delta.start,glm4.big$Deltamatrix[glm4.big$conv.step,])
      Q.start<-c(Q.start,glm4.big$Q_long[[glm4.big$conv.step+1]])
    }
    
    glm4_final <- glm4.big
    
    summary(glm4_final)
    
    model.summary <- summary(glm4_final)
    model.coef <- data.frame(t(model.summary$coefficients[,1]))
    random.stddev <- model.summary$StdDev[1]
    
    #prediction
    boot_data$predictions <- predict(glm4_final, newdata = boot_data)
    
    # prediction for training dataset
    mb <- as.data.frame(mean(boot_data$cpa - boot_data$predictions, na.rm = T))
    names(mb) <- "mb"
    rmse <- as.data.frame(sqrt(mean((boot_data$cpa - boot_data$predictions)^2, na.rm = T)))
    names(rmse) <- "rmse"
    r2 <- 1 - (sum((boot_data$cpa-boot_data$predictions)^2)/sum((boot_data$cpa-mean(boot_data$cpa))^2))
    names(r2) <- "r2"
    
    para_data_test <- boot_data_test[, variables]
    para_data_test$cpa <- boot_data_test$cpa
    para_data_test_fortypcd <- para_data_test
    para_data_test_fortypcd$fortypcd <- boot_data_test$fortypcd
    
    # testing dataset: get predictions
    predicted_mm_test <- predict(glm4_final, newdata = para_data_test_fortypcd, allow.new.levels = TRUE) # predictions from lme
    
    mb_test <- as.data.frame(mean(para_data_test$cpa - predicted_mm_test, na.rm = T))
    names(mb_test) <- "mb_test"
    rmse_test <- as.data.frame(sqrt(mean((para_data_test$cpa - predicted_mm_test)^2, na.rm = T)))
    names(rmse_test) <- "rmse_test"
    r2_test <- 1 - (sum((para_data_test$cpa-predicted_mm_test)^2)/sum((para_data_test$cpa-mean(predicted_mm_test))^2))
    names(r2_test) <- "r2_test"
    
    
    stat <- cbind(mb, rmse, r2, mb_test, rmse_test, r2_test, random.stddev)
    stat <- cbind(stat, model.coef)
    stat$ownership = ownership_index[i]
    
    
    return(stat)
  }
}

mixed_lasso_stats <- do.call(rbind, mixed_lasso_stats)

mixed_lasso_summary <- mixed_lasso_stats %>%
  group_by(ownership) %>% 
  dplyr::summarise_all(.funs = c("low"=~quantile(., probs = 0.025),
                                 "med"=~quantile(., probs = 0.5),
                                 "up"=~quantile(., probs = 0.975)),
                       na.rm = T)
mixed_lasso_summary=mixed_lasso_summary[,order(colnames(mixed_lasso_summary))]

clipr::write_last_clip()
print(mixed_lasso_summary)

## LASSO and RF ####
mixed_lasso_RF_stats <- data.frame()

mixed_lasso_RF_MODEL <- foreach(1:beta, .errorhandling = 'remove', .packages = c("MASS", "nlme", "glmmLasso", "ranger", "dplyr")) %:%
  foreach(i= 1:length(ownership_index) )%dopar% {
    
    final_plot_data_subset <- env_scaled %>% 
      filter(ownership == ownership_index[i])
    unique(final_plot_data_subset$ownership)
    
    columns <- c("cpa", "uniq_spl_unit_id", "fortypcd", "qmd", "baph", "mean_dia", "mean_actualht", "mean_HD", "tph", "slope", "aspect", "sdi", colnames(worldclim[,-1]))
    final_plot_data_subset <- final_plot_data_subset[, columns]
    variables <- c("qmd", "baph", "mean_dia", "mean_actualht", "mean_HD", "tph", "slope", "aspect", "sdi", colnames(worldclim[,-1]))
    final_plot_data_subset_unscaled <- final_plot_data_subset
    ## center all metric variables so that also the 
    ## starting values with glmmPQL are in the correct scaling
    final_plot_data_subset[, variables] <- scale(final_plot_data_subset[, variables], center=T, scale=T) # THIS IS CAUSING ISSUES 
    final_plot_data_subset <- data.frame(final_plot_data_subset)
    
    
    plotIDs <- unique(final_plot_data_subset$uniq_spl_unit_id)
    smp_size <- floor(0.7*length(plotIDs)) # switch to 0.8 for 80/20 split
    plot_ind <- as.data.frame(sample(plotIDs, size = smp_size))
    colnames(plot_ind) <- "uniq_spl_unit_id"
    
    boot_data <- inner_join(final_plot_data_subset, plot_ind)
    boot_data <- boot_data[,-2]
    boot_data_test <- anti_join(final_plot_data_subset, boot_data)
    boot_data_test <- boot_data_test[,-2]
    boot_data_test$fortypcd <- factor(boot_data_test$fortypcd)
    boot_data$fortypcd <- factor(boot_data$fortypcd)
    
    lambda <- seq(500,0,by=-5)
    family <- gaussian(link = "identity")
    N<-dim(boot_data)[1]
    ind<-sample(N,N)
    
    kk<-5
    nk <- floor(N/kk)
    
    Devianz_ma<-matrix(Inf,ncol=kk,nrow=length(lambda))
    
    ## first fit good starting model
    PQL <- glmmPQL(cpa~1,random = ~1|fortypcd,family=family,data=boot_data)
    
    Delta.start <- as.matrix(t(c(as.numeric(PQL$coef$fixed),rep(0,9),as.numeric(t(PQL$coef$random$fortypcd)))))
    Q.start <- as.numeric(VarCorr(PQL)[1,1])
    
    
    ## loop over the folds  
    for (k in 1:kk)
    {
      print(paste("CV Loop ", k ,sep=""))
      
      if (k < kk)
      {
        indi <- ind[(k-1)*nk+(1:nk)]
      }else{
        indi <- ind[((k-1)*nk+1):N]
      }
      
      boot_data.train<-boot_data[-indi,]
      boot_data.test<-boot_data[indi,]
      Delta.temp <- Delta.start
      Q.temp <- Q.start
      
      ## loop over lambda grid
      for(l in 1:length(lambda))
      {
        #print(paste("Lambda Iteration ", j,sep=""))
        
        glm4 <- try(glmmLasso(cpa ~ bio01+bio02+bio03+bio04+bio05+bio06+bio07+bio08+bio09+bio10+bio11+bio12+bio13+bio14+bio15+bio16+bio17+bio18+bio19+ qmd + baph + mean_dia + mean_actualht + mean_HD + tph + slope + aspect + sdi, rnd = list(fortypcd=~1),  
                              family = family, data = boot_data.train, lambda=lambda[l],switch.NR=FALSE,final.re=FALSE,
                              control=list(start=Delta.temp[l,],q_start=Q.temp[l]))
                    ,silent=TRUE) 
        if(!inherits(glm4, "try-error"))
        {  
          y.hat<-predict(glm4,boot_data.test)    
          Delta.temp<-rbind(Delta.temp,glm4$Deltamatrix[glm4$conv.step,])
          Q.temp<-c(Q.temp,glm4$Q_long[[glm4$conv.step+1]])
          
          Devianz_ma[l,k]<-sum(family$dev.resids(boot_data.test$cpa,y.hat,wt=rep(1,length(y.hat))))
        }
      }
    }
    
    Devianz_vec<-apply(Devianz_ma,1,sum)
    opt4<-which.min(Devianz_vec)
    
    ## now fit full model until optimal lambda (which is at opt4)
    for(o in 1:opt4)
    {  
      glm4.big <- glmmLasso(cpa ~ bio01+bio02+bio03+bio04+bio05+bio06+bio07+bio08+bio09+bio10+bio11+bio12+bio13+bio14+bio15+bio16+bio17+bio18+bio19+ qmd + baph + mean_dia + mean_actualht + mean_HD + tph + slope + aspect + sdi, rnd = list(fortypcd=~1),  
                            family = family, data = boot_data, lambda=lambda[o],switch.NR=FALSE,final.re=FALSE,
                            control=list(start=Delta.start[o,],q_start=Q.start[o]))
      Delta.start<-rbind(Delta.start,glm4.big$Deltamatrix[glm4.big$conv.step,])
      Q.start<-c(Q.start,glm4.big$Q_long[[glm4.big$conv.step+1]])
    }
    
    glm4_final <- glm4.big
    
    glm4_sum <- summary(glm4_final)
    
    
    #prediction
    boot_data$residual <-  resid(glm4_final)
    boot_data_temp <- boot_data
    boot_data_temp$fitted <- predict(glm4_final, boot_data_temp)
    boot_data_temp$residual <- boot_data_temp$cpa - boot_data_temp$fitted
    boot_data$residual <- boot_data_temp$residual
    
    rf_data <- boot_data[, variables]
    rf_data$residuals <- boot_data$residual
    
    # Define the control for cross-validation
    train_control <- trainControl(method = "cv",
                                  number = 5,
                                  verboseIter = TRUE,
                                  returnData = FALSE,
                                  allowParallel = TRUE)
    
    # Train the random forest model using ranger with cross-validation
    rf_model <- train(residuals ~ .,
                      data = rf_data,
                      method = "ranger",
                      trControl = train_control,
                      importance = 'permutation')
    
    #Prediction
    predictions <- predict(rf_model, newdata = rf_data)
    rf_data$final_predictions <- boot_data_temp$fitted + predictions
    rf_data$cpa <- boot_data$cpa
    
    
    # # Extract variable importance - no longer works since it's just on residuals from lme?
    # importance_matrix <- as.data.frame(varImp(rf_model)$importance)
    # importance_matrix <- data.frame(Feature = rownames(importance_matrix), Importance = importance_matrix[, 1])
    # importance_matrix$rank <- rank(-importance_matrix$Importance)
    
    # Prediction for training dataset
    mb <- as.data.frame(mean(rf_data$cpa - rf_data$final_predictions, na.rm = T))
    names(mb) <- "mb"
    rmse <- as.data.frame(sqrt(mean((rf_data$cpa - rf_data$final_predictions)^2, na.rm = T)))
    names(rmse) <- "rmse"
    R2 <- 1 - (sum((rf_data$cpa-rf_data$final_predictions)^2)/sum((rf_data$cpa-mean(rf_data$cpa))^2))
    names(R2) <- "R2"
    
    rf_data_test <- boot_data_test[, variables]
    rf_data_test$cpa <- boot_data_test$cpa
    rf_data_test_fortypcd <- rf_data_test
    rf_data_test_fortypcd$fortypcd <- boot_data_test$fortypcd
    
    # testing dataset: get predictions
    predicted_mm_test <- predict(glm4_final, newdata = rf_data_test_fortypcd, allow.new.levels = TRUE) # predictions from lme
    rf_data_test$residuals <- predicted_mm_test - rf_data_test$cpa # get residuals from lme
    predicted_rf_test <- predict(rf_model, newdata = rf_data_test) # predictions from RF model using residuals of lme model 
    rf_data_test$predicted <- predicted_mm_test + predicted_rf_test # sum up two predictions from lme and rf
    
    mb_test <- as.data.frame(mean(rf_data_test$cpa - rf_data_test$predicted, na.rm = T))
    names(mb_test) <- "mb_test"
    rmse_test <- as.data.frame(sqrt(mean((rf_data_test$cpa - rf_data_test$predicted)^2, na.rm = T)))
    names(rmse_test) <- "rmse_test"
    R2_test <- 1 - (sum((rf_data_test$cpa-rf_data_test$predicted)^2)/sum((rf_data_test$cpa-mean(rf_data_test$cpa))^2))
    names(R2_test) <- "R2_test"
    
    
    stat <- cbind(mb, rmse, R2, mb_test, rmse_test, R2_test)
    stat$ownership = ownership_index[i]
    return(stat)
    
  }

mixed_lasso_RF_stats <- do.call(rbind, mixed_lasso_RF_MODEL)
mixed_lasso_RF_stats <- do.call(rbind, mixed_lasso_RF_stats)
mixed_lasso_RF_model_stats <- mixed_lasso_RF_stats %>% 
  dplyr::group_by(ownership) %>% 
  dplyr::summarise_all(.funs = c("low"=~quantile(., probs = 0.025),
                                 "med"=~quantile(., probs = 0.5),
                                 "up"=~quantile(., probs = 0.975)),
                       na.rm = T)
mixed_lasso_RF_model_stats=mixed_lasso_RF_model_stats[,order(colnames(mixed_lasso_RF_model_stats))]
#lasso_RF_model_stats$ownership <- ownership_index[i]
#lasso_RF_stats <- rbind(lasso_RF_stats, lasso_RF_model_stats)
clipr::write_last_clip()
print(mixed_lasso_RF_model_stats)

# Selected coordinates ####
raw_coord <- read.csv("ff_cond_coordinates.csv")
mod_coor <- raw_coord
mod_coor$plot_ID <- paste(mod_coor$STATECD, 
                          mod_coor$COUNTYCD, 
                          mod_coor$PLOT, 
                          #selected_coordinates$CONDID,
                          sep = "_")

fuzzy_coordinates <- read.csv("fuzzy_coordinates.csv") 

selected_coordinates <- final_plot_data
selected_coordinates$plot_ID <- paste(selected_coordinates$STATECD, 
                                      selected_coordinates$COUNTYCD, 
                                      selected_coordinates$PLOT, 
                                      #selected_coordinates$CONDID,
                                      sep = "_")
selected_coordinates <- unique(selected_coordinates[c("plot_ID")])
selected_coordinates <- right_join(fuzzy_coordinates, selected_coordinates)

selected_coordinates <- selected_coordinates[!is.na(selected_coordinates$LAT),]
selected_coordinates <- as.data.frame(unique(selected_coordinates))

write.csv(selected_coordinates, "selected_coordinates.csv")


# Subset all predictor variables ####

## ONLY PARAMETRIC PARTS #### 
variable_stats <- data.frame()

beta = 1000

i = 10

final_plot_data_subset <- final_plot_data %>% 
  filter(ownership == ownership_index[i])
unique(final_plot_data_subset$ownership)

var_stats <- foreach(1:beta, .errorhandling = 'remove', .packages = c("lme4", "ranger", "dplyr")) %dopar% {
  
  final_plot_data_subset <- final_plot_data %>% 
    filter(ownership == ownership_index[i])
  
  plotIDs <- unique(final_plot_data_subset$uniq_spl_unit_id)
  smp_size <- floor(0.7*length(plotIDs)) # switch to 0.8 for 80/20 split
  plot_ind <- as.data.frame(sample(plotIDs, size = smp_size))
  colnames(plot_ind) <- "uniq_spl_unit_id"
  
  boot_data <- inner_join(final_plot_data_subset, plot_ind)
  boot_data_test <- anti_join(final_plot_data_subset, boot_data)
  
  # Run model
  variables <- c("qmd", "baph", "mean_dia", "mean_actualht", "mean_HD", "tph")
  
  predictors <- boot_data[, variables]
  cols <- names(predictors)
  lapply(seq_along(cols), function(x) combn(cols, x, function(y) 
    glm(reformulate(y, "z"), data = data), simplify = FALSE))
  
  
  mixed_model_1 <- lmer(cpa ~ qmd + baph + mean_dia + mean_actualht + mean_HD + tph + (1|fortypcd), data = boot_data) #formula is the one you use. For example, H~ D + DI + (1|subplot)
  boot_data$residuals <- resid(mixed_model_1)
  
  #prediction
  boot_data$predictions <- predict(mixed_model_1, newdata = boot_data)
  
  # prediction for training dataset
  mb <- as.data.frame(mean(boot_data$cpa - boot_data$predictions, na.rm = T))
  names(mb) <- "mb"
  rmse <- as.data.frame(sqrt(mean((boot_data$cpa - boot_data$predictions)^2, na.rm = T)))
  names(rmse) <- "rmse"
  r2 <- 1 - (sum((boot_data$cpa-boot_data$predictions)^2)/sum((boot_data$cpa-mean(boot_data$cpa))^2))
  names(r2) <- "r2"
  
  # prepare the data for ranger
  variables <- c("qmd", "baph", "mean_dia", "mean_actualht", "mean_HD", "tph")
  
  para_data_test <- boot_data_test[, variables]
  para_data_test$cpa <- boot_data_test$cpa
  para_data_test_fortypcd <- para_data_test
  para_data_test_fortypcd$fortypcd <- boot_data_test$fortypcd
  
  # testing dataset: get predictions
  predicted_mm_test <- predict(mixed_model_1, newdata = para_data_test_fortypcd, allow.new.levels = TRUE) # predictions from lme
  
  mb_test <- as.data.frame(mean(para_data_test$cpa - predicted_mm_test, na.rm = T))
  names(mb_test) <- "mb_test"
  rmse_test <- as.data.frame(sqrt(mean((para_data_test$cpa - predicted_mm_test)^2, na.rm = T)))
  names(rmse_test) <- "rmse_test"
  r2_test <- 1 - (sum((para_data_test$cpa-predicted_mm_test)^2)/sum((para_data_test$cpa-mean(predicted_mm_test))^2))
  names(r2_test) <- "r2_test"
  
  
  stat <- cbind(mb, rmse, r2, mb_test, rmse_test, r2_test)
  return(stat)
  
}

para_model <- do.call(rbind, para_model)

# Model fit statistics
para_model_stats <- para_model %>%
  dplyr::summarise_all(.funs = c("low"=~quantile(., probs = 0.025),
                                 "med"=~quantile(., probs = 0.5),
                                 "up"=~quantile(., probs = 0.975)),
                       na.rm = T)
para_model_stats=para_model_stats[,order(colnames(para_model_stats))]
para_model_stats$ownership <- ownership_index[i]
Para_stats <- rbind(Para_stats, para_model_stats)
clipr::write_last_clip()
print(Para_stats)



# Lasso ####

