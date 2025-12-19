#Load packages
library(ggplot2)
library(dplyr)
library(readxl)
library(foreach)
library(doParallel)
library(ranger)
library(lme4)
library(stringr)
library(caret)
library(minpack.lm)
library(quantreg)
library(lqmm)
library(ForestFit)
library(MASS)
library(mgcv)
library(abdiv)
library(writexl)

#Smokies and Cherokee

setwd("C:/Users/tara/Desktop/carbon_project_1/")

#Import data
tree_data_og <- read.csv("ff_tree3.csv")
condition <- read.csv("ff_cond_clean.csv")

tree_data <- tree_data_og

tree_data <- tree_data[!is.na(tree_data$DIA), ]
tree_data <- tree_data[!is.na(tree_data$ACTUALHT), ]
tree_data$cpa <- tree_data$cpa/(0.4046*2000) # convert acres to hectares and lbs to tons
tree_data$tph <- tree_data$tpa/(0.4046856)

#### OLD METHODOLOGY ####

#OWNGRPCD = 10 Forest Service (OWNCD = 11, 12, 13).
#20 Other Federal (OWNCD 21, 22, 23, 24, 25).

#OWNCD = 11 National Forest. 12 National Grassland and/or Prairie. 13 Other Forest Service land.
#21 National Park Service. 22 Bureau of Land Management. 23 Fish and Wildlife Service. 24 Departments of Defense/Energy.
#25 Other Federal.

#select Cherokee national forest #ADFORCD 804
cond_Cherokee <- condition[which(condition$ADFORCD == 804), ]

tree_data_Cherokee <- lapply(1:nrow(cond_Cherokee), function(i){
  
  cond_Cherokee_1 <- cond_Cherokee[i, ] 
  
  tree_data_Cherokee_1 <- tree_data[which(tree_data$STATECD == cond_Cherokee_1$STATECD &
                                            tree_data$COUNTYCD == cond_Cherokee_1$COUNTYCD &
                                            tree_data$UNITCD == cond_Cherokee_1$UNITCD &
                                            tree_data$PLOT == cond_Cherokee_1$PLOT &
                                            tree_data$CONDID == cond_Cherokee_1$CONDID &
                                            tree_data$CYCLE == cond_Cherokee_1$CYCLE), ]
  
  return(tree_data_Cherokee_1)
  
})

tree_data_Cherokee <- do.call(rbind, tree_data_Cherokee)
tree_data_Cherokee$ownership <- "Cherokee NF"


#select GSMNP #OWNCD = 21 National Park Service #Statecd = TN(47) and NC (37)
# Tennessee: Sevier (155), Blount(9), and Cocke (29) counties.
# North Carolina: Swain (173) and Haywood (87) counties.

cond_GSMNP <- condition[which(condition$OWNCD == 21 & 
                                condition$STATECD == 37 &
                                condition$COUNTYCD == 173 |
                                condition$OWNCD == 21 & 
                                condition$STATECD == 37 &
                                condition$COUNTYCD == 87 |   
                                condition$OWNCD == 21 & 
                                condition$STATECD == 47 &
                                condition$COUNTYCD == 155 |
                                condition$OWNCD == 21 & 
                                condition$STATECD == 47 &
                                condition$COUNTYCD == 9 |
                                condition$OWNCD == 21 & 
                                condition$STATECD == 47 &
                                condition$COUNTYCD == 29
), ]

tree_data_GSMNP <- lapply(1:nrow(cond_GSMNP), function(i){
  
  cond_GSMNP_1 <- cond_GSMNP[i, ] 
  
  tree_data_GSMNP_1 <- tree_data[which(tree_data$STATECD == cond_GSMNP_1$STATECD &
                                         tree_data$COUNTYCD == cond_GSMNP_1$COUNTYCD &
                                         tree_data$UNITCD == cond_GSMNP_1$UNITCD &
                                         tree_data$PLOT == cond_GSMNP_1$PLOT &
                                         tree_data$CONDID == cond_GSMNP_1$CONDID &
                                         tree_data$CYCLE == cond_GSMNP_1$CYCLE), ]
  
  return(tree_data_GSMNP_1)
  
})

tree_data_GSMNP <- do.call(rbind, tree_data_GSMNP)
tree_data_GSMNP$ownership <- "GSMNP"

#Combine both datasets

tree_data <- rbind(tree_data_GSMNP, tree_data_Cherokee)

tree_data_matlab <- tree_data %>% 
  filter(ownership == "GSMNP") 
tree_data_matlab <- tree_data_matlab[c("MEASYEAR", "cond_carbon")]
tree_data_matlab <- unique(tree_data_matlab)
temp <- sort(sample(nrow(tree_data_matlab), nrow(tree_data_matlab) * 0.1))
tree_data_matlab <- tree_data_matlab[temp,]

tree_data_matlab <- tree_data_matlab[order(tree_data_matlab$MEASYEAR), ]
clipr::write_last_clip()
# "STATECD", "COUNTYCD", "PLOT", "CONDID", "CYCLE"


tree_data_matlab <- unique(tree_data[c("STATECD", "COUNTYCD", "PLOT", "CONDID", "CYCLE")])

tree_data_matlab <- tree_data_matlab[sample(nrow(tree_data_matlab), size = nrow(tree_data_matlab), replace = F), ]
# NEW METHODOLOGY - WIP ####
tree_data <- tree_data[which(tree_data$OWNCD == 11| tree_data$OWNCD == 21), ]

tree_data <- tree_data %>%
  mutate(ownership = case_when(
    ADFORCD == "802" ~ "Daniel Boone",
    ADFORCD == "803" ~ "Chattahoochee-Oconee",
    ADFORCD == "804" ~ "Cherokee",
    ADFORCD == "808" ~ "George Washington-Jefferson",
    #ADFORCD == "811" ~ "NFS in North Carolina",
    ADFORCD == "814" ~ "George Washington-Jefferson",
    ADFORCD == "914" ~ "Wayne",
    ADFORCD == "918" ~ "Wayne",
    ADFORCD == "921" ~ "Monongahela",
    STATECD == 37 & COUNTYCD %in% c(11, 21, 23, 27, 87, 89, 111, 115, 121, 175, 199) ~ "Pisgah",
    STATECD == 37 & COUNTYCD %in% c(39, 43, 75, 99, 113, 173) ~ "Nantahala", 
    TRUE ~ NA_character_ # For observations that don't match any park
  ))

tree_data <- tree_data %>%
  mutate(ownership = case_when(
    STATECD == 51 & COUNTYCD %in% c(187, 139, 165, 15, 157, 113, 79, 3) ~ "Shenandoah",
    STATECD == 47 & COUNTYCD %in% c(155, 9, 29) ~ "Great Smoky Mountains",
    STATECD == 37 & COUNTYCD %in% c(173, 87) ~ "Great Smoky Mountains",
    #STATECD == 21 & COUNTYCD %in% c(61, 99, 9) ~ "Mammoth Cave",
    STATECD == 54 & COUNTYCD %in% c(19, 89) ~ "New River Gorge",
    TRUE ~ NA_character_ # For observations that don't match any park
  ))

#write_xlsx(tree_data, "combined.xlsx")

# temporal changes over time ####
  # Cherokee
tree_data_Cherokee_2 <- tree_data_Cherokee

tree_data_Cherokee_2$forest_type_group <- ifelse(tree_data_Cherokee_2$FORTYPCD >= 100 & tree_data_Cherokee_2$FORTYPCD < 120,
   "White/red/jack pine group",
  ifelse(tree_data_Cherokee_2$FORTYPCD >= 120 & tree_data_Cherokee_2$FORTYPCD < 140,
    "Spruce/fir group",
  ifelse(tree_data_Cherokee_2$FORTYPCD >= 160 & tree_data_Cherokee_2$FORTYPCD < 170,
     "Loblolly/shortleaf pine group",
  ifelse(tree_data_Cherokee_2$FORTYPCD >= 400 & tree_data_Cherokee_2$FORTYPCD < 500,
    "Oak/pine group",
  ifelse(tree_data_Cherokee_2$FORTYPCD >= 500 & tree_data_Cherokee_2$FORTYPCD < 600,
    "Oak/hickory group",
  ifelse(tree_data_Cherokee_2$FORTYPCD >= 800 & tree_data_Cherokee_2$FORTYPCD < 900,
    "Maple/beech/birch group",
  ifelse(tree_data_Cherokee_2$FORTYPCD >= 960 & tree_data_Cherokee_2$FORTYPCD < 970,
    "Other hardwoods group",
  ifelse(tree_data_Cherokee_2$FORTYPCD >= 990 & tree_data_Cherokee_2$FORTYPCD < 999,
    "Exotic hardwoods group",
    "Nonstocked"))))))))


tree_data_Cherokee_summary <- tree_data_Cherokee_2 %>%
  distinct(STATECD, COUNTYCD, PLOT, CONDID, CYCLE, cpa, .keep_all = TRUE) %>%
  filter(forest_type_group != "Nonstocked") %>% 
  group_by(STATECD, COUNTYCD, PLOT, CONDID, CYCLE, forest_type_group) %>% 
  summarise_at(.vars = "cpa",
               .funs = ~sum(.)) %>% 
  dplyr::group_by(CYCLE, forest_type_group) %>%
  dplyr::summarise_at(.vars = c("cpa"), 
                      .funs = c("low"=~quantile(., probs = 0.025),
                                "med"=~quantile(., probs = 0.5),
                                "up"=~quantile(., probs = 0.975))) 

clipr::write_clip(tree_data_Cherokee_summary)

png(filename = paste0("CPA_changes_over_time/Cherokee.png",sep =""),
    width = 7,
    height = 4,
    units = "in",
    res = 500)
r <- ggplot(tree_data_Cherokee_summary, aes(CYCLE, med, group = forest_type_group)) + geom_point(aes(color=forest_type_group)) + geom_line(aes(color=forest_type_group)) +  
  xlab("Cycle") + ylab("Carbon per unit area (tons/ha)") + #ggtitle("Change in Mean Carbon/Acre over time in Cherokee National Forest") +
   theme_bw() + theme(axis.line = element_line(color='black'),
                            plot.background = element_blank(),
                            panel.grid.minor = element_blank(),
                            panel.border = element_blank(),
                            panel.grid.major.x = element_blank(),
                            panel.grid.major.y = element_line(linewidth =.1, color="gray")) +
  labs(color = 'Forest Type Major Group')
print(r)
dev.off()

  # GSMNP
tree_data_GSMNP_2 <- tree_data_GSMNP

tree_data_GSMNP_2$forest_type_group <- ifelse(tree_data_GSMNP_2$FORTYPCD >= 100 & tree_data_GSMNP_2$FORTYPCD < 120,
                                                 "White/red/jack pine group",
                                                 ifelse(tree_data_GSMNP_2$FORTYPCD >= 120 & tree_data_GSMNP_2$FORTYPCD < 140,
                                                        "Spruce/fir group",
                                                        ifelse(tree_data_GSMNP_2$FORTYPCD >= 160 & tree_data_GSMNP_2$FORTYPCD < 170,
                                                               "Loblolly/shortleaf pine group",
                                                               ifelse(tree_data_GSMNP_2$FORTYPCD >= 400 & tree_data_GSMNP_2$FORTYPCD < 500,
                                                                      "Oak/pine group",
                                                                      ifelse(tree_data_GSMNP_2$FORTYPCD >= 500 & tree_data_GSMNP_2$FORTYPCD < 600,
                                                                             "Oak/hickory group",
                                                                             ifelse(tree_data_Cherokee_2$FORTYPCD >= 600 & tree_data_Cherokee_2$FORTYPCD < 700,
                                                                                    "Oak/gum/cypress group",
                                                                                    ifelse(tree_data_Cherokee_2$FORTYPCD >= 700 & tree_data_Cherokee_2$FORTYPCD < 800,
                                                                                           "Elm/ash/cottonwood group",
                                                                             ifelse(tree_data_GSMNP_2$FORTYPCD >= 800 & tree_data_GSMNP_2$FORTYPCD < 900,
                                                                                    "Maple/beech/birch group",
                                                                                    ifelse(tree_data_GSMNP_2$FORTYPCD >= 960 & tree_data_GSMNP_2$FORTYPCD < 970,
                                                                                           "Other hardwoods group",
                                                                                           ifelse(tree_data_GSMNP_2$FORTYPCD >= 990 & tree_data_GSMNP_2$FORTYPCD < 999,
                                                                                                  "Exotic hardwoods group",
                                                                                                  "Nonstocked"))))))))))


tree_data_GSMNP_summary <- tree_data_GSMNP_2 %>%
  distinct(STATECD, COUNTYCD, PLOT, CONDID, CYCLE, .keep_all = TRUE) %>%
  filter(forest_type_group != "Nonstocked") %>% 
  group_by(STATECD, COUNTYCD, PLOT, CONDID, CYCLE, forest_type_group) %>% 
  summarise_at(.vars = "cpa",
               .funs = ~sum(.)) %>% 
  dplyr::group_by(CYCLE, forest_type_group) %>%
  dplyr::summarise_at(.vars = c("cpa"), 
                      .funs = c("low"=~quantile(., probs = 0.025),
                                "med"=~quantile(., probs = 0.5),
                                "up"=~quantile(., probs = 0.975))) 

clipr::write_clip(tree_data_GSMNP_summary)

png(filename = paste0("CPA_changes_over_time/GSMNP.png",sep =""),
    width = 7,
    height = 4,
    units = "in",
    res = 500)
r <- ggplot(tree_data_GSMNP_summary, aes(CYCLE, med, group = forest_type_group)) + geom_point(aes(color=forest_type_group)) + geom_line(aes(color=forest_type_group)) +  
  xlab("Cycle") + ylab("Carbon per unit area (tons/ha)") + #ggtitle("Change in Mean Carbon/Acre over time in GSMNP National Forest") + 
  theme_bw() + theme(axis.line = element_line(color='black'),
                            plot.background = element_blank(),
                            panel.grid.minor = element_blank(),
                            panel.border = element_blank(),
                            panel.grid.major.x = element_blank(),
                            panel.grid.major.y = element_line(linewidth =.1, color="gray")) +
  labs(color = 'Forest Type Major Group')
print(r)
dev.off()


# testing cpa calculation ####
test_cpa_calc <- tree_data_GSMNP_2
test_cpa_calc$unique_observ_id <- paste0(test_cpa_calc$STATECD,"_",test_cpa_calc$COUNTYCD,"_",test_cpa_calc$PLOT,"_",test_cpa_calc$CONDID,"_",test_cpa_calc$CYCLE)
test_cpa_calc <- test_cpa_calc %>% 
  group_by(unique_observ_id) %>% 
  mutate(C = sum(cpa*tph)/1000) %>% 
  summarise_at(.vars = "C",
               .funs = ~sum(.), .keep_all = TRUE)