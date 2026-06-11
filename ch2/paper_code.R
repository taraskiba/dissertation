# Modified for forest types, instead of forest type groups
library(ggplot2)
library(dplyr)
library(readxl)
library(reshape2)
library(data.table)

setwd("C:/Users/tara/Desktop/carbon_project_1/dissertation/ch2") 

#paper_data <- read.csv("tree_plot_data_revision.csv") %>% 
#  filter(ownership != "New River Gorge")
paper_data <- final_plot_data
forest_type_code <- read.csv("forest_type_codes.csv") %>% 
  select(-"Group.Code") %>% 
  rename(fortypcd = Code)

park_forest_names <- unique(paper_data$ownership)

# HISTOGRAMS ####
# setwd("C:/Users/tara/Desktop/co-author paper/histograms")
# for(i in 1:length(park_forest_names)){
#   paper_data_subset <- paper_data %>% 
#     filter(ownership == park_forest_names[i])
#   
#   png(filename = paste0(park_forest_names[i], "_histogram.png", sep = ""),
#       width = 6,
#       height = 6,
#       units = "in",
#       res = 300)
#   title <- expression(paste("Total carbon per ha (Mg C/ha)"))
#   k <- ggplot(paper_data_subset, aes(cpa)) + geom_histogram(bins = 30, color="gray", fill="gray") + 
#     scale_color_grey() + theme_bw() + labs(x=title, y = "Number of Observations") + theme(axis.line = element_line(colour = "black", 
#                                                                                                     linetype = "solid"),
#                                                                          axis.text=element_text(size=14),
#                                                                          axis.title=element_text(size=16))
#   print(k)
#   dev.off()
#   
# }


## Count by national park/forest and forest type group ####
observ_count_group <- paper_data
observ_count_group$fortypcd_group <- ifelse(observ_count_group$fortypcd >= 100 & observ_count_group$fortypcd < 120, "White/red/jack pine",
                                            ifelse(observ_count_group$fortypcd >= 120 & observ_count_group$fortypcd < 140, "Spruce/fir",
                                                   ifelse(observ_count_group$fortypcd >= 140 & observ_count_group$fortypcd < 150, "Longleaf/slash pine",
                                                          ifelse(observ_count_group$fortypcd >= 160 & observ_count_group$fortypcd < 170, "Loblolly/shortleaf pine",
                                                                 ifelse(observ_count_group$fortypcd >= 170 & observ_count_group$fortypcd < 180, "Pinyon/juniper",
                                                                        ifelse(observ_count_group$fortypcd >= 180 & observ_count_group$fortypcd < 200, "Douglas-fir",
                                                                               ifelse(observ_count_group$fortypcd >= 220 & observ_count_group$fortypcd < 240, "Ponderosa pine",
                                                                                      ifelse(observ_count_group$fortypcd >= 400 & observ_count_group$fortypcd < 500, "Oak/pine",
                                                                                             ifelse(observ_count_group$fortypcd >= 500 & observ_count_group$fortypcd < 600, "Oak/hickory",
                                                                                                    ifelse(observ_count_group$fortypcd >= 600 & observ_count_group$fortypcd < 700, "Oak/gum/cypress group",
                                                                                                           ifelse(observ_count_group$fortypcd >= 700 & observ_count_group$fortypcd < 800, "Elm/ash/cottonwood",
                                                                                                                  ifelse(observ_count_group$fortypcd >= 800 & observ_count_group$fortypcd < 900, "Maple/beech/birch",
                                                                                                                         ifelse(observ_count_group$fortypcd >= 900 & observ_count_group$fortypcd < 910, "Aspen/birch",
                                                                                                                                ifelse(observ_count_group$fortypcd >= 910 & observ_count_group$fortypcd < 920, "Alder/maple",
                                                                                                                                       ifelse(observ_count_group$fortypcd >= 960 & observ_count_group$fortypcd < 970, "Other hardwoods",
                                                                                                                                              ifelse(observ_count_group$fortypcd >= 990 & observ_count_group$fortypcd < 999, "Exotic hardwoods",
                                                                                                                                                     ifelse(observ_count_group$fortypcd == 999, "Nonstocked", "check")))))))))))))))))
cols <- c("ownership", "fortypcd_group", "fortypcd")
observ_count_group_sub <- observ_count_group[, cols] 
observ_count_group_sub <- observ_count_group_sub %>% 
  distinct() %>% 
  count(ownership, fortypcd_group)
observ_count_group_sub <- reshape2::dcast(observ_count_group_sub, fortypcd_group~ownership)
observ_count_group_sub <- observ_count_group_sub[,c(1,2,3,8,9,5,4,7,11,6,10)]
observ_count_group_sub <- data.frame(t(observ_count_group_sub))
observ_count_group_sub[is.na(observ_count_group_sub)] <- 0
clipr::write_clip(observ_count_group_sub)

observ_count_group <- observ_count_group %>% 
  count(ownership, fortypcd_group)
observ_count_group <- dcast(observ_count_group, fortypcd_group~ownership)
observ_count_group[is.na(observ_count_group)] <- 0
observ_count_group <- observ_count_group %>% 
  rename("CONF" = "Chattahoochee-Oconee",
         "CNF" = "Cherokee",
         "DBNF" = "Daniel Boone",
         "GWJNF" = "George Washington-Jefferson",
         "GSMNP" = "Great Smoky Mountains",
         "MNF" = "Monongahela",
         "NNF" = "Nantahala",
         "PNF" = "Pisgah",
         "SNP" = "Shenandoah",
         "WNF" = "Wayne")
observ_count_group <- observ_count_group[,c(1,2,3,8,9,5,4,7,11,6,10)]
observ_count_group <- data.frame(t(observ_count_group))
clipr::write_clip(observ_count_group)


## Count by national park/forest and forest type ####
observ_count_group <- paper_data
observ_count_group$fortypcd_group <- ifelse(observ_count_group$fortypcd >= 100 & observ_count_group$fortypcd < 120, "White/red/jack pine",
                                            ifelse(observ_count_group$fortypcd >= 120 & observ_count_group$fortypcd < 140, "Spruce/fir",
                                                   ifelse(observ_count_group$fortypcd >= 140 & observ_count_group$fortypcd < 150, "Longleaf/slash pine",
                                                          ifelse(observ_count_group$fortypcd >= 160 & observ_count_group$fortypcd < 170, "Loblolly/shortleaf pine",
                                                                 ifelse(observ_count_group$fortypcd >= 170 & observ_count_group$fortypcd < 180, "Pinyon/juniper",
                                                                        ifelse(observ_count_group$fortypcd >= 180 & observ_count_group$fortypcd < 200, "Douglas-fir",
                                                                               ifelse(observ_count_group$fortypcd >= 220 & observ_count_group$fortypcd < 240, "Ponderosa pine",
                                                                                      ifelse(observ_count_group$fortypcd >= 400 & observ_count_group$fortypcd < 500, "Oak/pine",
                                                                                             ifelse(observ_count_group$fortypcd >= 500 & observ_count_group$fortypcd < 600, "Oak/hickory",
                                                                                                    ifelse(observ_count_group$fortypcd >= 600 & observ_count_group$fortypcd < 700, "Oak/gum/cypress group",
                                                                                                           ifelse(observ_count_group$fortypcd >= 700 & observ_count_group$fortypcd < 800, "Elm/ash/cottonwood",
                                                                                                                  ifelse(observ_count_group$fortypcd >= 800 & observ_count_group$fortypcd < 900, "Maple/beech/birch",
                                                                                                                         ifelse(observ_count_group$fortypcd >= 900 & observ_count_group$fortypcd < 910, "Aspen/birch",
                                                                                                                                ifelse(observ_count_group$fortypcd >= 910 & observ_count_group$fortypcd < 920, "Alder/maple",
                                                                                                                                      ifelse(observ_count_group$fortypcd >= 960 & observ_count_group$fortypcd < 970, "Other hardwoods",
                                                                                                                                             ifelse(observ_count_group$fortypcd >= 990 & observ_count_group$fortypcd < 999, "Exotic hardwoods",
                                                                                                                                                    ifelse(observ_count_group$fortypcd == 999, "Nonstocked", "check")))))))))))))))))
cols <- c("ownership", "fortypcd")


observ_count_group_sub <- observ_count_group[, cols] 
observ_count_group_sub <- observ_count_group_sub %>% 
  count(ownership, fortypcd)
observ_count_group_sub <- reshape2::dcast(observ_count_group_sub, fortypcd~ownership)
observ_count_group_sub <- observ_count_group_sub[,c(1,2,3,8,9,5,4,7,11,6,10)]
observ_count_group_sub <- left_join(observ_count_group_sub, forest_type_code, by = "fortypcd")
observ_count_group_sub <- `rownames<-`(observ_count_group_sub, observ_count_group_sub$FIA.Forest.Type)
observ_count_group_sub <- observ_count_group_sub[, -c(1,12)]
observ_count_group_sub <- data.frame(t(observ_count_group_sub))
observ_count_group_sub[is.na(observ_count_group_sub)] <- 0
colnames(observ_count_group_sub) <- gsub("...", " / ", colnames(observ_count_group_sub), fixed = TRUE)
colnames(observ_count_group_sub) <- gsub(".", " ", colnames(observ_count_group_sub), fixed = TRUE)
observ_count_group_sub
clipr::write_clip(observ_count_group_sub)

observ_count_group_sub[is.na(observ_count_group_sub)] <- 0

observ_count_group <- observ_count_group %>% 
  count(ownership, fortypcd)
observ_count_group <- dcast(observ_count_group, fortypcd~ownership)
observ_count_group[is.na(observ_count_group)] <- 0
observ_count_group <- observ_count_group %>% 
  rename("CONF" = "Chattahoochee-Oconee",
         "CNF" = "Cherokee",
         "DBNF" = "Daniel Boone",
         "GWJNF" = "George Washington-Jefferson",
         "GSMNP" = "Great Smoky Mountains",
         "MNF" = "Monongahela",
         "NNF" = "Nantahala",
         "PNF" = "Pisgah",
         "SNP" = "Shenandoah",
         "WNF" = "Wayne")
observ_count_group <- observ_count_group[,c(1,2,3,8,9,5,4,7,11,6,10)]
observ_count_group <- data.frame(t(observ_count_group))
clipr::write_clip(observ_count_group)
## Count by national park/forest and forest type with names ####

observ_count <- paper_data %>% 
  count(ownership, fortypcd)
observ_count <- dcast(observ_count, fortypcd~ownership)
observ_count[is.na(observ_count)] <- 0
observ_count$forest_type <- ifelse(observ_count$fortypcd == 103, "EWP",
                                ifelse(observ_count$fortypcd == 104, "EWP/EH",
                                ifelse(observ_count$fortypcd == 105, "EH",
                                       ifelse(observ_count$fortypcd == 161, "LBP",
                                              ifelse(observ_count$fortypcd == 162, "SLP",
                                                     ifelse(observ_count$fortypcd == 163, "VP",
                                                            ifelse(observ_count$fortypcd == 165, "TMP",
                                                                   ifelse(observ_count$fortypcd == 167, "PP",
                                                                          ifelse(observ_count$fortypcd == 401, "EWP/NRO/WA",
                                                                                 ifelse(observ_count$fortypcd == 404, "SLP/O",
                                                                                        ifelse(observ_count$fortypcd == 405, "VP/SRO",
                                                                                               ifelse(observ_count$fortypcd == 406, "LBP/HW",
                                                                                                      ifelse(observ_count$fortypcd == 409, "OP/HW",
                                                                                                             ifelse(observ_count$fortypcd == 501, "PO/BO",
                                                                                                                    ifelse(observ_count$fortypcd == 502, "CO",
                                                                                                                           ifelse(observ_count$fortypcd == 503, "WO/RO/H",
                                                                                                                                  ifelse(observ_count$fortypcd == 504, "WO",
                                                                                                                                         ifelse(observ_count$fortypcd == 505, "NRO",
                                                                                                                                                ifelse(observ_count$fortypcd == 506, "YP/WO/NRO",
                                                                                                                                                       ifelse(observ_count$fortypcd == 508, "SG/YP",
                                                                                                                                                              ifelse(observ_count$fortypcd == 510, "SO",
                                                                                                                                                                     ifelse(observ_count$fortypcd == 511, "YP",
                                                                                                                                                                            ifelse(observ_count$fortypcd == 513, "BL",
                                                                                                                                                                                   ifelse(observ_count$fortypcd == 515, "CO/BO/SO",
                                                                                                                                                                                          ifelse(observ_count$fortypcd == 516, "C/WA/YP",
                                                                                                                                                                                                 ifelse(observ_count$fortypcd == 517, "E/A/BL",
                                                                                                                                                                                                        ifelse(observ_count$fortypcd == 519, "RM/O",
                                                                                                                                                                                                 ifelse(observ_count$fortypcd == 520, "MUH",
                                                                                                                                                                                                        ifelse(observ_count$fortypcd == 702, "RB/S",
                                                                                                                                                                                                               ifelse(observ_count$fortypcd == 708, "RM/LL",
                                                                                                                                                                                                                      ifelse(observ_count$fortypcd == 801, "SM/B/YB",
                                                                                                                                                                                                                             ifelse(observ_count$fortypcd == 962, "OHW",
                                                                                                                                                                                                                                    ifelse(observ_count$fortypcd == 123, "RS",
                                                                                                                                                                                                                                           ifelse(observ_count$fortypcd == 507, "S/P",
                                                                                                                                                                                                                                                  ifelse(observ_count$fortypcd == 805, "HM/B",
                                                                                                                                                                                                                                                         ifelse(observ_count$fortypcd == 809, "RM/U",
                                                                                                                                                                                                                                                                ifelse(observ_count$fortypcd == 512, "BW",
                                                                                                                                                                                                                                                                       ifelse(observ_count$fortypcd == 608, "S/ST/RM",
                                                                                                                                                                                                                                                                              ifelse(observ_count$fortypcd == 802, "BC",
                                                                                                                                                                                                                                                                                     ifelse(observ_count$fortypcd == 991, "PLW",
                                                                                                                                                                                                                                                                                            ifelse(observ_count$fortypcd == 905, "PC",
                                                                                                                                                                                                                                                                                                   ifelse(observ_count$fortypcd == 514, "SSO",
                                                                                                                                                                                                                                                                                                          ifelse(observ_count$fortypcd == 706, "S/H/E/GA",
                                                                                                                                                                                                                                                                                                                 ifelse(observ_count$fortypcd == 705, "S/P/AE",
                                                                                                                                                                                                                                                                                                                        ifelse(observ_count$fortypcd == 901, "A", "check"
                                                                                                                                                                                                                                                                                                                               )))))))))))))))))))))))))))))))))))))))))))))

observ_count <- observ_count %>% 
  rename("CONF" = "Chattahoochee-Oconee",
         "CNF" = "Cherokee",
         "DBNF" = "Daniel Boone",
         "GWJNF" = "George Washington-Jefferson",
         "GSMNP" = "Great Smoky Mountains",
         "MNF" = "Monongahela",
         "NNF" = "Nantahala",
         "PNF" = "Pisgah",
         "SNP" = "Shenandoah",
         "WNF" = "Wayne")
observ_count <- observ_count[,c(1,12,2,3,8,9,5,4,7,11,6,10)]
clipr::write_clip(
  
)
paper_data <- read.csv("tree_plot_data_revision.csv") %>% 
  filter(ownership != "New River Gorge")

park_forest_names <- unique(paper_data$ownership)



## Count by national park/forest and forest type group ####
observ_count_group <- paper_data
observ_count_group$fortypcd_group <- ifelse(observ_count_group$fortypcd >= 100 & observ_count_group$fortypcd < 120, "WP/RP/JP",
                                            ifelse(observ_count_group$fortypcd >= 120 & observ_count_group$fortypcd < 140, "Spr/Fir",
                                                   ifelse(observ_count_group$fortypcd >= 140 & observ_count_group$fortypcd < 150, "Lon/Sla Pine",
                                                          ifelse(observ_count_group$fortypcd >= 160 & observ_count_group$fortypcd < 170, "Lob/SLP",
                                                                 ifelse(observ_count_group$fortypcd >= 170 & observ_count_group$fortypcd < 180, "Pin/Juni",
                                                                        ifelse(observ_count_group$fortypcd >= 180 & observ_count_group$fortypcd < 200, "Doug-fir",
                                                                               ifelse(observ_count_group$fortypcd >= 220 & observ_count_group$fortypcd < 240, "Pond Pine",
                                                                                      ifelse(observ_count_group$fortypcd >= 400 & observ_count_group$fortypcd < 500, "Oak/Pine",
                                                                                             ifelse(observ_count_group$fortypcd >= 500 & observ_count_group$fortypcd < 600, "Oak/Hick",
                                                                                                    ifelse(observ_count_group$fortypcd >= 600 & observ_count_group$fortypcd < 700, "Oak/Gum/Cyp ",
                                                                                                           ifelse(observ_count_group$fortypcd >= 700 & observ_count_group$fortypcd < 800, "Elm/Ash/Cot",
                                                                                                                  ifelse(observ_count_group$fortypcd >= 800 & observ_count_group$fortypcd < 900, "Map/Bch/Bir",
                                                                                                                         ifelse(observ_count_group$fortypcd >= 900 & observ_count_group$fortypcd < 910, "Asp/Bir",
                                                                                                                                ifelse(observ_count_group$fortypcd >= 910 & observ_count_group$fortypcd < 920, "Ald/Map",
                                                                                                                                       ifelse(observ_count_group$fortypcd >= 960 & observ_count_group$fortypcd < 970, "Other HW",
                                                                                                                                              ifelse(observ_count_group$fortypcd >= 990 & observ_count_group$fortypcd < 999, "Exotic HW",
                                                                                                                                                     ifelse(observ_count_group$fortypcd == 999, "Nonstocked", "check")))))))))))))))))
cols <- c("ownership", "fortypcd_group", "fortypcd")
observ_count_group_sub <- observ_count_group[, cols] 
observ_count_group_sub <- observ_count_group_sub %>% 
  distinct() %>% 
  count(ownership, fortypcd_group)
observ_count_group_sub <- reshape2::dcast(observ_count_group_sub, fortypcd_group~ownership)
observ_count_group_sub <- observ_count_group_sub[,c(1,2,3,8,9,5,4,7,11,6,10)]
observ_count_group_sub <- data.frame(t(observ_count_group_sub))
observ_count_group_sub[is.na(observ_count_group_sub)] <- 0
clipr::write_clip(observ_count_group_sub)

# Distribution of forest type
library(ggplot2)
library(paletteer)
library(patchwork)
library(ggpubr)

ownership_index <- c("Chattahoochee-Oconee", "Cherokee", "Nantahala", "Pisgah","George Washington-Jefferson", "Daniel Boone","Monongahela","Wayne","Great Smoky Mountains","Shenandoah")

observ_count_group <- paper_data
observ_count_group$fortypcd_group <- ifelse(observ_count_group$fortypcd >= 100 & observ_count_group$fortypcd < 120, "White/red/jack pine",
                                            ifelse(observ_count_group$fortypcd >= 120 & observ_count_group$fortypcd < 140, "Spruce/fir",
                                                   ifelse(observ_count_group$fortypcd >= 140 & observ_count_group$fortypcd < 150, "Longleaf/slash pine",
                                                          ifelse(observ_count_group$fortypcd >= 160 & observ_count_group$fortypcd < 170, "Loblolly/shortleaf pine",
                                                                 ifelse(observ_count_group$fortypcd >= 170 & observ_count_group$fortypcd < 180, "Pinyon/juniper",
                                                                        ifelse(observ_count_group$fortypcd >= 180 & observ_count_group$fortypcd < 200, "Douglas-fir",
                                                                               ifelse(observ_count_group$fortypcd >= 220 & observ_count_group$fortypcd < 240, "Ponderosa pine",
                                                                                      ifelse(observ_count_group$fortypcd >= 400 & observ_count_group$fortypcd < 500, "Oak/pine",
                                                                                             ifelse(observ_count_group$fortypcd >= 500 & observ_count_group$fortypcd < 600, "Oak/hickory",
                                                                                                    ifelse(observ_count_group$fortypcd >= 600 & observ_count_group$fortypcd < 700, "Oak/gum/cypress group",
                                                                                                           ifelse(observ_count_group$fortypcd >= 700 & observ_count_group$fortypcd < 800, "Elm/ash/cottonwood",
                                                                                                                  ifelse(observ_count_group$fortypcd >= 800 & observ_count_group$fortypcd < 900, "Maple/beech/birch",
                                                                                                                         ifelse(observ_count_group$fortypcd >= 900 & observ_count_group$fortypcd < 910, "Aspen/birch",
                                                                                                                                ifelse(observ_count_group$fortypcd >= 910 & observ_count_group$fortypcd < 920, "Alder/maple",
                                                                                                                                       ifelse(observ_count_group$fortypcd >= 960 & observ_count_group$fortypcd < 970, "Other hardwoods",
                                                                                                                                              ifelse(observ_count_group$fortypcd >= 990 & observ_count_group$fortypcd < 999, "Exotic hardwoods",
                                                                                                                                                     ifelse(observ_count_group$fortypcd == 999, "Nonstocked", "check")))))))))))))))))
cols <- c("ownership", "fortypcd_group", "fortypcd")
observ_count_group_sub <- observ_count_group[, cols] 
observ_count_group_sub <- observ_count_group_sub

plots <- ggplot()

for(i in 1:length(ownership_index)){
  observ_count_group_sub_sub <- observ_count_group_sub[which(observ_count_group_sub$ownership == ownership_index[i]),c(2,3)]
  p1 <- ggplot(observ_count_group_sub_sub, aes(x=fortypcd_group, fill = fortypcd_group)) + 
  geom_bar() + scale_fill_paletteer_d("MoMAColors::Avedon") + theme_bw() + theme(axis.text.x=element_blank()) + labs(fill = "Forest Type Group") + xlab("Region") 
  plots <- plots+p1
}

plots

# scale color def
# scale_fill_manual(name = 'Forest Type Group', 
#                    values = c("Oak/pine" = '#FF7200FF',  "Loblolly/shortleaf pine" = '#FF8827FF', "Oak/hickory" = '#FF9C4CFF', "White/red/jack pine" = '#FFB274FF', "Elm/ash/cottonwood" = '#F1CAA8FF', "Maple/beech/birch"  = '#E3E1DCFF',
#                               "Other hardwoods" = '#C2CEAAFF', "Spruce/fir" = '#A1BA77FF', "Aspen/birch" = '#8BAC54FF', "Oak/gum/cypress group" = '#7EA13EFF', "Exotic hardwoods" = '#648C16FF'),
#                    drop = FALSE)

i=1
observ_count_group_sub_sub <- observ_count_group_sub[which(observ_count_group_sub$ownership == ownership_index[i]),c(2,3)]
p1 <- ggplot(observ_count_group_sub_sub, aes(x=fortypcd_group, fill = fortypcd_group)) + 
  geom_bar() + scale_fill_manual(name = 'Forest Type Group', 
                                  values = c("Oak/pine" = '#FF7200FF',  "Loblolly/shortleaf pine" = '#FF8827FF', "Oak/hickory" = '#FF9C4CFF', "White/red/jack pine" = '#FFB274FF', "Elm/ash/cottonwood" = '#F1CAA8FF', "Maple/beech/birch"  = '#E3E1DCFF',
                                             "Other hardwoods" = '#C2CEAAFF', "Spruce/fir" = '#A1BA77FF', "Aspen/birch" = '#8BAC54FF', "Oak/gum/cypress group" = '#7EA13EFF', "Exotic hardwoods" = '#648C16FF'),
                                  drop = FALSE) + theme_bw() + theme(axis.text.x=element_blank()) + labs(title = ownership_index[i])
i=2
observ_count_group_sub_sub <- observ_count_group_sub[which(observ_count_group_sub$ownership == ownership_index[i]),c(2,3)]
p2 <- ggplot(observ_count_group_sub_sub, aes(x=fortypcd_group, fill = fortypcd_group)) + 
  geom_bar() + scale_fill_manual(name = 'Forest Type Group', 
                                  values = c("Oak/pine" = '#FF7200FF',  "Loblolly/shortleaf pine" = '#FF8827FF', "Oak/hickory" = '#FF9C4CFF', "White/red/jack pine" = '#FFB274FF', "Elm/ash/cottonwood" = '#F1CAA8FF', "Maple/beech/birch"  = '#E3E1DCFF',
                                             "Other hardwoods" = '#C2CEAAFF', "Spruce/fir" = '#A1BA77FF', "Aspen/birch" = '#8BAC54FF', "Oak/gum/cypress group" = '#7EA13EFF', "Exotic hardwoods" = '#648C16FF'),
                                  drop = FALSE) + theme_bw() + theme(axis.text.x=element_blank()) + labs(title = ownership_index[i])
i=3
observ_count_group_sub_sub <- observ_count_group_sub[which(observ_count_group_sub$ownership == ownership_index[i]),c(2,3)]
p3 <- ggplot(observ_count_group_sub_sub, aes(x=fortypcd_group, fill = fortypcd_group)) + 
  geom_bar() + scale_fill_manual(name = 'Forest Type Group', 
                                  values = c("Oak/pine" = '#FF7200FF',  "Loblolly/shortleaf pine" = '#FF8827FF', "Oak/hickory" = '#FF9C4CFF', "White/red/jack pine" = '#FFB274FF', "Elm/ash/cottonwood" = '#F1CAA8FF', "Maple/beech/birch"  = '#E3E1DCFF',
                                             "Other hardwoods" = '#C2CEAAFF', "Spruce/fir" = '#A1BA77FF', "Aspen/birch" = '#8BAC54FF', "Oak/gum/cypress group" = '#7EA13EFF', "Exotic hardwoods" = '#648C16FF'),
                                  drop = FALSE) + theme_bw() + theme(axis.text.x=element_blank()) + labs(title = ownership_index[i])
i=4
observ_count_group_sub_sub <- observ_count_group_sub[which(observ_count_group_sub$ownership == ownership_index[i]),c(2,3)]
p4 <- ggplot(observ_count_group_sub_sub, aes(x=fortypcd_group, fill = fortypcd_group)) + 
  geom_bar() + scale_fill_manual(name = 'Forest Type Group', 
                                  values = c("Oak/pine" = '#FF7200FF',  "Loblolly/shortleaf pine" = '#FF8827FF', "Oak/hickory" = '#FF9C4CFF', "White/red/jack pine" = '#FFB274FF', "Elm/ash/cottonwood" = '#F1CAA8FF', "Maple/beech/birch"  = '#E3E1DCFF',
                                             "Other hardwoods" = '#C2CEAAFF', "Spruce/fir" = '#A1BA77FF', "Aspen/birch" = '#8BAC54FF', "Oak/gum/cypress group" = '#7EA13EFF', "Exotic hardwoods" = '#648C16FF'),
                                  drop = FALSE) + theme_bw()  + theme(axis.text.x=element_blank()) + labs(title = ownership_index[i])
i=5
observ_count_group_sub_sub <- observ_count_group_sub[which(observ_count_group_sub$ownership == ownership_index[i]),c(2,3)]
p5 <- ggplot(observ_count_group_sub_sub, aes(x=fortypcd_group, fill = fortypcd_group)) + 
  geom_bar() + scale_fill_manual(name = 'Forest Type Group', 
                                  values = c("Oak/pine" = '#FF7200FF',  "Loblolly/shortleaf pine" = '#FF8827FF', "Oak/hickory" = '#FF9C4CFF', "White/red/jack pine" = '#FFB274FF', "Elm/ash/cottonwood" = '#F1CAA8FF', "Maple/beech/birch"  = '#E3E1DCFF',
                                             "Other hardwoods" = '#C2CEAAFF', "Spruce/fir" = '#A1BA77FF', "Aspen/birch" = '#8BAC54FF', "Oak/gum/cypress group" = '#7EA13EFF', "Exotic hardwoods" = '#648C16FF'),
                                  drop = FALSE) + theme_bw() + theme(axis.text.x = element_blank()) + labs(title = ownership_index[i])
i=6
observ_count_group_sub_sub <- observ_count_group_sub[which(observ_count_group_sub$ownership == ownership_index[i]),c(2,3)]
p6 <- ggplot(observ_count_group_sub_sub, aes(x=fortypcd_group, fill = fortypcd_group)) + 
  geom_bar() + scale_fill_manual(name = 'Forest Type Group', 
                                  values = c("Oak/pine" = '#FF7200FF',  "Loblolly/shortleaf pine" = '#FF8827FF', "Oak/hickory" = '#FF9C4CFF', "White/red/jack pine" = '#FFB274FF', "Elm/ash/cottonwood" = '#F1CAA8FF', "Maple/beech/birch"  = '#E3E1DCFF',
                                             "Other hardwoods" = '#C2CEAAFF', "Spruce/fir" = '#A1BA77FF', "Aspen/birch" = '#8BAC54FF', "Oak/gum/cypress group" = '#7EA13EFF', "Exotic hardwoods" = '#648C16FF'),
                                  drop = FALSE) + theme_bw() + theme(axis.text.x = element_blank()) + labs(title = ownership_index[i])
i=7
observ_count_group_sub_sub <- observ_count_group_sub[which(observ_count_group_sub$ownership == ownership_index[i]),c(2,3)]
p7 <- ggplot(observ_count_group_sub_sub, aes(x=fortypcd_group, fill = fortypcd_group)) + 
  geom_bar() + scale_fill_manual(name = 'Forest Type Group', 
                                  values = c("Oak/pine" = '#FF7200FF',  "Loblolly/shortleaf pine" = '#FF8827FF', "Oak/hickory" = '#FF9C4CFF', "White/red/jack pine" = '#FFB274FF', "Elm/ash/cottonwood" = '#F1CAA8FF', "Maple/beech/birch"  = '#E3E1DCFF',
                                             "Other hardwoods" = '#C2CEAAFF', "Spruce/fir" = '#A1BA77FF', "Aspen/birch" = '#8BAC54FF', "Oak/gum/cypress group" = '#7EA13EFF', "Exotic hardwoods" = '#648C16FF'),
                                  drop = FALSE) + theme_bw() + theme(axis.text.x = element_blank()) + labs(title = ownership_index[i])
i=8
observ_count_group_sub_sub <- observ_count_group_sub[which(observ_count_group_sub$ownership == ownership_index[i]),c(2,3)]
p8 <- ggplot(observ_count_group_sub_sub, aes(x=fortypcd_group, fill = fortypcd_group)) + 
  geom_bar() + scale_fill_manual(name = 'Forest Type Group', 
                                  values = c("Oak/pine" = '#FF7200FF',  "Loblolly/shortleaf pine" = '#FF8827FF', "Oak/hickory" = '#FF9C4CFF', "White/red/jack pine" = '#FFB274FF', "Elm/ash/cottonwood" = '#F1CAA8FF', "Maple/beech/birch"  = '#E3E1DCFF',
                                             "Other hardwoods" = '#C2CEAAFF', "Spruce/fir" = '#A1BA77FF', "Aspen/birch" = '#8BAC54FF', "Oak/gum/cypress group" = '#7EA13EFF', "Exotic hardwoods" = '#648C16FF'),
                                  drop = FALSE) + theme_bw()+ theme(axis.text.x = element_blank()) + labs(title = ownership_index[i])
i=9
observ_count_group_sub_sub <- observ_count_group_sub[which(observ_count_group_sub$ownership == ownership_index[i]),c(2,3)]
p9 <- ggplot(observ_count_group_sub_sub, aes(x=fortypcd_group, fill = fortypcd_group)) + 
  geom_bar() + scale_fill_manual(name = 'Forest Type Group', 
                                  values = c("Oak/pine" = '#FF7200FF',  "Loblolly/shortleaf pine" = '#FF8827FF', "Oak/hickory" = '#FF9C4CFF', "White/red/jack pine" = '#FFB274FF', "Elm/ash/cottonwood" = '#F1CAA8FF', "Maple/beech/birch"  = '#E3E1DCFF',
                                             "Other hardwoods" = '#C2CEAAFF', "Spruce/fir" = '#A1BA77FF', "Aspen/birch" = '#8BAC54FF', "Oak/gum/cypress group" = '#7EA13EFF', "Exotic hardwoods" = '#648C16FF'),
                                  drop = FALSE) + theme_bw() + theme(axis.text.x = element_blank()) + labs(title = ownership_index[i])
i=10
observ_count_group_sub_sub <- observ_count_group_sub[which(observ_count_group_sub$ownership == ownership_index[i]),c(2,3)]
p10 <- ggplot(observ_count_group_sub_sub, aes(x=fortypcd_group, fill = fortypcd_group)) + 
  geom_bar() + scale_fill_manual(name = 'Forest Type Group', 
                                  values = c("Oak/pine" = '#FF7200FF',  "Loblolly/shortleaf pine" = '#FF8827FF', "Oak/hickory" = '#FF9C4CFF', "White/red/jack pine" = '#FFB274FF', "Elm/ash/cottonwood" = '#F1CAA8FF', "Maple/beech/birch"  = '#E3E1DCFF',
                                             "Other hardwoods" = '#C2CEAAFF', "Spruce/fir" = '#A1BA77FF', "Aspen/birch" = '#8BAC54FF', "Oak/gum/cypress group" = '#7EA13EFF', "Exotic hardwoods" = '#648C16FF'),
                                  drop = FALSE) + theme_bw() + theme(axis.text.x = element_blank()) + labs(title = ownership_index[i])

ggarrange(p1+ rremove("ylab") + rremove("xlab"),p2+ rremove("ylab") + rremove("xlab"),p3+ rremove("ylab") + rremove("xlab"),p4+ rremove("ylab") + rremove("xlab"),p5+ rremove("ylab") + rremove("xlab"),p6+ rremove("ylab") + rremove("xlab"),p7+ rremove("ylab") + rremove("xlab"),p8+ rremove("ylab") + rremove("xlab"),p9+ rremove("ylab") + rremove("xlab"),p10+ rremove("ylab") + rremove("xlab"), ncol=2, nrow=5, common.legend = TRUE, legend="bottom")

dev.off() 

# SUMMARY STATISTICS ####
## forest/park level stand statistics ####
#please compute mean and standard deviation for "mean_dia", "mean_actualht", 
#"mean_HD",  "tph" and "baph." Exp. dia needs to be created by
#tree_plot_data$exp_dia <- exp(tree_plot_data$mean_dia). Display the mean with 
#the standard deviation in parentheses. #

paper_data_selected <- paper_data %>% 
  mutate(exp_dia = exp(mean_dia)) %>%
  dplyr::select(c(ownership, mean_dia, exp_dia , mean_actualht, mean_HD, tph, baph))
paper_data_summary <- paper_data_selected %>% 
  group_by(ownership) %>% 
  summarise_at(.vars = c("mean_dia", "exp_dia" , "mean_actualht", "mean_HD", "tph", "baph"),
               .funs = c("mean"=~mean(.),
                         "sd"=~sd(.))) 
paper_data_summary_final <- paper_data_summary %>% 
  mutate(across(where(is.numeric), round, 1))
paper_data_summary_final$exp_dia_mean <- formatC(paper_data_summary_final$exp_dia_mean, format = "e", digits = 2)
paper_data_summary_final$exp_dia_sd <- formatC(paper_data_summary_final$exp_dia_sd, format = "e", digits = 2)
paper_data_summary_final$mean_dia <- paste0(paper_data_summary_final$mean_dia_mean, " (", paper_data_summary_final$mean_dia_sd, ")")
paper_data_summary_final$exp_dia <- paste0(paper_data_summary_final$exp_dia_mean, " (", paper_data_summary_final$exp_dia_sd, ")")
paper_data_summary_final$mean_actualht <- paste0(paper_data_summary_final$mean_actualht_mean, " (", paper_data_summary_final$mean_actualht_sd, ")")
paper_data_summary_final$mean_HD <- paste0(paper_data_summary_final$mean_HD_mean, " (", paper_data_summary_final$mean_HD_sd, ")")
paper_data_summary_final$tph <- paste0(paper_data_summary_final$tph_mean, " (", paper_data_summary_final$tph_sd, ")")
paper_data_summary_final$baph <- paste0(paper_data_summary_final$baph_mean, " (", paper_data_summary_final$baph_sd, ")")
paper_data_summary_final <- paper_data_summary_final %>% 
  dplyr::select(ownership, mean_dia, exp_dia, mean_actualht, mean_HD, tph, baph)
paper_data_summary_final <- paper_data_summary_final[c(1,2,7,8,4,3,6,10,5,9),]
clipr::write_last_clip()

## forest type level stand statistics ####

paper_data_selected <- paper_data %>% 
  mutate(exp_dia = exp(mean_dia)) %>%
  dplyr::select(c(mean_dia, exp_dia , mean_actualht, mean_HD, tph, baph, fortypcd))
paper_data_summary <- paper_data_selected %>% 
  group_by(fortypcd) %>% 
  summarise_at(.vars = c("mean_dia", "exp_dia" , "mean_actualht", "mean_HD", "tph", "baph"),
               .funs = c("mean"=~mean(.),
                         "sd"=~sd(.))) 
paper_data_summary_final <- paper_data_summary %>% 
  mutate(across(where(is.numeric), round, 1))
paper_data_summary_final$exp_dia_mean <- formatC(paper_data_summary_final$exp_dia_mean, format = "e", digits = 2)
paper_data_summary_final$exp_dia_sd <- formatC(paper_data_summary_final$exp_dia_sd, format = "e", digits = 2)
paper_data_summary_final$mean_dia <- paste0(paper_data_summary_final$mean_dia_mean, " (", paper_data_summary_final$mean_dia_sd, ")")
paper_data_summary_final$exp_dia <- paste0(paper_data_summary_final$exp_dia_mean, " (", paper_data_summary_final$exp_dia_sd, ")")
paper_data_summary_final$mean_actualht <- paste0(paper_data_summary_final$mean_actualht_mean, " (", paper_data_summary_final$mean_actualht_sd, ")")
paper_data_summary_final$mean_HD <- paste0(paper_data_summary_final$mean_HD_mean, " (", paper_data_summary_final$mean_HD_sd, ")")
paper_data_summary_final$tph <- paste0(paper_data_summary_final$tph_mean, " (", paper_data_summary_final$tph_sd, ")")
paper_data_summary_final$baph <- paste0(paper_data_summary_final$baph_mean, " (", paper_data_summary_final$baph_sd, ")")
paper_data_summary_final <- paper_data_summary_final %>% 
  dplyr::select(fortypcd, mean_dia, exp_dia, mean_actualht, mean_HD, tph, baph)
paper_data_summary_final <- left_join(paper_data_summary_final, forest_type_code, by = "fortypcd")
paper_data_summary_final <- paper_data_summary_final[,-1]
paper_data_summary_final <- paper_data_summary_final[, c(ncol(paper_data_summary_final), 1:(ncol(paper_data_summary_final)-1))]
paper_data_summary_final
clipr::write_last_clip()

# CARBON CALCULATIONS ####
## carbon stats for all forest type groups and region ####
carbon_stats_final <- paper_data 
carbon_stats_final$fortypcd_group <- ifelse(carbon_stats_final$fortypcd >= 100 & carbon_stats_final$fortypcd < 120, "White/red/jack pine",
                                            ifelse(carbon_stats_final$fortypcd >= 120 & carbon_stats_final$fortypcd < 140, "Spruce/fir",
                                                   ifelse(carbon_stats_final$fortypcd >= 140 & carbon_stats_final$fortypcd < 150, "Longleaf/slash pine",
                                                          ifelse(carbon_stats_final$fortypcd >= 160 & carbon_stats_final$fortypcd < 170, "Loblolly/shortleaf pine",
                                                                 ifelse(carbon_stats_final$fortypcd >= 170 & carbon_stats_final$fortypcd < 180, "Pinyon/juniper",
                                                                        ifelse(carbon_stats_final$fortypcd >= 180 & carbon_stats_final$fortypcd < 200, "Douglas-fir",
                                                                               ifelse(carbon_stats_final$fortypcd >= 220 & carbon_stats_final$fortypcd < 240, "Ponderosa pine",
                                                                                      ifelse(carbon_stats_final$fortypcd >= 400 & carbon_stats_final$fortypcd < 500, "Oak/pine",
                                                                                             ifelse(carbon_stats_final$fortypcd >= 500 & carbon_stats_final$fortypcd < 600, "Oak/hickory",
                                                                                                    ifelse(carbon_stats_final$fortypcd >= 600 & carbon_stats_final$fortypcd < 700, "Oak/gum/cypress group",
                                                                                                           ifelse(carbon_stats_final$fortypcd >= 700 & carbon_stats_final$fortypcd < 800, "Elm/ash/cottonwood",
                                                                                                                  ifelse(carbon_stats_final$fortypcd >= 800 & carbon_stats_final$fortypcd < 900, "Maple/beech/birch",
                                                                                                                         ifelse(carbon_stats_final$fortypcd >= 900 & carbon_stats_final$fortypcd < 910, "Aspen/birch",
                                                                                                                                ifelse(carbon_stats_final$fortypcd >= 910 & carbon_stats_final$fortypcd < 920, "Alder/maple",
                                                                                                                                       ifelse(carbon_stats_final$fortypcd >= 960 & carbon_stats_final$fortypcd < 970, "Other hardwoods",
                                                                                                                                              ifelse(carbon_stats_final$fortypcd >= 990 & carbon_stats_final$fortypcd < 999, "Exotic hardwoods",
                                                                                                                                                     ifelse(carbon_stats_final$fortypcd == 999, "Nonstocked", "check")))))))))))))))))



carbon_stats_final <- carbon_stats_final %>% 
  group_by(ownership, fortypcd_group) %>% 
  summarise_at(.vars = "cpa",
               .funs = c("mean"=~mean(.),
               "sd"=~sd(.)))
carbon_stats_final[is.na(carbon_stats_final)] <- 0 
carbon_stats_final <- carbon_stats_final %>% 
  mutate(across(where(is.numeric), round, 1))
#
carbon_stats_final2 <- carbon_stats_final
#
carbon_stats_final$combined <- paste0(carbon_stats_final$mean, " (", carbon_stats_final$sd, ")")
carbon_stats_final <- carbon_stats_final %>% 
  dplyr::select(c(ownership, fortypcd_group, combined))
carbon_stats_final <- dcast(carbon_stats_final, fortypcd_group ~ ownership)
carbon_stats_final[is.na(carbon_stats_final)] <- 0
carbon_stats_final$forest_type <- ifelse(carbon_stats_final$fortypcd == 103, "EWP",
                                   ifelse(carbon_stats_final$fortypcd == 104, "EWP/EH",
                                          ifelse(carbon_stats_final$fortypcd == 105, "EH",
                                                 ifelse(carbon_stats_final$fortypcd == 161, "LBP",
                                                        ifelse(carbon_stats_final$fortypcd == 162, "SLP",
                                                               ifelse(carbon_stats_final$fortypcd == 163, "VP",
                                                                      ifelse(carbon_stats_final$fortypcd == 165, "TMP",
                                                                             ifelse(carbon_stats_final$fortypcd == 167, "PP",
                                                                                    ifelse(carbon_stats_final$fortypcd == 401, "EWP/NRO/WA",
                                                                                           ifelse(carbon_stats_final$fortypcd == 404, "SLP/O",
                                                                                                  ifelse(carbon_stats_final$fortypcd == 405, "VP/SRO",
                                                                                                         ifelse(carbon_stats_final$fortypcd == 406, "LBP/HW",
                                                                                                                ifelse(carbon_stats_final$fortypcd == 409, "OP/HW",
                                                                                                                       ifelse(carbon_stats_final$fortypcd == 501, "PO/BO",
                                                                                                                              ifelse(carbon_stats_final$fortypcd == 502, "CO",
                                                                                                                                     ifelse(carbon_stats_final$fortypcd == 503, "WO/RO/H",
                                                                                                                                            ifelse(carbon_stats_final$fortypcd == 504, "WO",
                                                                                                                                                   ifelse(carbon_stats_final$fortypcd == 505, "NRO",
                                                                                                                                                          ifelse(carbon_stats_final$fortypcd == 506, "YP/WO/NRO",
                                                                                                                                                                 ifelse(carbon_stats_final$fortypcd == 508, "SG/YP",
                                                                                                                                                                        ifelse(carbon_stats_final$fortypcd == 510, "SO",
                                                                                                                                                                               ifelse(carbon_stats_final$fortypcd == 511, "YP",
                                                                                                                                                                                      ifelse(carbon_stats_final$fortypcd == 513, "BL",
                                                                                                                                                                                             ifelse(carbon_stats_final$fortypcd == 515, "CO/BO/SO",
                                                                                                                                                                                                    ifelse(carbon_stats_final$fortypcd == 516, "C/WA/YP",
                                                                                                                                                                                                           ifelse(carbon_stats_final$fortypcd == 517, "E/A/BL",
                                                                                                                                                                                                                  ifelse(carbon_stats_final$fortypcd == 519, "RM/O",
                                                                                                                                                                                                                         ifelse(carbon_stats_final$fortypcd == 520, "MUH",
                                                                                                                                                                                                                                ifelse(carbon_stats_final$fortypcd == 702, "RB/S",
                                                                                                                                                                                                                                       ifelse(carbon_stats_final$fortypcd == 708, "RM/LL",
                                                                                                                                                                                                                                              ifelse(carbon_stats_final$fortypcd == 801, "SM/B/YB",
                                                                                                                                                                                                                                                     ifelse(carbon_stats_final$fortypcd == 962, "OHW",
                                                                                                                                                                                                                                                            ifelse(carbon_stats_final$fortypcd == 123, "RS",
                                                                                                                                                                                                                                                                   ifelse(carbon_stats_final$fortypcd == 507, "S/P",
                                                                                                                                                                                                                                                                          ifelse(carbon_stats_final$fortypcd == 805, "HM/B",
                                                                                                                                                                                                                                                                                 ifelse(carbon_stats_final$fortypcd == 809, "RM/U",
                                                                                                                                                                                                                                                                                        ifelse(carbon_stats_final$fortypcd == 512, "BW",
                                                                                                                                                                                                                                                                                               ifelse(carbon_stats_final$fortypcd == 608, "S/ST/RM",
                                                                                                                                                                                                                                                                                                      ifelse(carbon_stats_final$fortypcd == 802, "BC",
                                                                                                                                                                                                                                                                                                             ifelse(carbon_stats_final$fortypcd == 991, "PLW",
                                                                                                                                                                                                                                                                                                                    ifelse(carbon_stats_final$fortypcd == 905, "PC",
                                                                                                                                                                                                                                                                                                                           ifelse(carbon_stats_final$fortypcd == 514, "SSO",
                                                                                                                                                                                                                                                                                                                                  ifelse(carbon_stats_final$fortypcd == 706, "S/H/E/GA",
                                                                                                                                                                                                                                                                                                                                         ifelse(carbon_stats_final$fortypcd == 705, "S/P/AE",
                                                                                                                                                                                                                                                                                                                                                ifelse(carbon_stats_final$fortypcd == 901, "A", "check"
                                                                                                                                                                                                                                                                                                                                                )))))))))))))))))))))))))))))))))))))))))))))

carbon_stats_final <- carbon_stats_final[complete.cases(carbon_stats_final),]
carbon_stats_final <- carbon_stats_final %>% 
  rename("CONF" = "Chattahoochee-Oconee",
         "CNF" = "Cherokee",
         "NNF" = "Nantahala",
         "GWJNF" = "George Washington-Jefferson",
         "PNF" = "Pisgah",
         "DBNF" = "Daniel Boone",
         "MNF" = "Monongahela",
         "WNF" = "Wayne",
         "GSMNP" = "Great Smoky Mountains",
         "SNP" = "Shenandoah")
         
carbon_stats_final <- carbon_stats_final[,c(1,2,3,8,9,5,4,7,11,6,10)]
# carbon_stats_final2 <- data.frame(t(carbon_stats_final[-1]))
# colnames(carbon_stats_final2) <- carbon_stats_final[, 1]
carbon_stats_final <- data.frame(t(carbon_stats_final))
clipr::write_clip(carbon_stats_final)




## carbon stats over all forest types and region ####
carbon_forest_type <- paper_data 
carbon_forest_type <- carbon_forest_type %>% 
  group_by(ownership, fortypcd) %>% 
  summarise_at(.vars = "cpa",
               .funs = c("mean"=~mean(.),
                         "sd"=~sd(.)))
carbon_forest_type[is.na(carbon_forest_type)] <- 0 
carbon_forest_type <- carbon_forest_type %>% 
  mutate(across(where(is.numeric), round, 1))

carbon_forest_type$combined <- paste0(carbon_forest_type$mean, " (", carbon_forest_type$sd, ")")
carbon_forest_type <- carbon_forest_type %>% 
  dplyr::select(c(ownership, fortypcd, combined))
carbon_forest_type <- dcast(carbon_forest_type, fortypcd ~ ownership)
carbon_forest_type[is.na(carbon_forest_type)] <- 0
carbon_forest_type <- left_join(carbon_forest_type, forest_type_code, by = "fortypcd")
carbon_forest_type <- carbon_forest_type[,-1]
carbon_forest_type <- carbon_forest_type[, c(ncol(carbon_forest_type), 1:(ncol(carbon_forest_type)-1))]
carbon_forest_type <- `rownames<-`(carbon_forest_type, carbon_forest_type$FIA.Forest.Type)
carbon_forest_type <- carbon_forest_type[,-1]
carbon_forest_type <- data.frame(t(carbon_forest_type))
colnames(carbon_forest_type) <- gsub("...", " / ", colnames(carbon_forest_type), fixed = TRUE)
colnames(carbon_forest_type) <- gsub(".", " ", colnames(carbon_forest_type), fixed = TRUE)

# 
# carbon_stats_final <- carbon_stats_final %>% 
#   rename("CONF" = "Chattahoochee-Oconee",
#          "CNF" = "Cherokee",
#          "NNF" = "Nantahala",
#          "GWJNF" = "George Washington-Jefferson",
#          "PNF" = "Pisgah",
#          "DBNF" = "Daniel Boone",
#          "MNF" = "Monongahela",
#          "WNF" = "Wayne",
#          "GSMNP" = "Great Smoky Mountains",
#          "SNP" = "Shenandoah")

carbon_forest_type <- carbon_forest_type[c(1,2,7,8,4,3,6,10,5,9),]
carbon_forest_type <- carbon_forest_type[complete.cases(carbon_forest_type),]
clipr::write_clip(carbon_forest_type)

## distribution of total carbon ####
carbon_stats_final2$forest_type <- ifelse(carbon_stats_final2$fortypcd == 103, "EWP",
                                         ifelse(carbon_stats_final2$fortypcd == 104, "EWP/EH",
                                                ifelse(carbon_stats_final2$fortypcd == 105, "EH",
                                                       ifelse(carbon_stats_final2$fortypcd == 161, "LBP",
                                                              ifelse(carbon_stats_final2$fortypcd == 162, "SLP",
                                                                     ifelse(carbon_stats_final2$fortypcd == 163, "VP",
                                                                            ifelse(carbon_stats_final2$fortypcd == 165, "TMP",
                                                                                   ifelse(carbon_stats_final2$fortypcd == 167, "PP",
                                                                                          ifelse(carbon_stats_final2$fortypcd == 401, "EWP/NRO/WA",
                                                                                                 ifelse(carbon_stats_final2$fortypcd == 404, "SLP/O",
                                                                                                        ifelse(carbon_stats_final2$fortypcd == 405, "VP/SRO",
                                                                                                               ifelse(carbon_stats_final2$fortypcd == 406, "LBP/HW",
                                                                                                                      ifelse(carbon_stats_final2$fortypcd == 409, "OP/HW",
                                                                                                                             ifelse(carbon_stats_final2$fortypcd == 501, "PO/BO",
                                                                                                                                    ifelse(carbon_stats_final2$fortypcd == 502, "CO",
                                                                                                                                           ifelse(carbon_stats_final2$fortypcd == 503, "WO/RO/H",
                                                                                                                                                  ifelse(carbon_stats_final2$fortypcd == 504, "WO",
                                                                                                                                                         ifelse(carbon_stats_final2$fortypcd == 505, "NRO",
                                                                                                                                                                ifelse(carbon_stats_final2$fortypcd == 506, "YP/WO/NRO",
                                                                                                                                                                       ifelse(carbon_stats_final2$fortypcd == 508, "SG/YP",
                                                                                                                                                                              ifelse(carbon_stats_final2$fortypcd == 510, "SO",
                                                                                                                                                                                     ifelse(carbon_stats_final2$fortypcd == 511, "YP",
                                                                                                                                                                                            ifelse(carbon_stats_final2$fortypcd == 513, "BL",
                                                                                                                                                                                                   ifelse(carbon_stats_final2$fortypcd == 515, "CO/BO/SO",
                                                                                                                                                                                                          ifelse(carbon_stats_final2$fortypcd == 516, "C/WA/YP",
                                                                                                                                                                                                                 ifelse(carbon_stats_final2$fortypcd == 517, "E/A/BL",
                                                                                                                                                                                                                        ifelse(carbon_stats_final2$fortypcd == 519, "RM/O",
                                                                                                                                                                                                                               ifelse(carbon_stats_final2$fortypcd == 520, "MUH",
                                                                                                                                                                                                                                      ifelse(carbon_stats_final2$fortypcd == 702, "RB/S",
                                                                                                                                                                                                                                             ifelse(carbon_stats_final2$fortypcd == 708, "RM/LL",
                                                                                                                                                                                                                                                    ifelse(carbon_stats_final2$fortypcd == 801, "SM/B/YB",
                                                                                                                                                                                                                                                           ifelse(carbon_stats_final2$fortypcd == 962, "OHW",
                                                                                                                                                                                                                                                                  ifelse(carbon_stats_final2$fortypcd == 123, "RS",
                                                                                                                                                                                                                                                                         ifelse(carbon_stats_final2$fortypcd == 507, "S/P",
                                                                                                                                                                                                                                                                                ifelse(carbon_stats_final2$fortypcd == 805, "HM/B",
                                                                                                                                                                                                                                                                                       ifelse(carbon_stats_final2$fortypcd == 809, "RM/U",
                                                                                                                                                                                                                                                                                              ifelse(carbon_stats_final2$fortypcd == 512, "BW",
                                                                                                                                                                                                                                                                                                     ifelse(carbon_stats_final2$fortypcd == 608, "S/ST/RM",
                                                                                                                                                                                                                                                                                                            ifelse(carbon_stats_final2$fortypcd == 802, "BC",
                                                                                                                                                                                                                                                                                                                   ifelse(carbon_stats_final2$fortypcd == 991, "PLW",
                                                                                                                                                                                                                                                                                                                          ifelse(carbon_stats_final2$fortypcd == 905, "PC",
                                                                                                                                                                                                                                                                                                                                 ifelse(carbon_stats_final2$fortypcd == 514, "SSO",
                                                                                                                                                                                                                                                                                                                                        ifelse(carbon_stats_final2$fortypcd == 706, "S/H/E/GA",
                                                                                                                                                                                                                                                                                                                                               ifelse(carbon_stats_final2$fortypcd == 705, "S/P/AE",
                                                                                                                                                                                                                                                                                                                                                      ifelse(carbon_stats_final2$fortypcd == 901, "A", "check"
                                                                                                                                                                                                                                                                                                                                                      )))))))))))))))))))))))))))))))))))))))))))))



carbon_stats_final2 <- carbon_stats_final2 %>% 
  rename("CONF" = "Chattahoochee-Oconee",
         "CNF" = "Cherokee",
         "NNF" = "Nantahala",
         "GWJNF" = "George Washington-Jefferson",
         "PNF" = "Pisgah",
         "DBNF" = "Daniel Boone",
         "MNF" = "Monongahela",
         "WNF" = "Wayne",
         "GSMNP" = "Great Smoky Mountains",
         "SNP" = "Shenandoah")

carbon_stats_final2 <- carbon_stats_final2[,c(1,12,2,3,8,9,5,4,7,11,6,10)]
carbon_stats_final2 <- data.frame(t(carbon_stats_final2[-1]))
colnames(carbon_stats_final2) <- carbon_stats_final2[, 1]
names <- rownames(carbon_stats_final2)
rownames(carbon_stats_final2) <- NULL
carbon_stats_final2 <- cbind(names,carbon_stats_final2)
setwd("C:/Users/tara/Desktop/co-author paper/carbon forest type")
forest_type_no <- c(2:11)
colnames(carbon_stats_final2) <- as.character(carbon_stats_final2[1, ])
carbon_stats_final2 <- carbon_stats_final2[-1,]
forest_type_name <- colnames(carbon_stats_final2)
forest_type_name <- forest_type_name[2:11]
for(i in length(forest_type_no)){
  temp_data <- carbon_stats_final2 %>% 
    select(forest_type, forest_type_name[i])
  png(filename = paste0(forest_type_name[i], "_bar_graph.png", sep = ""),
      width = 6,
      height = 6,
      units = "in",
      res = 300)
  title <- expression(paste("Total carbon per ha (tons/ha)"))
  k <- ggplot(temp_data, aes(cpa)) + geom_histogram(bins = 30, color="gray", fill="gray") + 
    scale_color_grey() + theme_bw() + labs(x=title, y = "Count") + theme(axis.line = element_line(colour = "black", 
                                                                                                  linetype = "solid"),
                                                                         axis.text=element_text(size=14),
                                                                         axis.title=element_text(size=16,face="bold"))
  print(k)
  dev.off()
}


## average carbon for ownership ####
average_carbon <- paper_data %>% 
  group_by(ownership) %>% 
  summarise_at(.vars = "cpa",
               .funs = c("mean"=~mean(.),
                       "sd"=~sd(.))) %>% 
  mutate(across(where(is.numeric), round, 1))
average_carbon$combined <- paste0(average_carbon$mean, " (", average_carbon$sd, ")")
average_carbon <- average_carbon %>% 
  select(c(ownership, combined)) 
average_carbon <- average_carbon[c(1,2,7,8,4,3,6,10,5,9),]
clipr::write_clip(average_carbon)

# test comparison against carbon_data_process average cycle cpa
average_carbon_test <- paper_data 
average_carbon_test$fortypcd_group <- ifelse(average_carbon_test$fortypcd >= 100 & average_carbon_test$fortypcd < 120, "White/red/jack pine",
                                                             ifelse(average_carbon_test$fortypcd >= 120 & average_carbon_test$fortypcd < 140, "Spruce/fir",
                                                                    ifelse(average_carbon_test$fortypcd >= 140 & average_carbon_test$fortypcd < 150, "Longleaf/slash pine",
                                                                           ifelse(average_carbon_test$fortypcd >= 160 & average_carbon_test$fortypcd < 170, "Loblolly/shortleaf pine",
                                                                                  ifelse(average_carbon_test$fortypcd >= 170 & average_carbon_test$fortypcd < 180, "Pinyon/juniper",
                                                                                         ifelse(average_carbon_test$fortypcd >= 180 & average_carbon_test$fortypcd < 200, "Douglas-fir",
                                                                                                ifelse(average_carbon_test$fortypcd >= 220 & average_carbon_test$fortypcd < 240, "Ponderosa pine",
                                                                                                       ifelse(average_carbon_test$fortypcd >= 400 & average_carbon_test$fortypcd < 500, "Oak/pine",
                                                                                                              ifelse(average_carbon_test$fortypcd >= 500 & average_carbon_test$fortypcd < 600, "Oak/hickory",
                                                                                                                     ifelse(average_carbon_test$fortypcd >= 600 & average_carbon_test$fortypcd < 700, "Oak/gum/cypress group",
                                                                                                                            ifelse(average_carbon_test$fortypcd >= 700 & average_carbon_test$fortypcd < 800, "Elm/ash/cottonwood",
                                                                                                                                   ifelse(average_carbon_test$fortypcd >= 800 & average_carbon_test$fortypcd < 900, "Maple/beech/birch",
                                                                                                                                          ifelse(average_carbon_test$fortypcd >= 900 & average_carbon_test$fortypcd < 910, "Aspen/birch",
                                                                                                                                                 ifelse(average_carbon_test$fortypcd >= 910 & average_carbon_test$fortypcd < 920, "Alder/maple",
                                                                                                                                                        ifelse(average_carbon_test$fortypcd >= 960 & average_carbon_test$fortypcd < 970, "Other hardwoods",
                                                                                                                                                               ifelse(average_carbon_test$fortypcd >= 990 & average_carbon_test$fortypcd < 999, "Exotic hardwoods",
                                                                                                                                                                      ifelse(average_carbon_test$fortypcd == 999, "Nonstocked", "check")))))))))))))))))

average_carbon_test <- average_carbon_test %>% 
  distinct(STATECD, COUNTYCD, PLOT, CONDID, CYCLE, .keep_all = TRUE) %>%
  filter(fortypcd != 999 & ownership == "Cherokee") %>% #Great Smoky Mountains
  group_by(STATECD, COUNTYCD, PLOT, CONDID, CYCLE, fortypcd_group) %>% 
  dplyr::group_by(CYCLE, fortypcd_group) %>%
  dplyr::summarise_at(.vars = c("cpa"), 
                      .funs = c("low"=~quantile(., probs = 0.025),
                                "med"=~quantile(., probs = 0.5),
                                "up"=~quantile(., probs = 0.975))) 

png(filename = paste0("C:/Users/tara/Desktop/carbon_project_1/CPA_changes_over_time/Cherokee.png",sep =""),
    width = 7,
    height = 4,
    units = "in",
    res = 500)
r <- ggplot(average_carbon_test, aes(CYCLE, med, group = fortypcd_group)) + geom_point(aes(color=fortypcd_group)) + geom_line(aes(color=fortypcd_group)) +  
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




paper_data <- read.csv("tree_plot_data_revision.csv") %>% 
  filter(ownership != "New River Gorge")

park_forest_names <- unique(paper_data$ownership)

# HISTOGRAMS ####
setwd("C:/Users/tara/Desktop/co-author paper/histograms")
for(i in 1:length(park_forest_names)){
  paper_data_subset <- paper_data %>% 
    filter(ownership == park_forest_names[i])
  
  png(filename = paste0(park_forest_names[i], "_histogram.png", sep = ""),
      width = 6,
      height = 6,
      units = "in",
      res = 300)
  title <- expression(paste("Total carbon per ha (tons/ha)"))
  k <- ggplot(paper_data_subset, aes(cpa)) + geom_histogram(bins = 30, color="gray", fill="gray") + 
    scale_color_grey() + theme_bw() + labs(x=title, y = "Count") + theme(axis.line = element_line(colour = "black", 
                                                                                                    linetype = "solid"),
                                                                         axis.text=element_text(size=14),
                                                                         axis.title=element_text(size=16,face="bold"))
  print(k)
  dev.off()
  
}

# Count by national park/forest and forest type group ####
observ_count_group <- paper_data
observ_count_group$fortypcd_group <- ifelse(observ_count_group$fortypcd >= 100 & observ_count_group$fortypcd < 120, "White/red/jack pine",
                                            ifelse(observ_count_group$fortypcd >= 120 & observ_count_group$fortypcd < 140, "Spruce/fir",
                                                   ifelse(observ_count_group$fortypcd >= 140 & observ_count_group$fortypcd < 150, "Longleaf/slash pine",
                                                          ifelse(observ_count_group$fortypcd >= 160 & observ_count_group$fortypcd < 170, "Loblolly/shortleaf pine",
                                                                 ifelse(observ_count_group$fortypcd >= 170 & observ_count_group$fortypcd < 180, "Pinyon/juniper",
                                                                        ifelse(observ_count_group$fortypcd >= 180 & observ_count_group$fortypcd < 200, "Douglas-fir",
                                                                               ifelse(observ_count_group$fortypcd >= 220 & observ_count_group$fortypcd < 240, "Ponderosa pine",
                                                                                      ifelse(observ_count_group$fortypcd >= 400 & observ_count_group$fortypcd < 500, "Oak/pine",
                                                                                             ifelse(observ_count_group$fortypcd >= 500 & observ_count_group$fortypcd < 600, "Oak/hickory",
                                                                                                    ifelse(observ_count_group$fortypcd >= 600 & observ_count_group$fortypcd < 700, "Oak/gum/cypress group",
                                                                                                           ifelse(observ_count_group$fortypcd >= 700 & observ_count_group$fortypcd < 800, "Elm/ash/cottonwood",
                                                                                                                  ifelse(observ_count_group$fortypcd >= 800 & observ_count_group$fortypcd < 900, "Maple/beech/birch",
                                                                                                                         ifelse(observ_count_group$fortypcd >= 900 & observ_count_group$fortypcd < 910, "Aspen/birch",
                                                                                                                                ifelse(observ_count_group$fortypcd >= 910 & observ_count_group$fortypcd < 920, "Alder/maple",
                                                                                                                                      ifelse(observ_count_group$fortypcd >= 960 & observ_count_group$fortypcd < 970, "Other hardwoods",
                                                                                                                                             ifelse(observ_count_group$fortypcd >= 990 & observ_count_group$fortypcd < 999, "Exotic hardwoods",
                                                                                                                                                    ifelse(observ_count_group$fortypcd == 999, "Nonstocked", "check")))))))))))))))))
cols <- c("ownership", "fortypcd_group", "fortypcd")
observ_count_group_sub <- observ_count_group[, cols] 
observ_count_group_sub <- observ_count_group_sub %>% 
  distinct() %>% 
  count(ownership, fortypcd_group)
observ_count_group_sub <- reshape2::dcast(observ_count_group_sub, fortypcd_group~ownership)
observ_count_group_sub <- observ_count_group_sub[,c(1,2,3,8,9,5,4,7,11,6,10)]
observ_count_group_sub <- data.frame(t(observ_count_group_sub))
observ_count_group_sub[is.na(observ_count_group_sub)] <- 0
clipr::write_clip(observ_count_group_sub)