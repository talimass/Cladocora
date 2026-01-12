####### LEPTOSERIS ANALYSIS
options(scipen = 999)
fire.data<- read_csv("data/formatted_fire_data.csv")## you need to path this to your data file

#FIRe Data

fire.data$depth <- as_factor(fire.data$depth)
fire_data <- fire.data %>% na.omit()
## thin data

quartiles <- quantile(fire_data$fv_fm, probs = c(.25,.75))
IQR_cor <- IQR(fire_data$fv_fm)

lower <- quartiles[1] - 1.5*IQR_cor
upper <- quartiles[2] + 1.5*IQR_cor

fvfm_no_outlier <- subset(fire_data, fire_data$fv_fm >lower & fire_data$fv_fm< upper)

quartiles <- quantile(fire_data$Sigma, probs = c(.25,.75))
IQR_cor <- IQR(fire_data$Sigma)

lower <- quartiles[1] - 1.5*IQR_cor
upper <- quartiles[2] + 1.5*IQR_cor

sigma_no_outlier <- subset(fire_data, fire_data$Sigma >lower & fire_data$Sigma< upper)

quartiles <- quantile(fire_data$Pmax.e.s, probs = c(.25,.75))
IQR_cor <- IQR(fire_data$Pmax.e.s)

lower <- quartiles[1] - 1.5*IQR_cor
upper <- quartiles[2] + 1.5*IQR_cor

Pmax.e.s_no_outlier <- subset(fire_data, fire_data$Pmax.e.s >lower & fire_data$Pmax.e.s< upper)

quartiles <- quantile(fire_data$p, probs = c(.25,.75))
IQR_cor <- IQR(fire_data$p)

lower <- quartiles[1] - 1.5*IQR_cor
upper <- quartiles[2] + 1.5*IQR_cor

p_no_outlier <- subset(fire_data, fire_data$p >lower & fire_data$p< upper)

########################## STATS ANALYSIS LEPTOSERIS

fvfm_no_outlier <- fvfm_no_outlier %>% filter(Species %in% "Leptoseris")

KW_fvfm <- kruskal.test(fv_fm ~ depth, data = fvfm_no_outlier)
KW_fvfm
DT_fvfm <- dunnTest(fvfm_no_outlier$fv_fm ~ fvfm_no_outlier$depth, method = "bonferroni")
DT_fvfm

# sigma stats analysis
sigma_no_outlier <- sigma_no_outlier %>% filter(Species %in% "Leptoseris")
KW_sigma<- kruskal.test(Sigma ~ depth, data = sigma_no_outlier)
KW_sigma 
dt_sigma <- dunnTest(sigma_no_outlier$Sigma ~ sigma_no_outlier$depth, method = "bonferroni")
dt_sigma

# p stats analysis
p_no_outlier <- p_no_outlier %>% filter(Species %in% "Leptoseris")
KW_p <- kruskal.test(p ~ depth, data = p_no_outlier)
KW_p
DT_p <- dunnTest(p_no_outlier$p ~ p_no_outlier$depth, method = "bonferroni")
DT_p

#Pmax stats analysis
Pmax.e.s_no_outlier <- Pmax.e.s_no_outlier %>% filter(Species %in% "Leptoseris")
KW_pmax <- kruskal.test(Pmax.e.s ~ depth, data = Pmax.e.s_no_outlier)
KW_pmax
DT_pmax <- dunnTest(Pmax.e.s_no_outlier$Pmax.e.s ~ Pmax.e.s_no_outlier$depth, method = "bonferroni")
DT_pmax

