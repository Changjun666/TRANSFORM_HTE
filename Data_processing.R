library(usethis)
library(devtools)
library(AFTrees)
library(dplyr)
library(ggplot2)
library(readxl)
library(dplyr)

data <- read.csv("D:/hp/Desktop/Research in Yale/TF project/TF_data.csv")

mydata <- data.frame(
  ID = data$ID,
  age = data$AGESLT90,
  sex = data$SEXN,
  non_white = data$non_white,
  black = data$black,
  Asian = data$Asian,
  INSPRIV=data$INSPRIV,
  INSMCAID = data$INSMCAID,
  INSMCARE = data$INSMCARE,
  INSOTH=data$INSOTH,
  INSNONE=data$INSNONE,
  LVEFGR1N_cat1 = data$LVEFGR1N_cat1,
  LVEFGR1N_cat2 = data$LVEFGR1N_cat2,
  LVEFGR1N_cat3 = data$LVEFGR1N_cat3,
  LDPRIORN=data$LDPRIORN,
  LSTLDPN_Fur = data$LSTLDPN_Fur,
  LSTLDPN_Tors = data$LSTLDPN_Tors,
  LSTLDPN_Bum = data$LSTLDPN_Bum,
  BBPSYS=data$BBPSYS,
  BBPDIAS = data$BBPDIAS,
  BPHR = data$BPHR,
  BSODIUM=data$BSODIUM,
  BPOTAS=data$BPOTAS,
  BSCRTMG = data$BSCRTMG,
  BGFR_cat1 = data$BGFR_cat1,
  BGFR_cat2 = data$BGFR_cat2,
  BGFR_cat3 = data$BGFR_cat3,
  BHEMGL=data$BHEMGL,
  BMI=data$BMI,
  EARESNTP = data$EARESNTP,
  EARESBNP = data$EARESBNP,
  HXDMN=data$HXDMN,
  HXAFIBN=data$HXAFIBN,
  HFLSTYR = data$HFLSTYR,
  HFETION_dmy = data$HFETION_dmy,
  HFDURN_cat1 = data$HFDURN_cat1,
  HFDURN_cat2 = data$HFDURN_cat2,
  HFDURN_cat3 = data$HFDURN_cat3,
  NYHAADN_cat1 = data$NYHAADN_cat1,
  NYHAADN_cat2 = data$NYHAADN_cat2,
  NYHAADN_cat3 = data$NYHAADN_cat3,
  NYHAADN_cat4 = data$NYHAADN_cat4,
  NYHRNDN_cat1 = data$NYHRNDN_cat1,
  NYHRNDN_cat2 = data$NYHRNDN_cat2,
  NYHRNDN_cat3 = data$NYHRNDN_cat3,
  NYHRNDN_cat4 = data$NYHRNDN_cat4,
  HXCKDN = data$HXCKDN,
  HXCOPDN = data$HXCOPDN,
  HF_CATN = data$HF_CATN,
  BCLSUMM = data$BCLSUMM,
  BBUNMG = data$BBUNMG,
  LOS = data$LOS,
  BPHQSC = data$BPHQSC,
  BTSYMPT = data$BTSYMPT,
  BQUALIFE = data$BQUALIFE,
  BSOCLIM = data$BSOCLIM,
  BSYMBURD = data$BSYMBURD,
  BSYMFREQ = data$BSYMFREQ,
  BSYMSTAB = data$BSYMSTAB,
  BMRAN = data$BMRAN,
  HXHFN = data$HXHFN,
  HXHTNN = data$HXHTNN,
  HXHYPCN = data$HXHYPCN,
  HXPADN = data$HXPADN,
  HXCADN = data$HXCADN,
  HXMIN = data$HXMIN,
  HXPPCIN = data$HXPPCIN,
  HXCABGN = data$HXCABGN,
  HXSTKN = data$HXSTKN,
  ICDN = data$ICDN,
  CRTN = data$CRTN,
  ACE_inhibitor = data$ACE_inhibitor,
  ARNi = data$ARNi,
  MRA = data$MRA,
  Beta_blocker = data$Beta_blocker,
  Long_acting_nitrate = data$Long_acting_nitrate,
  Thiazide_diuretics = data$Thiazide_diuretics,
  IV_inotrop_vasopress = data$IV_inotrop_vasopress,
  Anticoagulants_and_Antiplatelets = as.integer(
    data$Warfarin == 1 |
      data$Direct_oral_anticoagulant == 1 |
      data$Aspirin == 1 |
      data$Other_antiplatelet_No_Asp == 1
  ),
  Heart_and_Blood_Pressure_Therapies = as.integer(
    data$Digoxin == 1 |
      data$Hydralazine == 1 |
      data$Antiarrhy_No_Bblocker == 1
  ),
  Diabetes_Treatment = as.integer(
    data$SGLT2i == 1 |
      data$GLP1_agonist == 1 |
      data$Oral_gluc_lower_ther == 1 |
      data$Insulin == 1
  ),
  Renin_Angiotensin_System_Inhibitors = as.integer(
    data$ACE_ARB == 1 |
      data$ACE_ARB_ARNi == 1
  ),
  TEACHHSP_teach = data$TEACHHSP_teach,
  TEACHHSP_VA = data$TEACHHSP_VA,
  ETHNICN_dmy = data$ETHNICN_dmy,
  HXSMKN_current = data$HXSMKN_current,
  HXSMKN_former = data$HXSMKN_former,#86 in total
  
  Time = ifelse(data$ADTHDY <= 900,data$ADTHDY, 900),   # Days to Death (ALL)/NDI/Last Known Alive - Primary Censoring
  Event = ifelse(data$ADTHFLN == 1 & data$ADTHDY <= 900 , 1 ,0), # Event status (1: C>T，0: C<T)
  W = ifelse(data$arm_tor_dmy == 1, 1, 0)  # Treatment (1=T, 0=F)
)


std = function(x) {(x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)}
mydata$EARESNTP <- std(data$EARESNTP)
mydata$EARESBNP <- std(data$EARESBNP)

mydata <- mydata %>%
  mutate(EARESMerged = coalesce(EARESNTP, EARESBNP))
mydata$EARESNTP <- NULL
mydata$EARESBNP <- NULL
mean(data$EARESBNP, na.rm= TRUE)
sd(data$EARESBNP, na.rm= TRUE)
(-0.25*sd(data$EARESBNP, na.rm= TRUE))+mean(data$EARESBNP, na.rm= TRUE)
(-0.25*sd(data$EARESNTP, na.rm= TRUE))+mean(data$EARESNTP, na.rm= TRUE)

filter_columns_by_na <- function(X, threshold = 0.25) {
  na_proportion <- colMeans(is.na(X))
  selected_columns <- names(na_proportion[na_proportion < threshold])
  X_filtered <- X[, selected_columns, drop = FALSE]
  return(X_filtered)
}
X <- filter_columns_by_na(mydata)

generate_and_merge_missing_indicators <- function(X) {
  
  for (col in names(X)) {
    if (any(is.na(X[[col]]))) {
      indicator_name <- paste0("missing_indicator_", col)
      
      X[[indicator_name]] <- as.integer(is.na(X[[col]]))
    }
  }
  
  
  indicator_cols <- grep("missing_indicator_", names(X), value = TRUE)
  unique_indicators <- list()
  
  for (col in indicator_cols) {
    
    current_indicator <- X[[col]]
    matched <- FALSE
    
    for (name in names(unique_indicators)) {
      
      if (all(unique_indicators[[name]] == current_indicator)) {
        matched <- TRUE
        
        X[[name]] <- pmax(X[[name]], current_indicator)  
        X[[col]] <- NULL  
        break
      }
    }
    
    
    if (!matched) {
      unique_indicators[[col]] <- current_indicator
    }
  }
  
  return(X)
}

X <- generate_and_merge_missing_indicators(X)

dataNA <- function(X) {
  for (j in 1:ncol(X)) {
    if (is.numeric(X[[j]])) {
      temp <- X[, j]
      unique_vals <- unique(temp[!is.na(temp)]) 
      if (all(unique_vals %in% c(0, 1))) {
        mode_val <- ifelse(sum(temp == 1, na.rm = TRUE) >= sum(temp == 0, na.rm = TRUE), 1, 0)
        temp[is.na(temp)] <- mode_val
      } else {
        temp[is.na(temp)] <- mean(temp, na.rm = TRUE)
      }
      X[, j] <- temp
    } else {
      warning(paste("Column", j, "is not numeric and was not modified."))
    }
  }
  return(X)
}

X <- dataNA(X)

remove_single_level_columns <- function(X) {
  X_filtered <- Filter(function(col) length(unique(col[!is.na(col)])) > 1, X)
  return(X_filtered)
}
X <- remove_single_level_columns(X)

mydata_trt_1 <- X %>%  
  filter(W == 1) %>% 
  select(-W) 

mydata_trt_0 <- X %>%  
  filter(W == 0) %>% 
  select(-W) 


single_level_cols <- names(mydata_trt_1)[sapply(mydata_trt_1, function(col) length(unique(na.omit(col))) == 1) |
                                           sapply(mydata_trt_0, function(col) length(unique(na.omit(col))) == 1)]


X <- X %>% select(-all_of(single_level_cols))
id_imp_1 = setdiff(X$ID, mydata_trt_1$ID)
id_imp_0 = setdiff(X$ID, mydata_trt_0$ID)

mydata_trt_1_imp <- X%>%  
  filter(ID %in% id_imp_1) %>%
  select(-W) 

mydata_trt_0_imp <- X %>%  
  filter(ID %in% id_imp_0) %>% 
  select(-W)

mean(X$Time)
mean(X[X$Event==1,]$Time)
mean(X[X$Event==0,]$Time)
