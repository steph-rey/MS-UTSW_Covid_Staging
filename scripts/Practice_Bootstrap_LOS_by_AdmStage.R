# Bootstrap Practice: LOS by Admission Stage
# Date Created: 2022-02-23
# Date Modified: 2022-02-24

# Install and load libraries ----
# install.packages("sur")  # need for boot.mean func
library(sur)
library(tidyverse)

# Read in datasets; select only necessary variables ----
ut <- readRDS("/Users/sreynolds2/Documents/GitHub/MS-UTSW_Covid_Staging/data/ut_df_unique_pts.rds") %>% 
    select(ID, age, sex, race, ethnicity, LOS, stage_adm)

uc <- readRDS("/Users/sreynolds2/Documents/GitHub/MS-Covid_Staging/data/dm_stg_unique_pt_11.08.21.rds") %>% 
    select(ID, age, sex, race, ethnicity, LOS, stage_adm)

# Since `stage_adm` is coded as ordinal var for UT, need to do the same for UC
uc$stage_adm <- factor(uc$stage_adm, ordered = T, levels = c("4", "5", "6", "7", "8", "9"))

# Frequency counts for admission stage ----
table(uc$stage_adm)
table(ut$stage_adm)

# Calculate mean LOS for UCSF and UTSW ---- 
mean(uc$LOS)
mean(ut$LOS)

# Calculate mean for 1000 bootstrapped samples ----
    # @x: original sample
    # @B: num of boostrapped samples by randomly sampling w/ replacement
    # @n: size of each bootstrapped sample; deafult is size of original sample
boot.mean(uc$LOS, B=1000)$bootstrap.samples
b_uc <- as_tibble(boot.mean(uc$LOS, B=1000))

boot.mean(ut$LOS, B=1000)
b_ut <- as_tibble(boot.mean(ut$LOS, B=1000))

# Bootstrap practice for UCSF ---- 
# Method 1: Group by stage_adm, draw 50 random samples w/ replacement, then calculate bootstrapped mean using 100 replications 
uc.tbl <-uc %>% 
    group_by(stage_adm) %>% 
    slice_sample(n=50, replace=T) %>% 
    summarise(mean_uc_m1 = round(boot.mean(uc$LOS, B=100)$mean, 2)) 

# Method 2: Group by stage_adm, then calculate bootstrap mean using 500 replications
uc.tbl2 <- uc %>% 
    group_by(stage_adm) %>% 
    summarise(mean_uc_m2 = round(boot.mean(uc$LOS, B=500)$mean, 2))

# Bootstrap practice for UTSW ----
# Method 1: Group by stage_adm, draw 50 random samples w/ replacement, then calculate bootstrapped mean using 100 replications 
ut.tbl <- ut %>% 
    group_by(stage_adm) %>% 
    slice_sample(n=50, replace=T) %>% 
    summarise(mean_ut_m1 = round(boot.mean(ut$LOS, B=100)$mean, 2))

# Method 2: Group by stage_adm, then calculate bootstrap mean using 500 replications
ut.tbl2 <- ut %>% 
    group_by(stage_adm) %>% 
    summarise(mean_ut_m2 = round(boot.mean(ut$LOS, B=500)$mean, 2))

# Combine all tables to compare mean LOS for UCSF vs. UTSW for patients of different admission stages 
results <- uc.tbl %>% inner_join(uc.tbl2) %>% inner_join(ut.tbl) %>% inner_join(ut.tbl2)

# Save as txt file 
write.table(results, file = "/Users/sreynolds2/Documents/GitHub/MS-UTSW_Covid_Staging/results/practice_bootstrap_results.txt", sep = ",", quote = FALSE, row.names = F)




