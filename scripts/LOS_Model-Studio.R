# ModelStudio: Interactive Studio for Explanatory Model Analysis ----

# Install and load required packages ----
install.packages("modelStudio")
install.packages("DALEX")

library(modelStudio)
library(tidyverse)
library(tidymodels)
library(DALEX)

# Load data; deselect unnecessary variables ----
regdf <- readRDS("/Users/sreynolds2/Documents/GitHub/MS-UTSW_Covid_Staging/data/regdf_11.08.21.rds") %>% 
    select(-c(ID, max_stage))

glimpse(regdf)

# Fit data to model ----
m1 <- boost_tree(learn_rate = 0.3) %>% 
    set_mode("regression") %>% 
    set_engine("xgboost") %>% 
    fit(t.LOS ~ ., data = regdf)

m1

# Explainer ---- 
explainer <- DALEX::explain(
    model = m1,
    data = regdf,
    y = regdf$t.LOS,
    label = "XGBOOST"
)

# Model Studio ----
modelStudio::modelStudio(explainer)







