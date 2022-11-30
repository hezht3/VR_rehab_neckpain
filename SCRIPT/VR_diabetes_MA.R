##################
# VR Diabetes MA #
##################

setwd("/Users/zhengtinghe/Library/CloudStorage/OneDrive-JohnsHopkins/Research/rehabilitation/VR_diabetes_Hao&He/VR_rehab_diabetes")

require(tidyverse)
require(metafor)


#################
# Prepare input #
#################

data <- read_csv("./INPUT/VR_diabetes_balance.csv")

for(corr in seq(0, 1, 0.1)) {
    cat("\n")
    cat("\n")
    cat("\n")
    cat(paste("# Correlation of", corr, sep = ""))
    data_es <- escalc(measure = "SMCC",
                      m1i = mean_post, m2i = mean_pre,
                      sd1i = sd_post, sd2i = sd_pre,
                      ni = n_post, ri = rep(corr, 16), data = data)   # assuming no correlation
    for(outc in table(data$outcome) %>% as.data.frame() %>% pull(Var1)) {
        cat("\n")
        cat(paste("###Random effects meta analysis for", outc, sep = ""))
        rma(data = data_es %>% filter(outcome == outc),
            yi = yi,
            vi = vi,
            method = "SJ",
            slab = Study,
            digits = 2,
            level = 95) %>% 
            forest() %>% 
            print()
    }
}
