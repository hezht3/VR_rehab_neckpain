##################
# VR Diabetes MA #
##################

setwd("/Users/zhengtinghe/Library/CloudStorage/OneDrive-JohnsHopkins/Research/rehabilitation/VR neck pain/VR_rehab_neckpain")

require(tidyverse)
require(metafor)


#################
# Prepare input #
#################

data <- read_csv("./INPUT/VR neck pain data.csv")

# Harmonize unit to 1-100
data <- data %>% 
    mutate(across(contains("mean"),
                  ~ case_when((outcome == "1. Neck Disability Index, short-term" |
                               outcome == "2. Neck Disability Index, long-term") &
                                  unit == "%" ~ .x/2,
                              TRUE ~ .x))) %>% 
    mutate(across(contains("sd"),
                  ~ case_when((outcome == "1. Neck Disability Index, short-term" |
                               outcome == "2. Neck Disability Index, long-term") &
                                  unit == "%" ~ .x/2,
                              TRUE ~ .x))) %>% 
    mutate(across(contains("mean"),
                  ~ case_when((outcome == "3. Pain, short-term" |
                               outcome == "4. Pain, long-term") &
                                  unit == "0-100" ~ .x/10,
                              TRUE ~ .x))) %>% 
    mutate(across(contains("sd"),
                  ~ case_when((outcome == "3. Pain, short-term" |
                               outcome == "4. Pain, long-term") &
                                  unit == "0-100" ~ .x/10,
                              TRUE ~ .x)))


# Transform effect size to SMD
data <- escalc(measure = "MD", vtype =  "UB",
               m1i = VR_mean_post, sd1i = VR_sd_post, n1i = VR_n_post,
               m2i = control_mean_post, sd2i = control_sd_post, n2i = control_n_post,
               data = data)


################
# Fit MA model #
################

# 1. Neck Disability Index, short-term
NDI_st <- rma(data = data %>% filter(outcome == "1. Neck Disability Index, short-term"),
              yi = yi,
              vi = vi,
              method = "DL",
              slab = study,
              digits = 2,
              level = 95)
summary(NDI_st)

tiff("./OUTPUT/Figure 2A. RMA forest plot_outcome1.tiff",
     width = 1700, height = 1000, pointsize = 10, res = 300)
forest(NDI_st, xlim = c(-18, 10),
       ilab = data %>%
           filter(outcome == "1. Neck Disability Index, short-term") %>%
           select(contains("_n_post")),
       ilab.xpos = c(-11,-9), cex = 0.75, ylim = c(-1, 8),
       xlab = "Mean Difference",
       mlab = "RE Model for Neck Disability Index, short-term", psize = 1)
text(-18, 7, "Author(s) and Year", pos = 4, cex = 0.8, font = 2)
text(-12, 7, "VR", pos = 4, cex = 0.8, font = 2)
text(-10, 7, "Control", pos = 4, cex = 0.8, font = 2)
text(10, 7, "Mean difference [95% CI]", pos = 2, cex = 0.8, font = 2)
text(-12.5, 8, "No. of participants", pos = 4, cex = 0.8, font = 2)
dev.off()

# 2. Neck Disability Index, long-term
NDI_lt <- rma(data = data %>% filter(outcome == "2. Neck Disability Index, long-term"),
              yi = yi,
              vi = vi,
              method = "DL",
              slab = study,
              digits = 2,
              level = 95)
summary(NDI_lt)

tiff("./OUTPUT/Figure 2B. RMA forest plot_outcome2.tiff",
     width = 1700, height = 1000, pointsize = 10, res = 300)
forest(NDI_lt, xlim = c(-18, 10),
       ilab = data %>%
           filter(outcome == "2. Neck Disability Index, long-term") %>%
           select(contains("_n_post")),
       ilab.xpos = c(-11,-9), cex = 0.75, ylim = c(-1, 7),
       xlab = "Mean Difference",
       mlab = "RE Model for Neck Disability Index, long-term", psize = 1)
text(-18, 6, "Author(s) and Year", pos = 4, cex = 0.8, font = 2)
text(-12, 6, "VR", pos = 4, cex = 0.8, font = 2)
text(-10, 6, "Control", pos = 4, cex = 0.8, font = 2)
text(10, 6, "Mean difference [95% CI]", pos = 2, cex = 0.8, font = 2)
text(-12.5, 7, "No. of participants", pos = 4, cex = 0.8, font = 2)
dev.off()

### Composite plot for Neck Disability Index
NDI <- rma(data = data %>% filter(outcome == "1. Neck Disability Index, short-term" | outcome == "2. Neck Disability Index, long-term"),
           yi = yi,
           vi = vi,
           method = "DL",
           slab = study,
           digits = 2,
           level = 95)

tiff("./OUTPUT/Figure 2Am. RMA forest plot_outcome1_composite.tiff",
     width = 2500, height = 1200, pointsize = 10, res = 300)
forest(NDI, xlim = c(-18, 10),
       ilab = data %>%
           filter(outcome == "1. Neck Disability Index, short-term" | outcome == "2. Neck Disability Index, long-term") %>%
           select(contains("_n_post")),
       slab = data %>%
           filter(outcome == "1. Neck Disability Index, short-term" | outcome == "2. Neck Disability Index, long-term") %>% 
           pull(study),
       ilab.xpos = c(-11,-9), cex = 0.75, ylim = c(1, 19),
       order = data %>%
           filter(outcome == "1. Neck Disability Index, short-term" | outcome == "2. Neck Disability Index, long-term") %>%
           mutate(order = n():1) %>% 
           pull(order) %>% 
           order(),
       rows = c(3:6, 11:15),
       addfit = FALSE,
       xlab = "Mean Difference",
       mlab = "RE Model for Neck-related disability (Neck Disability Index)", psize = 1)
text(-18, 18, "Author(s) and Year", pos = 4, cex = 0.8, font = 2)
text(-12, 18, "VR", pos = 4, cex = 0.8, font = 2)
text(-10, 18, "Control", pos = 4, cex = 0.8, font = 2)
text(10, 18, "Mean difference [95% CI]", pos = 2, cex = 0.8, font = 2)
text(-12, 19, "No. of participants", pos = 4, cex = 0.8, font = 2)
text(-18, c(16, 7), pos = 4, c("Neck-related disability (Neck Disability Index), short-term",
                               "Neck-related disability (Neck Disability Index), long-term"), cex = 0.8, font = 4)
addpoly(NDI_st, row = 9.5, cex = 0.75,
        mlab = paste0("Random-effect Model (Q = ", formatC(NDI_st$QE, digits=2, format="f"),
                      ", df = ", NDI_st$k - NDI_st$p, ", p = ", formatC(NDI_st$QEp, digits=2, format="f"), 
                      "; I-square = ", formatC(NDI_st$I2, digits=1, format="f"), "%)"))
addpoly(NDI_lt, row = 1.5, cex = 0.75,
        mlab = paste0("Random-effect Model (Q = ", formatC(NDI_lt$QE, digits=2, format="f"),
                      ", df = ", NDI_lt$k - NDI_lt$p, ", p = ", formatC(NDI_lt$QEp, digits=2, format="f"), 
                      "; I-square = ", formatC(NDI_lt$I2, digits=1, format="f"), "%)"))
dev.off()

# 3. Pain, short-term
pain_st <- rma(data = data %>% filter(outcome == "3. Pain, short-term"),
               yi = yi,
               vi = vi,
               method = "DL",
               slab = study,
               digits = 2,
               level = 95)
summary(pain_st)

tiff("./OUTPUT/Figure 2C. RMA forest plot_outcome3.tiff",
     width = 1700, height = 1000, pointsize = 10, res = 300)
forest(pain_st, xlim = c(-10, 6),
       ilab = data %>%
           filter(outcome == "3. Pain, short-term") %>%
           select(contains("_n_post")),
       ilab.xpos = c(-6,-4), cex = 0.75, ylim = c(-1, 9),
       xlab = "Mean Difference",
       mlab = "RE Model for Pain, short-term", psize = 1)
text(-10, 8, "Author(s) and Year", pos = 4, cex = 0.8, font = 2)
text(-6.5, 8, "VR", pos = 4, cex = 0.8, font = 2)
text(-4.9, 8, "Control", pos = 4, cex = 0.8, font = 2)
text(6, 8, "Mean difference [95% CI]", pos = 2, cex = 0.8, font = 2)
text(-6.6, 9, "No. of participants", pos = 4, cex = 0.8, font = 2)
dev.off()

# 4. Pain, long-term
pain_lt <- rma(data = data %>% filter(outcome == "4. Pain, long-term"),
               yi = yi,
               vi = vi,
               method = "DL",
               slab = study,
               digits = 2,
               level = 95)
summary(pain_lt)

tiff("./OUTPUT/Figure 2D. RMA forest plot_outcome4.tiff",
     width = 1700, height = 1000, pointsize = 10, res = 300)
forest(pain_lt, xlim = c(-10, 6),
       ilab = data %>%
           filter(outcome == "4. Pain, long-term") %>%
           select(contains("_n_post")),
       ilab.xpos = c(-6,-4), cex = 0.75, ylim = c(-1, 7),
       xlab = "Mean Difference",
       mlab = "RE Model for Pain, long-term", psize = 1)
text(-10, 6, "Author(s) and Year", pos = 4, cex = 0.8, font = 2)
text(-6.5, 6, "VR", pos = 4, cex = 0.8, font = 2)
text(-4.9, 6, "Control", pos = 4, cex = 0.8, font = 2)
text(6, 6, "Mean difference [95% CI]", pos = 2, cex = 0.8, font = 2)
text(-6.6, 7, "No. of participants", pos = 4, cex = 0.8, font = 2)
dev.off()

### Composite plot for Pain
pain <- rma(data = data %>% filter(outcome == "3. Pain, short-term" | outcome == "4. Pain, long-term"),
            yi = yi,
            vi = vi,
            method = "DL",
            slab = study,
            digits = 2,
            level = 95)

tiff("./OUTPUT/Figure 2Bm. RMA forest plot_outcome2_composite.tiff",
     width = 2500, height = 1300, pointsize = 10, res = 300)
forest(pain, xlim = c(-10, 6),
       ilab = data %>%
           filter(outcome == "3. Pain, short-term" | outcome == "4. Pain, long-term") %>%
           select(contains("_n_post")),
       slab = data %>%
           filter(outcome == "3. Pain, short-term" | outcome == "4. Pain, long-term") %>% 
           pull(study),
       ilab.xpos = c(-6,-4), cex = 0.75, ylim = c(1, 20),
       order = data %>%
           filter(outcome == "3. Pain, short-term" | outcome == "4. Pain, long-term") %>%
           mutate(order = n():1) %>% 
           pull(order) %>% 
           order(),
       rows = c(3:6, 11:16),
       addfit = FALSE,
       xlab = "Mean Difference",
       mlab = "RE Model for Neck pain intensity (visual analog scale)", psize = 1)
text(-10, 19, "Author(s) and Year", pos = 4, cex = 0.8, font = 2)
text(-6.5, 19, "VR", pos = 4, cex = 0.8, font = 2)
text(-4.9, 19, "Control", pos = 4, cex = 0.8, font = 2)
text(6, 19, "Mean difference [95% CI]", pos = 2, cex = 0.8, font = 2)
text(-6.2, 20, "No. of participants", pos = 4, cex = 0.8, font = 2)
text(-10, c(17, 7), pos = 4, c("Neck pain intensity (visual analog scale), short-term",
                               "Neck pain intensity (visual analog scale), long-term"), cex = 0.8, font = 4)
addpoly(pain_st, row = 9.5, cex = 0.75,
        mlab = paste0("Random-effect Model (Q = ", formatC(pain_st$QE, digits=2, format="f"),
                      ", df = ", pain_st$k - pain_st$p, ", p = ", formatC(pain_st$QEp, digits=2, format="f"), 
                      "; I-square = ", formatC(pain_st$I2, digits=1, format="f"), "%)"))
addpoly(pain_lt, row = 1.5, cex = 0.75,
        mlab = paste0("Random-effect Model (Q = ", formatC(pain_lt$QE, digits=2, format="f"),
                      ", df = ", pain_lt$k - pain_lt$p, ", p = ", formatC(pain_lt$QEp, digits=2, format="f"), 
                      "; I-square = ", formatC(pain_lt$I2, digits=1, format="f"), "%)"))
dev.off()

# 5. Kinesiophobia, short-term
Kin_st <- rma(data = data %>% filter(outcome == "5. Kinesiophobia, short-term"),
              yi = yi,
              vi = vi,
              method = "DL",
              slab = study,
              digits = 2,
              level = 95)
summary(Kin_st)

tiff("./OUTPUT/Figure 2E. RMA forest plot_outcome5.tiff",
     width = 1700, height = 1000, pointsize = 10, res = 300)
forest(Kin_st, xlim = c(-18, 14),
       ilab = data %>%
           filter(outcome == "5. Kinesiophobia, short-term") %>%
           select(contains("_n_post")),
       ilab.xpos = c(-10,-8), cex = 0.75, ylim = c(-1, 6),
       xlab = "Mean Difference",
       mlab = "RE Model for Kinesiophobia, short-term", psize = 1)
text(-18, 5, "Author(s) and Year", pos = 4, cex = 0.8, font = 2)
text(-11, 5, "VR", pos = 4, cex = 0.8, font = 2)
text(-9, 5, "Control", pos = 4, cex = 0.8, font = 2)
text(14, 5, "Mean difference [95% CI]", pos = 2, cex = 0.8, font = 2)
text(-11.5, 6, "No. of participants", pos = 4, cex = 0.8, font = 2)
dev.off()

# 6. Kinesiophobia, long-term
Kin_lt <- rma(data = data %>% filter(outcome == "6. Kinesiophobia, long-term"),
              yi = yi,
              vi = vi,
              method = "DL",
              slab = study,
              digits = 2,
              level = 95)
summary(Kin_lt)

tiff("./OUTPUT/Figure 2F. RMA forest plot_outcome6.tiff",
     width = 1700, height = 1000, pointsize = 10, res = 300)
forest(Kin_lt, xlim = c(-21, 12),
       ilab = data %>%
           filter(outcome == "6. Kinesiophobia, long-term") %>%
           select(contains("_n_post")),
       ilab.xpos = c(-13,-11), cex = 0.75, ylim = c(-1, 6),
       xlab = "Mean Difference",
       mlab = "RE Model for Kinesiophobia, long-term", psize = 1)
text(-21, 5, "Author(s) and Year", pos = 4, cex = 0.8, font = 2)
text(-14, 5, "VR", pos = 4, cex = 0.8, font = 2)
text(-12, 5, "Control", pos = 4, cex = 0.8, font = 2)
text(12, 5, "Mean difference [95% CI]", pos = 2, cex = 0.8, font = 2)
text(-11.5, 6, "No. of participants", pos = 4, cex = 0.8, font = 2)
dev.off()

### Composite plot for Kinesiophobia
Kin <- rma(data = data %>% filter(outcome == "5. Kinesiophobia, short-term" | outcome == "6. Kinesiophobia, long-term"),
           yi = yi,
           vi = vi,
           method = "DL",
           slab = study,
           digits = 2,
           level = 95)

tiff("./OUTPUT/Figure 2Cm. RMA forest plot_outcome2_composite.tiff",
     width = 2500, height = 1200, pointsize = 10, res = 300)
forest(Kin, xlim = c(-21, 12),
       ilab = data %>%
           filter(outcome == "5. Kinesiophobia, short-term" | outcome == "6. Kinesiophobia, long-term") %>%
           select(contains("_n_post")),
       slab = data %>%
           filter(outcome == "5. Kinesiophobia, short-term" | outcome == "6. Kinesiophobia, long-term") %>% 
           pull(study),
       ilab.xpos = c(-13,-11), cex = 0.75, ylim = c(1, 16),
       order = data %>%
           filter(outcome == "5. Kinesiophobia, short-term" | outcome == "6. Kinesiophobia, long-term") %>%
           mutate(order = n():1) %>% 
           pull(order) %>% 
           order(),
       rows = c(3:5, 10:12),
       addfit = FALSE,
       xlab = "Mean Difference",
       mlab = "RE Model for Fear of movement (Tampa Scale of Kinesiophobia)", psize = 1)
text(-21, 15, "Author(s) and Year", pos = 4, cex = 0.8, font = 2)
text(-14, 15, "VR", pos = 4, cex = 0.8, font = 2)
text(-12, 15, "Control", pos = 4, cex = 0.8, font = 2)
text(12, 15, "Mean difference [95% CI]", pos = 2, cex = 0.8, font = 2)
text(-14.2, 16, "No. of participants", pos = 4, cex = 0.8, font = 2)
text(-21, c(13, 6), pos = 4, c("Fear of movement (Tampa Scale of Kinesiophobia), short-term",
                               "Fear of movement (Tampa Scale of Kinesiophobia), long-term"), cex = 0.8, font = 4)
addpoly(Kin_st, row = 8.5, cex = 0.75,
        mlab = paste0("Random-effect Model (Q = ", formatC(Kin_st$QE, digits=2, format="f"),
                      ", df = ", Kin_st$k - Kin_st$p, ", p = ", formatC(Kin_st$QEp, digits=2, format="f"), 
                      "; I-square = ", formatC(Kin_st$I2, digits=1, format="f"), "%)"))
addpoly(Kin_lt, row = 1.5, cex = 0.75,
        mlab = paste0("Random-effect Model (Q = ", formatC(Kin_lt$QE, digits=2, format="f"),
                      ", df = ", Kin_lt$k - Kin_lt$p, ", p = ", formatC(Kin_lt$QEp, digits=2, format="f"), 
                      "; I-square = ", formatC(Kin_lt$I2, digits=1, format="f"), "%)"))
dev.off()


########################
# Sensitivity analyses #
########################
# Drop "poor" quality studies
data <- data %>% 
    filter(!study %in% c("Cetin 2022", "Nusser 2021"))

# 1. Neck Disability Index, short-term
NDI_st <- rma(data = data %>% filter(outcome == "1. Neck Disability Index, short-term"),
              yi = yi,
              vi = vi,
              method = "DL",
              slab = study,
              digits = 2,
              level = 95)
summary(NDI_st)

# 2. Neck Disability Index, long-term
NDI_lt <- rma(data = data %>% filter(outcome == "2. Neck Disability Index, long-term"),
              yi = yi,
              vi = vi,
              method = "DL",
              slab = study,
              digits = 2,
              level = 95)
summary(NDI_lt)

### Composite plot for Neck Disability Index
NDI <- rma(data = data %>% filter(outcome == "1. Neck Disability Index, short-term" | outcome == "2. Neck Disability Index, long-term"),
           yi = yi,
           vi = vi,
           method = "DL",
           slab = study,
           digits = 2,
           level = 95)

tiff("./OUTPUT/Figure S1. RMA forest plot_outcome1_composite (sensitivity analysis).tiff",
     width = 2500, height = 1200, pointsize = 10, res = 300)
forest(NDI, xlim = c(-18, 10),
       ilab = data %>%
           filter(outcome == "1. Neck Disability Index, short-term" | outcome == "2. Neck Disability Index, long-term") %>%
           select(contains("_n_post")),
       slab = data %>%
           filter(outcome == "1. Neck Disability Index, short-term" | outcome == "2. Neck Disability Index, long-term") %>% 
           pull(study),
       ilab.xpos = c(-11,-9), cex = 0.75, ylim = c(1, 18),
       order = data %>%
           filter(outcome == "1. Neck Disability Index, short-term" | outcome == "2. Neck Disability Index, long-term") %>%
           mutate(order = n():1) %>% 
           pull(order) %>% 
           order(),
       rows = c(3:6, 11:14),
       addfit = FALSE,
       xlab = "Mean Difference",
       mlab = "RE Model for Neck-related disability (Neck Disability Index)", psize = 1)
text(-18, 17, "Author(s) and Year", pos = 4, cex = 0.8, font = 2)
text(-12, 17, "VR", pos = 4, cex = 0.8, font = 2)
text(-10, 17, "Control", pos = 4, cex = 0.8, font = 2)
text(10, 17, "Mean difference [95% CI]", pos = 2, cex = 0.8, font = 2)
text(-12, 18, "No. of participants", pos = 4, cex = 0.8, font = 2)
text(-18, c(15, 7), pos = 4, c("Neck-related disability (Neck Disability Index), short-term",
                               "Neck-related disability (Neck Disability Index), long-term"), cex = 0.8, font = 4)
addpoly(NDI_st, row = 9.5, cex = 0.75,
        mlab = paste0("Random-effect Model (Q = ", formatC(NDI_st$QE, digits=2, format="f"),
                      ", df = ", NDI_st$k - NDI_st$p, ", p = ", formatC(NDI_st$QEp, digits=2, format="f"), 
                      "; I-square = ", formatC(NDI_st$I2, digits=1, format="f"), "%)"))
addpoly(NDI_lt, row = 1.5, cex = 0.75,
        mlab = paste0("Random-effect Model (Q = ", formatC(NDI_lt$QE, digits=2, format="f"),
                      ", df = ", NDI_lt$k - NDI_lt$p, ", p = ", formatC(NDI_lt$QEp, digits=2, format="f"), 
                      "; I-square = ", formatC(NDI_lt$I2, digits=1, format="f"), "%)"))
dev.off()

# 3. Pain, short-term
pain_st <- rma(data = data %>% filter(outcome == "3. Pain, short-term"),
               yi = yi,
               vi = vi,
               method = "DL",
               slab = study,
               digits = 2,
               level = 95)
summary(pain_st)

# 4. Pain, long-term
pain_lt <- rma(data = data %>% filter(outcome == "4. Pain, long-term"),
               yi = yi,
               vi = vi,
               method = "DL",
               slab = study,
               digits = 2,
               level = 95)
summary(pain_lt)

### Composite plot for Pain
pain <- rma(data = data %>% filter(outcome == "3. Pain, short-term" | outcome == "4. Pain, long-term"),
            yi = yi,
            vi = vi,
            method = "DL",
            slab = study,
            digits = 2,
            level = 95)

tiff("./OUTPUT/Figure S2. RMA forest plot_outcome2_composite (sensitivity analysis).tiff",
     width = 2500, height = 1300, pointsize = 10, res = 300)
forest(pain, xlim = c(-10, 6),
       ilab = data %>%
           filter(outcome == "3. Pain, short-term" | outcome == "4. Pain, long-term") %>%
           select(contains("_n_post")),
       slab = data %>%
           filter(outcome == "3. Pain, short-term" | outcome == "4. Pain, long-term") %>% 
           pull(study),
       ilab.xpos = c(-6,-4), cex = 0.75, ylim = c(1, 18),
       order = data %>%
           filter(outcome == "3. Pain, short-term" | outcome == "4. Pain, long-term") %>%
           mutate(order = n():1) %>% 
           pull(order) %>% 
           order(),
       rows = c(3:6, 11:14),
       addfit = FALSE,
       xlab = "Mean Difference",
       mlab = "RE Model for Neck pain intensity (visual analog scale)", psize = 1)
text(-10, 17, "Author(s) and Year", pos = 4, cex = 0.8, font = 2)
text(-6.5, 17, "VR", pos = 4, cex = 0.8, font = 2)
text(-4.9, 17, "Control", pos = 4, cex = 0.8, font = 2)
text(6, 17, "Mean difference [95% CI]", pos = 2, cex = 0.8, font = 2)
text(-6.2, 18, "No. of participants", pos = 4, cex = 0.8, font = 2)
text(-10, c(15, 7), pos = 4, c("Neck pain intensity (visual analog scale), short-term",
                               "Neck pain intensity (visual analog scale), long-term"), cex = 0.8, font = 4)
addpoly(pain_st, row = 9.5, cex = 0.75,
        mlab = paste0("Random-effect Model (Q = ", formatC(pain_st$QE, digits=2, format="f"),
                      ", df = ", pain_st$k - pain_st$p, ", p = ", formatC(pain_st$QEp, digits=2, format="f"), 
                      "; I-square = ", formatC(pain_st$I2, digits=1, format="f"), "%)"))
addpoly(pain_lt, row = 1.5, cex = 0.75,
        mlab = paste0("Random-effect Model (Q = ", formatC(pain_lt$QE, digits=2, format="f"),
                      ", df = ", pain_lt$k - pain_lt$p, ", p = ", formatC(pain_lt$QEp, digits=2, format="f"), 
                      "; I-square = ", formatC(pain_lt$I2, digits=1, format="f"), "%)"))
dev.off()
