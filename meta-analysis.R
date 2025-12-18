
library(metafor)

df <- `R导入`

df <- escalc(measure = "ROM", 

             m1i = b,              
             sd1i = error_b,       
             n1i = n_b,         
             m2i = a,               
             sd2i = error_a,        
             n2i = n_a,             
             data = df)
res <- rma(yi, vi, data = df, method = "REML")

summary(res)

forest(res)

names(df)[names(df) == "experiment.type"] <- "ExpType"
names(df)[names(df) == "Mutil.Cycle"]     <- "MultiCycle"
names(df)[names(df) == "Region"]          <- "Region"

head(df[, c("ExpType", "MultiCycle", "Region")])
# -----------------------------------------------------------
cat("========== 1. Overall Result ==========\n")
res_total <- rma(yi, vi, data = df, method = "REML")
print(predict(res_total)) 

# -----------------------------------------------------------
cat("\n========== 2. Subgroup: Experiment Type ==========\n")
res_type <- rma(yi, vi, mods = ~ factor(ExpType) - 1, data = df, method = "REML")
summary(res_type)
# -----------------------------------------------------------
cat("\n========== 3. Subgroup: FT Cycle (Multi vs Single) ==========\n")
res_cycle <- rma(yi, vi, mods = ~ factor(MultiCycle) - 1, data = df, method = "REML")
summary(res_cycle)
# -----------------------------------------------------------
cat("\n========== 4. Subgroup: Region ==========\n")
res_region <- rma(yi, vi, mods = ~ factor(Region) - 1, data = df, method = "REML")
summary(res_region)
