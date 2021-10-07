require(Superpower)

#Values from StandingData.xlsx, sheet: Exp2Acc 
design_result <- ANOVA_design(design = "2w*2w*2w",
                              n = 350, #Effective sample size (50*7)
                              mu = c(0.97, 0.94, 0.92, 0.84, 0.97, 0.95, 0.93, 0.87),
                              sd = c(0.03, 0.05, 0.05, 0.10, 0.03, 0.04, 0.04, 0.06),
                              r <- 0.50, 
                              labelnames = c("posture", "sit", "stand", 
                                             "congruency", "con", "incon", 
                                             "condition", "no_switch", "switch"),
                              plot = FALSE)

design_result
plot(design_result)

nsims = 500
power_result_vig_1 <- ANOVA_power(design_result, 
                                  alpha = 0.05, 
                                  nsims = nsims, 
                                  seed = 9951)
power_result_vig_1
#anova_congruency                   100.0     0.78175 (0.70 in paper)
#anova_posture:condition            100.0     0.13004 (0.09 in paper)
#anova_congruency:condition         100.0     0.47422 (0.45 in paper)

#plot_power(design_result, min_n = 10, max_n = 500)

#Reduce effect sizes by increasing SD 2.5 times 
design_result2 <- ANOVA_design(design = "2w*2w*2w",
                              n = 350, #Effective sample size (50*7)
                              mu = c(0.97, 0.94, 0.92, 0.84, 0.97, 0.95, 0.93, 0.87),
                              sd = c(0.03, 0.05, 0.05, 0.10, 0.03, 0.04, 0.04, 0.06)*2.5,
                              r <- 0.50, 
                              labelnames = c("posture", "sit", "stand", 
                                             "congruency", "con", "incon", 
                                             "condition", "no_switch", "switch"),
                              plot = FALSE)

design_result2
plot(design_result2)

power_result_vig_2 <- ANOVA_power(design_result2, 
                                  alpha = 0.05, 
                                  nsims = nsims, 
                                  seed = 9951)
power_result_vig_2
#anova_congruency                   100.0    0.364796
#anova_posture:condition             82.2    0.025899
#anova_congruency:condition         100.0    0.128113

