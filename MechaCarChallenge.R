library(dplyr)

Mecha_mpg <- read.csv(file = 'MechaCar_mpg.csv')

lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD, Mecha_mpg)

summary(lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD, Mecha_mpg))




coils <- read.csv(file = 'Suspension_Coil.csv')

total_summary <- coils %>% summarize(Mean=mean(PSI), Median=median(PSI), Variance=var(PSI), SD = sd(PSI))

lot_summary <- coils %>% group_by(Manufacturing_Lot) %>% summarize(Mean=mean(PSI), Median=median(PSI), Variance=var(PSI), SD = sd(PSI))




tval <- t.test(coils$PSI, mu=1500)

lot1 <- t.test(coils$PSI, mu=1500, subset = coils%Manufacturing_Lot == "Lot1")
lot2 <- t.test(coils$PSI, mu=1500, subset = coils%Manufacturing_Lot == "Lot2")
lot3 <- t.test(coils$PSI, mu=1500, subset = coils%Manufacturing_Lot == "Lot3")
