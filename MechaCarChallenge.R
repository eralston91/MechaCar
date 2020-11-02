library(dplyr)
MechaCar_mpg <- read.csv(file='MechaCar_mpg.csv',check.names=F,stringsAsFactors = F)
lm(formula = mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD, data = MechaCar_mpg)
summary(lm(formula = mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD, data = MechaCar_mpg))
suspension_coil <- read.csv(file='Suspension_Coil.csv', check.names=F, stringsAsFactors = F)
total_summary <- suspension_coil %>% summarize(Mean=mean(PSI),Median=median(PSI),Variance=var(PSI),SD=sd(PSI),.groups = 'keep') #create summary table with multiple columns
lot_summary <- suspension_coil %>% group_by(Manufacturing_Lot) %>% summarize(Mean=mean(PSI),Median=median(PSI),Variance=var(PSI),SD=sd(PSI),.groups = 'keep')
t.test(x=suspension_coil$PSI,mu=1500)
t.test(x=suspension_coil$PSI,mu=1500,subset(suspension_coil,Manufacturing_Lot == "Lot1",select = PSI,drop = FALSE))
t.test(x=suspension_coil$PSI,mu=1500,subset(suspension_coil,Manufacturing_Lot == "Lot2",select = PSI,drop = FALSE))
t.test(x=suspension_coil$PSI,mu=1500,subset(suspension_coil,Manufacturing_Lot == "Lot3",select = PSI,drop = FALSE))



