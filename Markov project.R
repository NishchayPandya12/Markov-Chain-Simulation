#Importing dataset
library(readxl)
programdata <- read_excel("~/Desktop/Projects/programdata.xlsx")
View(programdata)

install.packages('markovchain') 
library("markovchain")

#Task 1.1
#Current model----
#Setting up matrix
current_states <- c("UB", "SB", "WS", "NB") #Defining states

current_nstate <- length(current_states)

current_matrix <- matrix(data = rep(0, (current_nstate)^2), 
          nrow = current_nstate, ncol = current_nstate)

colnames(current_matrix) <- current_states 
rownames(current_matrix) <- current_states

current_matrix[1,] = c(0.4, 0.2, 0.2, 0.2) 
current_matrix[2,] = c(0.1, 0.1, 0.3, 0.5) 
current_matrix[3,] = c(0.1, 0.5, 0.1, 0.3) 
current_matrix[4,] = c(0.1, 0.1, 0.1, 0.7)

#Creating Markov chain

current_markov_chain <- new("markovchain", transitionMatrix = current_matrix) 

show(current_markov_chain)

plot(current_markov_chain)

#Updated model without intervention----
#Setting up matrix

updated_states <- c("LU","SU", "SB", "WS", "NB") #Defining states 
updated_nstate <- length(updated_states)

updated_matrix_1 <- matrix(data = rep(0, (updated_nstate)^2), 
               nrow = updated_nstate, ncol = updated_nstate)

colnames(updated_matrix_1) <- updated_states 
rownames(updated_matrix_1) <- updated_states

updated_matrix_1[1,] = c(0.75, 0, 0.1, 0.1, 0.05) 
updated_matrix_1[2,] = c(0.25, 0, 0.25, 0.25, 0.25) 
updated_matrix_1[3,] = c(0, 0.1, 0.1, 0.3, 0.5) 
updated_matrix_1[4,] = c(0, 0.1, 0.5, 0.1, 0.3) 
updated_matrix_1[5,] = c(0, 0.1, 0.1, 0.1, 0.7)

#Creating Markov chain

updated_markov_chain_1 <- new("markovchain", transitionMatrix = updated_matrix_1)

show(updated_markov_chain_1)

plot(updated_markov_chain_1)

#Updated model with intervention----
#Setting up matrix

updated_matrix_2 <- matrix(data = rep(0, (updated_nstate)^2), 
               nrow = updated_nstate, ncol = updated_nstate)

colnames(updated_matrix_2) <- updated_states 
rownames(updated_matrix_2) <- updated_states

updated_matrix_2[1,] = c(0.4, 0, 0.15, 0.25, 0.2) 
updated_matrix_2[2,] = c(0.15, 0, 0.25, 0.3, 0.3) 
updated_matrix_2[3,] = c(0, 0.1, 0.1, 0.3, 0.5) 
updated_matrix_2[4,] = c(0, 0.1, 0.5, 0.1, 0.3) 
updated_matrix_2[5,] = c(0, 0.1, 0.1, 0.1, 0.7)

#Creating Markov chain

updated_markov_chain_2 <- new("markovchain", transitionMatrix = updated_matrix_2) 

show(updated_markov_chain_2)

plot(updated_markov_chain_2)

#Task 1.2---- #Setup----

install.packages("dplyr") 
library("dplyr")

set.seed(123456789)

#Generating simulations through using for loop --- n <- nrow(programdata_csv)

nsim = 1000

length(current_states)

#2023----
sim_current_model_23 <- matrix(NA, nrow = current_nstate, ncol = nsim) 
rownames(sim_current_model_23) <- current_states
#2024----
sim_current_model_24 <- matrix(NA, nrow = current_nstate, ncol = nsim) 
rownames(sim_current_model_24) <- current_states
#2025----
sim_current_model_25 <- matrix(NA, nrow = current_nstate, ncol = nsim) 
rownames(sim_current_model_25) <- current_states
#2026----
sim_current_model_26 <- matrix(NA, nrow = current_nstate, ncol = nsim) 
rownames(sim_current_model_26) <- current_states
#2027----
sim_current_model_27 <- matrix(NA, nrow = current_nstate, ncol = nsim) 
rownames(sim_current_model_27) <- current_states

for (i in c(1:nsim)) {
 #Simulating a 5 year trajectory for the current model
sim_benefit <- sapply(programdata_csv$benefit_22, function(x) rmarkovchain(n = 5, object
                      = current_markov_chain, t0 = x))[c(1,2,3,4,5),] 
                                                       
                       for (j in 1:4) {
                      
                       #Summarise count
                                                                               
                         
                       sim_current_model_23[j, i] <- sum(sim_benefit[1,] == current_states[j]) 
                       sim_current_model_24[j, i] <- sum(sim_benefit[2,] == current_states[j]) 
                       sim_current_model_25[j, i] <- sum(sim_benefit[3,] == current_states[j]) 
                       sim_current_model_26[j, i] <- sum(sim_benefit[4,] == current_states[j]) 
                       sim_current_model_27[j, i] <- sum(sim_benefit[5,] == current_states[j])
                                                                             } }
#2023 distribution----
benefit_distribution_23 = rowMeans(sim_current_model_23)/n

benefit_distribution_23

#2024 distribution----
benefit_count_24 = rowMeans(sim_current_model_24)

benefit_count_24

barplot(benefit_count_24, main = "Benefit count of citizens in 2024 (current model)", 
      col = "navyblue")

benefit_distribution_24 = rowMeans(sim_current_model_24)/n 

benefit_distribution_24

#2025 distribution----
benefit_distribution_25 = rowMeans(sim_current_model_25)/n

benefit_distribution_25

#2026 distribution----
benefit_distribution_26 = rowMeans(sim_current_model_26)/n

benefit_distribution_26

#2027 distribution----
benefit_count_27 = rowMeans(sim_current_model_27)

benefit_count_27
barplot(benefit_count_27, main = "Benefit count of citizens in 2027 (current model)", 
        col = "navyblue")
benefit_distribution_27 = rowMeans(sim_current_model_27)/n 

benefit_distribution_27

#Setting up table with updated data---- 

SU_table <- programdata_csv 

SU_table[SU_table == "UB"] <- "SU" 

SU_table

updated_programdata <- SU_table

updated_programdata$benefit_22[updated_programdata$benefit_21 == "SU" & updated_programdata$benefit_22 == "SU"] <- "LU"

updated_programdata 

count(updated_programdata, benefit_22)

#Simulating distributions using for loop---- 

n <- nrow(updated_programdata) 

set.seed(123)

#2023----
sim_updatedI_model_23_wi <- matrix(NA, nrow = updated_nstate, ncol = nsim) 
rownames(sim_updatedI_model_23_wi) <- updated_states

#2024----
sim_updatedI_model_24_wi <- matrix(NA, nrow = updated_nstate, ncol = nsim) 
rownames(sim_updatedI_model_24_wi) <- updated_states

#2025----
sim_updated_model_25_int <- matrix(NA, nrow = updated_nstate, ncol = nsim) 
rownames(sim_updated_model_25_int) <- updated_states

#2026----
sim_updated_model_26_int <- matrix(NA, nrow = updated_nstate, ncol = nsim) 
rownames(sim_updated_model_26_int) <- updated_states

#2027----
sim_updated_model_27_int <- matrix(NA, nrow = updated_nstate, ncol = nsim) 
rownames(sim_updated_model_27_int) <- updated_states

for (i in c(1:nsim)) {
  #Generating first two trajectories using the updated model without intervention
  sim_updatedI_benefit_wi <- sapply(updated_programdata$benefit_22, function(x) 
    rmarkovchain(n = 2, object = updated_markov_chain_1, t0 = x))[c(1,2),]
  
  #Generating the remaining three trajectories using the updated model with intervention
  sim_updated_benefit_int <- sapply(updated_programdata$benefit_22, function(x) 
    rmarkovchain(n = 3, object = updated_markov_chain_2, t0 = x)) [c(1,2,3),]
  
  
  for (j in 1:5) {
    sim_updatedI_model_23_wi[j, i] <- sum(sim_updatedI_benefit_wi[1,] == updated_states[j])
    sim_updatedI_model_24_wi[j, i] <- sum(sim_updatedI_benefit_wi[2,] == updated_states[j])
    sim_updated_model_25_int[j, i] <- sum(sim_updated_benefit_int[1,] == updated_states[j]) 
    sim_updated_model_26_int[j, i] <- sum(sim_updated_benefit_int[2,] == updated_states[j]) 
    sim_updated_model_27_int[j, i] <- sum(sim_updated_benefit_int[3,] == updated_states[j])
  } }

#2023 distribution----
updated_benefit_distribution_23_wi = rowMeans(sim_updatedI_model_23_wi)/n 

updated_benefit_distribution_23_wi

#2024 distribution----
updated_benefit_count_24_wi = rowMeans(sim_updatedI_model_24_wi) 

updated_benefit_count_24_wi

barplot(updated_benefit_count_24_wi, main = "Benefit count of citizens in 2024 (updated model)",
        col = "darkred")
updated_benefit_distribution_24_wi = rowMeans(sim_updatedI_model_24_wi, na.rm = T)/n 

updated_benefit_distribution_24_wi

#2025 distribution----
updated_benefit_distribution_25_int = rowMeans(sim_updated_model_25_int)/n

updated_benefit_distribution_25_int

#2026 distribution----
updated_benefit_distribution_26_int = rowMeans(sim_updated_model_26_int)/n 

updated_benefit_distribution_26_int
#2027 distribution----
updated_benefit_count_27_int = rowMeans(sim_updated_model_27_int) 

updated_benefit_count_27_int

barplot(updated_benefit_count_27_int, main = "Benefit count of citizens in 2027 (updated model)",
        col = "darkred")
updated_benefit_distribution_27_int = rowMeans(sim_updated_model_27_int)/n 

updated_benefit_distribution_27_int

#Annual costs for current model----
#Set up----

UB <- 50000 
SB <- 60000 
WS <- 20000 
NB <- 0

current_payment <- c(UB, SB, WS, NB) 
#Benefit payment under current program 

set.seed(123)

#Payment2024----
current_cost_2024 <- t(sim_current_model_24) %*% current_payment 

mean(current_cost_2024)

#Payment2027----
current_cost_2027 <- t(sim_current_model_27) %*% current_payment

mean(current_cost_2027)

#Annual cost for updated model----
#Setup----

LU <- 50000
updated_payment <- c(LU, UB, SB, WS, NB)
#Benefit payment under updated model 

AC_1 <- 20000000 #Fixed cost for first two years
AC_2 <- 1000000 #Fixed cost for remaining years

#Payment2024----
sim_updated_cost_2024_wi <- t(sim_updatedI_model_24_wi) %*% updated_payment + AC_1

mean(sim_updated_cost_2024_wi)

#Payment2027----
sim_updated_cost_2027_int <- (t(sim_updated_model_27_int) %*% updated_payment) + AC_2

mean(sim_updated_cost_2027_int)

#Histogram for 2024----
install.packages("ggplot2")
library("ggplot2")
install.packages("qqplotr") 
library("qqplotr")

#Using ggplot and geom_histogram to generate histogram, with an overlay of normal density curve
p0 <- ggplot() +
  geom_histogram(aes(x = current_cost_2024, fill = "navyblue", y = ..density..), bins = 100) + 
  stat_function(fun = dnorm, args = list(mean = mean(current_cost_2024), sd =
  sd(current_cost_2024)), size = 1, col = "orange") +
  geom_histogram(aes(x = sim_updated_cost_2024_wi, fill = "red", y = ..density..), bins =
  100) +
  stat_function(fun = dnorm, args = list(mean = mean(sim_updated_cost_2024_wi), sd =
  sd(sim_updated_cost_2024_wi)), size = 1, col = "darkgreen")

p1 <- p0 + xlab("Dollars ($)") + ylab("Density") + 
  ggtitle("Comparison of cost distributions 2024") + 
  theme(plot.title = element_text(hjust = 0.5))

p2 <- p1+scale_fill_manual(values = c("navyblue", "red"), name = "Model", 
        labels = c("Current", "Updated"))
p2

#Using qqnorm and qqline to assess normality of cost distribution
#Current

qqnorm(current_cost_2024, main = "Normal Q-Q Plot for Costs Distribution in 2024 (Current Model)")

qqline(current_cost_2024, col = 'blue', lwd = 2)

par(xpd=FALSE)

#Updated
qqnorm(sim_updated_cost_2024_wi, main = "Normal Q-Q Plot for Costs Distribution in 2024 (Updated Model)")

qqline(sim_updated_cost_2024_wi, col = 'red', lwd = 2) 

par(xpd=FALSE)

#Finding p-value from the Shapiro test shapiro.test(current_cost_2024) shapiro.test(sim_updated_cost_2024_wi)
#Histogram for 2027----

k0 <- ggplot() +
  geom_histogram(aes(x = current_cost_2027, fill = "navyblue", y = ..density..), bins = 100,
  alpha = 0.6) +
  stat_function(fun = dnorm, args = list(mean = mean(current_cost_2027), sd =
  sd(current_cost_2027)), size = 1, col = "orange") +
  geom_histogram(aes(x = sim_updated_cost_2027_int, fill = "red", y = ..density..), bins =
  100, alpha = 0.6) +
  stat_function(fun = dnorm, args = list(mean = mean(sim_updated_cost_2027_int), sd =
  sd(sim_updated_cost_2027_int)), size = 1, col = "darkgreen") 

k1 <- k0 + xlab("Dollars ($)") + ylab("Density") +
ggtitle("Comparison of cost distributions 2027") + 
theme(plot.title = element_text(hjust = 0.5))
k2 <- k1+scale_fill_manual(values = c("navyblue", "red"), name = "Model", labels = 
      c("Current", "Updated"))

k2

#QQ plot
#current model

qqnorm(current_cost_2027, main = "Normal Q-Q Plot for Costs Distribution in 2027 (Current Model)")

qqline(current_cost_2027, col = 'blue', lwd = 2)

par(xpd=FALSE)

#updated model
qqnorm(sim_updated_cost_2027_int, main = "Normal Q-Q Plot for Costs Distribution in 2027 (Updated Model)")

qqline(sim_updated_cost_2027_int, col = 'red', lwd = 2) 

par(xpd=FALSE)

#Shapiro test shapiro.test(current_cost_2027)
shapiro.test(sim_updated_cost_2027_int)

#Descriptive statistics
#2024 summary(current_cost_2024) sd(current_cost_2024)

summary(sim_updated_cost_2024_wi) sd(sim_updated_cost_2024_wi)

#2027

summary(current_cost_2027) 

sd(current_cost_2027) 

summary(sim_updated_cost_2027_int) 

sd(sim_updated_cost_2027_int)

#Long run cost for current model----
#Fixed distribution----
p = 100
long_run_fixed_current_distribution <- current_markov_chain^p 

long_run_fixed_current_distribution

long_run_current_cost <- (n*long_run_fixed_current_distribution[1,]) %*% current_payment

long_run_current_cost
#Updated----
long_run_updated_distribution <- updated_markov_chain_2^p 

long_run_updated_distribution

long_run_updated_cost <- (n*long_run_updated_distribution[1,]) %*% updated_payment + AC_2

long_run_updated_cost

long_run_cost_vec <- c(long_run_current_cost, long_run_updated_cost) long_run_cost_vec
barplot(long_run_cost_vec,
        main = "Long-run Cost",
        xlab = "Expected long run costs",
        ylab = "Dollars ($)",
        names.arg = c("current model", "updated model"), col = "lightblue",
        border = "red",
        density = c(30, 30),
        space = c(0.8, 0.8))

#Monte carlo simulation
#Setup set.seed(345)

runs <- 10000000

#Monte Carlo simulation for long-run cost under the current model
sims_current <- rnorm(runs,mean=mean(long_run_cost_sim),sd=sd(long_run_cost_sim))

#Integral is calculated with the lower and upper bound being one standard deviation away from the mean
mc_integral_current <- sum(sims_current >= mean(long_run_cost_sim) - 
                       sd(long_run_cost_sim) & sims_current <= mean(long_run_cost_sim) + 
                       sd(long_run_cost_sim))/runs

mc_integral_current

#Monte Carlo simulation for long-run cost under the updated model
sims_updated <- rnorm(runs,mean=mean(updated_long_run_cost),sd=sd(updated_long_run_cost)) 

mc_integral_updated <- sum(sims_updated >= mean(updated_long_run_cost) - 
                           sd(updated_long_run_cost) & sims_updated <= mean(updated_long_run_cost) + 
                             sd(updated_long_run_cost))/runs
mc_integral_updated

data.frame(mc_integral_current, mc_integral_updated)

#Simulation for long run costs #Current model 
set.seed(12345)

sim_long_run_current_model <- matrix(NA, nrow = 4, ncol = nsim) 
rownames(sim_long_run_current_model) <- c("UB", "SB", "WS", "NB")

for (i in c(1:nsim)) {
  #Using a 20-step transition matrix, to ensure that equilibrium situation is attained, whilst minimising code running time
  sim_long_run_benefit <- sapply(programdata_csv$benefit_22, function(x) rmarkovchain(n = 20 , 
                          object = current_markov_chain , t0 = x))[c(1:20),]
  
  for (j in 1:4) {
    
    sim_long_run_current_model[j, i] <- sum(sim_long_run_benefit[20,] == 
                                        rownames(sim_long_run_current_model)[j])
  } }

#Long run distribution for current model----
long_run_benefit_distribution = rowMeans(sim_long_run_current_model)/n 

long_run_benefit_distribution
long_run_cost_sim <- t(sim_long_run_current_model) %*% current_payment 

long_run_cost_sim

mean(long_run_cost_sim)

#Long run cost for updated model---- 
#Simulation----

sim_updated_model_long_run <- matrix(NA, nrow = updated_nstate, ncol = nsim) 
rownames(sim_updated_model_long_run) <- updated_states
for (i in c(1:nsim)) {
  sim_updated_benefit_long_run <- sapply(updated_programdata$benefit_22, function(x)
    rmarkovchain(n = 20, object = updated_markov_chain_2, t0 = x)) [c(1:20),]
  for (j in 1:5) {
    sim_updated_model_long_run[j, i] <- sum(sim_updated_benefit_long_run[20,] == updated_states[j])
    
  } }

#Long run distribution----
updated_benefit_distribution_long_run = rowMeans(sim_updated_model_long_run)/n 

updated_benefit_distribution_long_run
updated_long_run_cost <- t(sim_updated_model_long_run) %*% updated_payment + AC_2 

mean(updated_long_run_cost)

#Histogram----
a0 <- ggplot() +
  geom_histogram(aes(x = long_run_cost_sim, fill = "navyblue", y = ..density..), bins = 100,
  alpha = 0.6) +
  stat_function(fun = dnorm, args = list(mean = mean(long_run_cost_sim), sd =
  sd(long_run_cost_sim)), size = 1, col = "orange") +
  geom_histogram(aes(x = updated_long_run_cost, fill = "red", y = ..density..), bins = 100,
  alpha = 0.6) +
  stat_function(fun = dnorm, args = list(mean = mean(updated_long_run_cost), sd =
  sd(updated_long_run_cost)), size = 1, col = "darkgreen")

a1 <- a0 + xlab("Dollars ($)") + ylab("Density") + 
  ggtitle("Comparison of long run cost distributions") + 
  theme(plot.title = element_text(hjust = 0.5))

a2 <- a1+scale_fill_manual(values = c("navyblue", "red"), name = "Model", 
      labels = c("Current", "Updated"))

a2