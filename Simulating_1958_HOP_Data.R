library(MASS)     # library for Multivariate Normal Distribution

set.seed(100)  # set a set of random numbers

# Define the path where the file is saved
path <- "O:\\Demetris\\simulated_1958_HOP_data\\dummyHOPNCDS.csv"

# Read the original table
Table <- read.csv(path,header=TRUE)

# Create a "clean" subset of the table by removing the rows with NAs 
D1 <- na.omit(Table)

# Create a table D2 that includes the columns of D1 with independent variables (continuous variables and gender)
D2 <- D1[-c(1,7:10,12)]

# Find the Variance-Covariance Matrix of the independent variables
VarCovMat <- cov(D2)

# Find the Mean of each independent variable
mu <- colMeans(D2)

# Produce a sample of n random independent variables that follow the Multivariate Normal Distribution
n <- 1000  # Define the size of simulated data
IndepVar <- data.frame(mvrnorm(n, mu, Sigma = matrix(VarCovMat, ncol(D2), ncol(D2))))

MP <- as.data.frame(table(D1$GENDER)[1])/(as.data.frame(table(D1$GENDER)[1])+as.data.frame(table(D1$GENDER)[2]))  # Calculate the percentage of males in the original "clean" data
MP2 <- round(MP*n)
GENDER_sorted <- sort(IndepVar$GENDER, decreasing = FALSE)
Th <- GENDER_sorted[MP2[1,1]]

IndepVar$GENDER <- ifelse(IndepVar$GENDER > Th, 1, 0)  # replace the continuous values of gender with categorical values (0 and 1) but keep the same proportions as in the original "clean" data 

# Do logistic regression for each discrete variable
s1 <- glm(D1$DIS_DIAB ~ D1$LAB_TSC + D1$LAB_TRIG + D1$LAB_HDL + D1$LAB_GLUC_ADJUSTED  + D1$PM_BMI_CONTINUOUS + D1$GENDER, family=binomial)
v1 <- s1$coefficients[1] + s1$coefficients[2]*IndepVar$LAB_TSC + s1$coefficients[3]*IndepVar$LAB_TRIG + s1$coefficients[4]*IndepVar$LAB_HDL + s1$coefficients[5]*IndepVar$LAB_GLUC_ADJUSTED  + s1$coefficients[6]*IndepVar$PM_BMI_CONTINUOUS + s1$coefficients[7]*IndepVar$GENDER
fp1 <- exp(v1)/(1+exp(v1))  # calculate the log odds 
DIS_DIAB <- rbinom(n,1,fp1)

s2 <- glm(D1$DIS_CVA ~ D1$LAB_TSC + D1$LAB_TRIG + D1$LAB_HDL + D1$LAB_GLUC_ADJUSTED  + D1$PM_BMI_CONTINUOUS + D1$GENDER, family=binomial)
v2 <- s2$coefficients[1] + s2$coefficients[2]*IndepVar$LAB_TSC + s2$coefficients[3]*IndepVar$LAB_TRIG + s2$coefficients[4]*IndepVar$LAB_HDL + s2$coefficients[5]*IndepVar$LAB_GLUC_ADJUSTED  + s2$coefficients[6]*IndepVar$PM_BMI_CONTINUOUS + s2$coefficients[7]*IndepVar$GENDER
fp2 <- exp(v2)/(1+exp(v2))  # calculate the log odds
DIS_CVA <- rbinom(n,1,fp2)

s3 <- glm(D1$DIS_AMI ~ D1$LAB_TSC + D1$LAB_TRIG + D1$LAB_HDL + D1$LAB_GLUC_ADJUSTED  + D1$PM_BMI_CONTINUOUS + D1$GENDER, family=binomial)
v3 <- s3$coefficients[1] + s3$coefficients[2]*IndepVar$LAB_TSC + s3$coefficients[3]*IndepVar$LAB_TRIG + s3$coefficients[4]*IndepVar$LAB_HDL + s3$coefficients[5]*IndepVar$LAB_GLUC_ADJUSTED  + s3$coefficients[6]*IndepVar$PM_BMI_CONTINUOUS + s3$coefficients[7]*IndepVar$GENDER
fp3 <- exp(v3)/(1+exp(v3))  # calculate the log odds
DIS_AMI <- rbinom(n,1,fp3)

PM_BMI_CATERGORIAL <- ifelse(IndepVar$PM_BMI_CONTINUOUS < 25, 1, ifelse(IndepVar$PM_BMI_CONTINUOUS >30, 3, 2))

# Combine the continuous and the discrete variables 
synthetic_data <- cbind(IndepVar,DIS_CVA,DIS_DIAB,DIS_AMI,PM_BMI_CATERGORIAL)

# Save the simulated data as a .csv file in the working directory
setwd("O:\\Demetris\\simulated_1958_HOP_data")
write.csv(synthetic_data,"Simulated_Data.csv")
