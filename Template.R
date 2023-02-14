####################   PARAMETRIC ANOVA Template   ##########################

# Read Data
results <- read.table("yields.txt",header=T)
results <- read.table(file.choose(),header=T)
names(results)
results

# Data Preparation: Make Data Ready 
mydata = stack(results)
names(mydata)=c("yield","soil")
dim(mydata)
fsoil= factor(mydata$soil)

# Data Visualization
boxplot(mydata$yield ~ fsoil, y="log", col="gray", xlab="Soil Type",ylab="Yield")

# compute The Mean and Variance for each Group
tapply(mydata$yield, fsoil, mean) 
tapply(mydata$yield, fsoil, var)
 

# Testa the Equity Of Variance
fligner.test(mydata$yield,fsoil)


# Do The ANOVA
An1 <- aov(mydata$yield ~ fsoil)
summary(An1)


# Perform Post-HOC for ANOVA
TukeyHSD(An1)
plot(TukeyHSD(An1))


# Draw Mean Plot
(plot.design(mydata$yield~fsoil))


#How do I type a tilde in Windows 10  ==== under Esc bottom

