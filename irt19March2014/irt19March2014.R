# Install and load library
install.packages("difR")
library(difR)
library(MASS)
library(ltm)
library(car)

# Load the verbal data set
data(verbal)

# Examine 
?verbal
head(verbal)

# Excluding the "Anger" variable
verbal <- verbal[colnames(verbal)!="Anger"]
r <- difMH(verbal, group=25, focal.name=1)
r

data(verbal)
# Look just at DoScold
m1 <- glm(S3DoScold ~ Anger + Gender + Anger:Gender, 
          data = verbal, family = "binomial")   
m2 <- update(m1,.~. -Anger:Gender)

# No evidence of non-uniform DIF
1-pchisq(anova(m2,m1)$Dev[2],df=1)
m3 <- update(m2,.~. -Gender)

# Evidence of uniform DIF 
1-pchisq(anova(m3,m2)$Dev[2],df=1)

# Report the change in R Squared
m2.r2 <- cor(fitted(m2),verbal$S3DoScold)^2
m3.r2 <- cor(fitted(m3),verbal$S3DoScold)^2
m2.r2 - m3.r2

# Ordered data
head(Science)

Science.n <- Science

for(i in 1:ncol(Science.n))
  Science.n[,i] <- recode(Science.n[,i],"'strongly agree' = 4; 'agree' = 3; 'disagree' = 2; else = 1",as.factor.result=FALSE)

Science.n$total <- rowSums(Science.n)
Science.n$Comfort.raw <- Science$Comfort

# Examine data set
head(Science.n)

# Randomly create a gender
Science.n$Gender <- rbinom(n=nrow(Science.n),1,prob=.5)
Science.n$Gender <- ifelse(Science.n$Gender == "1", "Female","Male")

head(Science.n)

# Order the Comfort variable
Science.n$Comfort.o <- ordered(Science.n$Comfort.raw)

m1 <- polr(Comfort.o ~ total + Gender + total:Gender, data = Science.n)
m2 <- polr(Comfort.o ~ total + Gender, data = Science.n)

# No non-uniform dif
anova(m1,m2)

m2 <- polr(Comfort.o ~ total + Gender, data = Science.n)
m3 <- polr(Comfort.o ~ total, data = Science.n)

# No uniform dif
anova(m2,m3)

# Nominal data
m1 <- multinom(Comfort.raw ~ total + Gender + total:Gender, data = Science.n)
m2 <- multinom(Comfort.raw ~ total + Gender, data = Science.n)

# No uniform DIF
anova(m1,m2)

m3 <- multinom(Comfort.raw ~ total, data = Science.n)

# No non-uniform DIF
anova(m2,m3)

# Automating this ..

NAMES <- names(Science.n)
for(i in 1:7){
  m1 <- multinom(Science.n[,i] ~ total + Gender + total:Gender, data = Science.n)
  m2 <- multinom(Science.n[,i] ~ total + Gender, data = Science.n)
  m3 <- multinom(Science.n[,i] ~ total, data = Science.n)
  
  cat("\n\n This is for :",NAMES[i], "\n\n")
  
  print(anova(m1,m2))
  print(anova(m2,m3))
}

for(i in 1:7){
  m1 <- polr(ordered(Science.n[,i]) ~ total + Gender + total:Gender, data = Science.n)
  m2 <- polr(ordered(Science.n[,i]) ~ total + Gender, data = Science.n)
  m3 <- polr(ordered(Science.n[,i]) ~ total, data = Science.n)
  
  cat("\n\n This is for :",NAMES[i], "\n\n")
  print(anova(m1,m2))
  print(anova(m2,m3))
}






