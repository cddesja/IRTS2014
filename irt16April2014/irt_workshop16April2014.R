dif <- data.frame(base = c(1.5,.1,NA,.3,.5,1.2),
                  target = c(NA,NA, 2.1,-.2,1,2))
dis <- data.frame(base = c(.8,.5,NA,1.1,1.5,2),
                  target = c(NA,NA,1.1,.8,1.5,1.2))

## Calculate B and A
m0 <- lm(base ~ target, data = dif)
A <- coef(m0)[1]
B <- coef(m0)[2]

## Find scores on linked metric
theta_unlink <- c(-1.4, .7, 2)
theta_link <- B*theta_unlink + A
theta_link
