pl.two <- function(theta,a,d){
	exp(a*(theta - d))/(1 + exp(a*(theta - d)))
	} 
thetas <- seq(from = -3, to = 3, by = .1)
d = c(0,0,.5,.5)
a = c(1,.5,.1,2)

irfs <- matrix(nrow = 61, ncol=4)
for(i in 1:4){
	irfs[,i] <- pl.two(theta=thetas,d=d[i],a=a[i])
	}

pdf(file = "~/Documents/HI/Teaching/IRTworkshop/irt19feb2014/fig1.pdf")	
plot(irfs[,1]~thetas,type = "l", col = "purple",ylab = "Probability",xlab = "Theta")
lines(irfs[,2]~thetas,type = "l", col = "blue")
lines(irfs[,3]~thetas,type = "l", col = "black")
lines(irfs[,4]~thetas,type = "l", col = "red")
dev.off()

negdis <- pl.two(thetas,a=-1.5,d=0)

pdf(file = "~/Documents/HI/Teaching/IRTworkshop/irt19feb2014/fig2.pdf")	
plot(negdis~thetas,type = "l", col = "black",ylab = "Probability",xlab = "Theta")
dev.off()

pl.three <- function(theta,a,d,g){
	g + (1 -g)*exp(a*(theta - d))/(1 + exp(a*(theta - d)))
	} 
thetas <- seq(from = -3, to = 3, by = .1)
g = c(0,.25,.5)
d = c(0,0,0)
a = c(1,1,1)

irfs <- matrix(nrow = 61, ncol=3)
for(i in 1:3){
	irfs[,i] <- pl.three(theta=thetas,d=d[i],a=a[i],g = g[i])
	}

pdf(file = "~/Documents/HI/Teaching/IRTworkshop/irt19feb2014/fig3.pdf")	
plot(irfs[,1]~thetas,type = "l", col = "purple",ylab = "Probability",xlab = "Theta")
lines(irfs[,2]~thetas,type = "l", col = "blue")
lines(irfs[,3]~thetas,type = "l", col = "black")
dev.off()
