n <- 100
x <- 63

a <- 1
b <- 1

p <- rbeta(1,a,b)

likefun <- function(x,p,n){
	if (p<0 || p>1){
		return(0)
	} 
	l <- dbinom(x,n,p)
	return(l)
}

priorfun <- function(p,a,b){
	if (p < 0 || p > 1){
		return(0)
      }
	pp = dbeta(p,a,b)
	return(pp)
}

iter <- as.data.frame(t(c(0,p)))
names(iter) <- c("Iteration","Theta")

printgen <- 10
reps <- 10^4
k <- 2

for (rep in 1:reps){
	p_prime <- runif(1,0,1)
	R <- ( likefun(x,p_prime,n) / likefun(x,p,n) ) * ( priorfun(p_prime,a,b) / priorfun(p,a,b) ) 
	u <- runif(1,0,1)
	if (u<R){ #Accept
		p <- p_prime
	} 

#Otherwise if u>=R, reject and p <- p
#Now store the current value of p
	if ( (rep %% printgen) == 0 ) {
		iter[k,] <- t(c(rep,p))
		k <- k+1
	}
}

i <- 1:(k-1)

plot(iter[,1],iter[,2])
hist(iter[,2])
plot(density(iter[,2]))