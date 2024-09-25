#--------------------------------------------
# FUNCTIONS IN THIS FILE
#
# myEstimate.GARCH <- function(stpars,loglik,score,e,mthd,ctrl)
# myLogLik.GARCH<- function(pars,e)
# myLogLik.garch <- function(pars,e,flag)
# myScore.GARCH <- function(pars,e)
# myTest.TV.robust <- function(pars,e)
# myTest.TV.TR2 <- function(pars,eps)
# myTest.TVfix.TR2 <- function(pars,psi2)
# dh.dt1.H0 <- function(pars,e)
# dg.dt2.H0 <- function(e)
#--------------------------------------------


myEstimate.GARCH <- function(stpars,loglik,score,e,mthd,ctrl){
# model:	GARCH
# purpose:	calls "optim" optimizer to maximize "loglik" function by varying parameters in first argument place
# input :	stpars -- starting values for all parameters to be estimated
#			loglik -- name of the function whose value is maximized
#			score  -- name of the function that calculates the score
#			... the following 1 argument is input for loglik and score function:
#			e -- returns
#			... arguments for optim function...
#			methd -- optimizing method
#			ctrl  -- list of all kinds of control and display options
# output:	prints estimates, stderrors, final loglik value
#			return value -- final parameter estimates
	#if (mthd=="BFGS")
	#score = NULL
	tmp <- optim(stpars,loglik,score,e,method=mthd,lower=-Inf,upper=Inf,control=ctrl,hessian=TRUE)
	
	return(c(tmp$convergence,tmp$par))
}


myLogLik.GARCH<- function(pars,e){
# model:	GARCH
# purpose:	call myLogLik.gjr with flag=0
# input:	pars -- GARCH parameter(s)
#			e 	 -- returns
# output:	ll   -- sum of loglik values over t=1...T
	ll <- myLogLik.garch(pars,e,0)
	return(ll)
}


myLogLik.garch <- function(pars,e,flag){
# model: GARCH, with target on unconditional level = 1
# purpose: compute loglikelihood (and related components) value
# input: pars = full parameter vector
#        e = returns
#        flag = return value indicator
#               0 : sum of loglikelihood(t) values over t=1...T
#               1 : conditional variances, T rows
	T <- NROW(e)
	h <- matrix(1,T,1)
	par.o <- pars[1]
	par.a <- pars[2]
	par.b <- pars[3]
	if (par.o<0) ll <- -100000
	else if (par.a<0) ll <- -100000
	else if (par.b<0) ll <- -100000
	else if ((par.a+par.b)>=1) ll <- -100000
	else {
		h <- matrix(0,T,1) #Tx1
		llt <- matrix(0,T,1) # Tx1
		for(t in seq(from=1,to=T)){
			if (t==1) h[t] <- sum(e^2)/T
			else {
				h[t] <- par.o + par.a*(e[t-1])^2 + par.b*h[t-1]
				llt[t] <- -0.5*log(h[t])-0.5*(e[t]^2)/h[t]
			}
		}
		ll <- sum(llt)
	}
	if(flag==0) return(ll)
	if(flag==1) return(h)  #Tx1
}


myScore.GARCH <- function(pars,e){
	T <- NROW(e)
	dhdt1 <- dh.dt1.H0(pars,e) # Tx3
	h <- myLogLik.garch(pars,e,1) # Tx1
	dldt1 <- matrix(0,nrow=T,ncol=3)
	for (t in seq(from=1,to=T)){
		dldt1[t,] <- -0.5*(1/h[t])*dhdt1[t,]*(1-(e[t]^2)/h[t])
	}
	return(colSums(dldt1))  # 1x3
}


myTest.TV.robust <- function(pars,e){
# robust version of the test
	k <- 0 # in case we want to drop some observations from the start
	if (k>0) e <- e[-(1:k)]  #***
	T <- NROW(e)
	# 1. Estimate GARCH, get squared standardised residuals minus one: et^2/ht - 1
	h <- myLogLik.garch(pars,e,1)
	ssrm1 <- (e^2)/h-1
	# 2. Regress 1,t/T,(t/T)^2,(t/T)^3 on 1/ht*dhdt1.Ho, get residuals wt
	dhdt1 <- dh.dt1.H0(pars,e)  # Tx3
	# X = Tx3 matrix, each row = 1/ht * dh/dtheta1
	X <- matrix(0,nrow=T,ncol=3) # Tx3
	for (t in seq(from=1,to=T)){
		X[t,] <- (1/h[t])*dhdt1[t,] # Tx3
	}
	# v = Tx4 matrix, with columns = 1 : t/T : (t/T)^2 : (t/T)^3 , t=1...T
	v <- dg.dt2.H0(e)  # Tx4
	XX.i <- solve(t(X)%*%X,tol=1e-21) # 3x3
	# residuals from regressing each column of v on X
	r <- v-X%*%XX.i%*%t(X)%*%v	# Tx4
	# 3. Regress vector of 1's on (st.res^2-1)*rt, get RSS
	Z <- matrix(0,nrow=T,ncol=4) # Tx4
	for (i in seq(from=1,to=4)){
		Z[,i] <- ssrm1*r[,i]
	}
	ZZ.i <- solve(t(Z)%*%Z) # 4x4
	res <- matrix(0,nrow=T,ncol=1) # Tx1
	ones <- matrix(1,nrow=T,ncol=1) # Tx1
	res <- ones-Z%*%ZZ.i%*%t(Z)%*%ones # Tx1
	# compute SSR = sum of squared residuals from previous regression
	SSR <- t(res)%*%res
	TestStat <- T-SSR
	pval <- pchisq(TestStat,4,lower.tail=FALSE)
	return(c(TestStat,pval))	
}


myTest.TV.TR2 <- function(pars,eps){
# TR2 and F -forms of the LM test
# Ho: et = sqrt(ht)zt (i.e. standard GARCH)
# H1: et = sqrt(ht)sqrt(gt)zt (i.e. TVGARCH, test if gt=1)
	T <- NROW(eps)
	# 1. Estimate GARCH, get squared standardised residuals minus one, compute SSR0
	h <- myLogLik.garch(pars,eps,1)	# Tx1
	ssrm1 <- (eps^2)/h-1				# Tx1
	SSR0 <- sum(ssrm1^2)
	# 2. Regress ssrm1 on 1/ht*dhdt1.Ho and dgdt2
	dhdt1 <- dh.dt1.H0(pars,eps)  # Tx3
	dgdt2 <- dg.dt2.H0(eps) 		# Tx4
	X <- matrix(0,nrow=T,ncol=7) # Tx7
	for (t in seq(from=1,to=T)){
		X[t,1:3] <- (1/h[t])*dhdt1[t,]	# Tx3
		X[t,4:7] <- dgdt2[t,]			# Tx4
	} 
	XX.i <- solve((t(X)%*%X)) # 7x7
	
	# if inversion really does not work...
	DoThisOne = 0
	if (DoThisOne){
		XX = t(X)%*%X
		A = XX[1:3,1:3]
		B = XX[1:3,4:7]
		C = XX[4:7,1:3]
		D = XX[4:7,4:7]
		A.i = solve(A)
		D.i = solve(D)
		M.i = solve(D-C%*%A.i%*%B)
		XX.i.11 = A.i+A.i%*%B%*%M.i%*%C%*%A.i
		XX.i.12 = (-1)*A.i%*%B%*%M.i
		XX.i.21 = (-1)*M.i%*%C%*%A.i
		XX.i.22 = M.i
		XX.i = matrix(0,7,7)
		XX.i[1:3,1:3] = XX.i.11
		XX.i[1:3,4:7] = XX.i.12
		XX.i[4:7,1:3] = XX.i.21
		XX.i[4:7,4:7] = XX.i.22
	}
	res <- ssrm1-X%*%XX.i%*%t(X)%*%ssrm1	# Tx1
	SSR1 <- sum(res^2)
	# 3. Compute test statistic
	LM <- T*(SSR0-SSR1)/SSR0
	F <- ((SSR0-SSR1)/4)/(SSR1/(T-7))
	pvalLM <- pchisq(LM,4,lower.tail=FALSE)
	pvalF <- pf(F,4,T-7,lower.tail=FALSE)
	return(c(LM,pvalLM,F,pvalF))
}


myTest.TVfix.TR2 <- function(pars,psi2){
# TR2 and F -forms of the LM test
# Ho: et = zt (i.e. no GARCH) or et=sqrt(ht) with known parameters
# H1: et = sqrt(gt)zt (i.e. TV) or et=sqrt(ht)sqrt(gt)zt with known ht pars -- test if gt=delta0)
# pars = estimate of delta0, psi2 = eps2/hbar	
	T <- NROW(psi2)
	# 1. Get squared standardised residuals minus one, compute SSR0
	ssrm1 <- (1/pars)*(psi2)-1				# Tx1
	SSR0 <- sum(ssrm1^2)
	# 2. Regress ssrm1 on 1/gt*dgdt2.Ho (here t2=delta) 
	dgdt2 <- dg.dt2.H0(psi2)		# Tx4
	X <- matrix(0,nrow=T,ncol=4) 	# Tx4
	for (t in seq(from=1,to=T)){
		X[t,1:4] <- (1/pars)*dgdt2[t,]	# Tx4
	} 
	XX.i <- solve(t(X)%*%X) # 4x4
	res <- ssrm1-X%*%XX.i%*%t(X)%*%ssrm1	# Tx1
	SSR1 <- sum(res^2)
	# 3. Compute test statistic
	LM <- T*(SSR0-SSR1)/SSR0
	F <- ((SSR0-SSR1)/3)/(SSR1/(T-4))
	pvalLM <- pchisq(LM,3,lower.tail=FALSE)
	pvalF <- pf(F,3,T-4,lower.tail=FALSE)
	return(c(LM,pvalLM,F,pvalF))
}


dh.dt1.H0 <- function(pars,e){
# purpose:	recursively calculates the partial derivative dh/dTheta1
#           in the linearized model, 
#			Theta1=GARCH parameters
# output:	Tx3 matrix, each row = dh/do, dh/da, dh/db	
	T <- NROW(e)
	par.b <- pars[3]
	e_0 <- 0
	h_0 <- (t(e)%*%e)/T
	h <- myLogLik.garch(pars,e,1) # Tx1
	dh_0 <- matrix(0,nrow=1,ncol=3)
	dh <- matrix(0,nrow=T,ncol=3)
	for (t in seq(from=1,to=T)){
		if (t==1) dh[t,] <- c(1,e_0^2,h_0) + par.b*dh_0
		else dh[t,] <- c(1,(e[t-1])^2,h[t-1]) + par.b*dh[t-1,]
	}
	return(dh) # Tx3
}


dg.dt2.H0 <- function(e){
# purpose:	recursively calculates the partial derivative dg/dTheta2
#           in the linearized model, 
#			Theta2=parameters from the linearized TV component, d0,d1,d2,d3
# output:	Tx4 matrix, each row = dg/dd0, dg/dd1, dg/dd2, dg/dd3	
	T <- NROW(e)
	dg <- matrix(0,nrow=T,ncol=4)
	dg[,1] <- 1
	trend1 <- seq(from=1,to=T)/T
	trend2 <- trend1^2
	trend3 <- trend1^3
	dg[,2] <- trend1
	dg[,3] <- trend2
	dg[,4] <- trend3
	return(dg) # Tx4
}


