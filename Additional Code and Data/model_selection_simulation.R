# model selection function: performs stepwise selection based on the AIC (default)
# or the BIC. The actual data generating model is Y ~ X1
modelSelection<-function(N=100,bic =FALSE){
  if(bic ==TRUE){
    k<-log(N)
  }else{
    k<-2
  }
  X<-replicate(5, rnorm(N))
  Y<-1 + X[,1] * 2 + rnorm(N) # actual model
  data<-data.frame(Y, X)
  model<-lm(Y ~. , data=data)
  
  best<-step(model, 
       direction = "both", 
       scope = formula(~ .),
       k = k,
       trace=FALSE
       )
  f<-deparse(formula(best)) # return selected formula as a string
  return(f)
}

set.seed(100)

aic_reps<-replicate(100, modelSelection())


bic_reps<-replicate(100, modelSelection(bic=TRUE))


sort(table(aic_reps), decreasing=TRUE) # chooses the wrong model ~ 50% of the time

sort(table(bic_reps), decreasing=TRUE) # chooses the wrong model ~ 10% of the time

