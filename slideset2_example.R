######################## SLIDE SET 2 - K-NEAREST NEIGHBOR EXAMPLE###############################

# This code generates the figures used in the Slide Set 2
# This code is based on a previous version by Prof. Frédéric Godin (Concordia)

###################GENERATE IN-SAMPLE AND OUT-OF-SAMPLE DATASETS#####################################

set.seed(1986) #Set the random seed (ensures results are reproducible)


nIN = 60 # number of  observations insample
nOOS = 60 # number of  observations out-of-sample

x_min =-2 # min x value
x_max = 2 # max x value
xvalIS = (x_max-x_min)*runif(nIN) + x_min
xvalOOS = (x_max-x_min)*runif(nOOS) + x_min

fIS = xvalIS^3 #true pattern for insample observations
fOOS = xvalOOS^3 #true pattern for out-of-sample observations
yobsIS = fIS + rnorm(nIN) # noisy in sample observations
yobsOOS = fOOS + rnorm(nOOS) # noisy out-of-sample observations

plot(xvalIS,yobsIS,main='In-sample observations',xlab = 'Predictor x', ylab = 'Response y')
plot(xvalOOS,yobsOOS,main='Out-of-sample observations',xlab = 'Predictor x', ylab = 'Response y')



##################################PREDICTION FOR A SINGLE POINT##########################

PredX = -0.5 #x at which the prediction is made

####MAKE THE PREDICTION WITH 4 NN
NNnumber = 4 # the k in kNN
indexNN = which(rank( abs(PredX-xvalIS) ) <= NNnumber) # find the indexs of the kNNs
NNpreds = mean(yobsIS[indexNN])


plot(xvalIS,yobsIS,main=paste('Nearest neighbor prediction for x = ',toString(PredX),sep=""),xlab = 'Predictor x', ylab = 'Response y')
abline(v=PredX, col='red')
lines(PredX,NNpreds,col='red',type="p")
lines(xvalIS[indexNN],yobsIS[indexNN],col='darkgreen',type="p",pch=3)
legend(x=-0.4,y=8.5, legend=c("Observations",  "Nearest Neighbors", "Predicted value"), col=c("black","darkgreen","red"), pch=c(1,3,1) )




###############PREDICTION FOR MANY POINTS AND VARIOUS SMOOTHING PARAMETER VALUES#########


#######FUNCTION PERFORMING PREDICTION WITH NEAREST NEIGHBORS
PredictNearestNeighbors1D = function(yval,xval,xpred,NNnumber){
  #
  # INPUTS:
  #   yval: dependent variable for each data point
  #   xval: independent variable for each data point
  #   xpred: vector of independent variable values at which the forecast is made 
  #   NNnumber: number of nearest  neighbors considered  
  # OUTPUTS:
  #   NNpreds: vector of predictions for each entry of the vector xpred
  #
  
  nout = length(xpred)
  NNpreds = rep(0,nout)
  
  for(jj in 1:nout){
    indexNN = which(rank( abs(xpred[jj]-xval) ) <= NNnumber)
    NNpreds[jj] = mean(yval[indexNN])
  }
  
  return(NNpreds)
  
}


################RUNNING THE NEAREST NEIGHBORS FOR MANY X AND MANY K######################

NNnumbervec1 = c(1, 10)

plot(xvalIS,yobsIS,main='Nearest Neighbors Prediction',xlab = 'Predictor x', ylab = 'Response y')
xpred = seq(from=x_min, to=x_max, length.out = 401)

colors <- rainbow(3) #color rainbow for the plot

for(KK in 1:length(NNnumbervec1)) {
  
  NNpreds = PredictNearestNeighbors1D(yval=yobsIS,xval=xvalIS,xpred=xpred,NNnumber=NNnumbervec1[KK])
  
  lines(xpred,NNpreds, col=colors[KK])
  
}

legendlist = c(paste("K=",toString(NNnumbervec1[1]),sep=""),  paste("K=",toString(NNnumbervec1[2]),sep=""))
legend(y=8,x=-1.5, legend=legendlist, col=colors[1:length(NNnumbersvec1)] , lty=c(1,1))

##########Plot with single K
plot(xvalIS,yobsIS,main='Nearest Neighbors Prediction',xlab = 'Predictor x', ylab = 'Response y')
NNK=3
NNpreds = PredictNearestNeighbors1D(yval=yobsIS,xval=xvalIS,xpred=xpred,NNnumber=NNK)
lines(xpred,NNpreds, col=colors[3])
legendlist = paste("K=",toString(NNK),sep="")
legend(y=8,x=-1.5, legend=legendlist, col=colors[3] , lty=c(1))


################GET THE INSAMPLE/OUT-OF-SAMPLE NEAREST NEIGHBORS MSE FOR MANY K######################

NNnumbervec2 = 1:10
ValidationMSE = rep(0,length(NNnumbervec2))
TrainingMSE = rep(0,length(NNnumbervec2))

for(KK in 1:length(NNnumbervec2)) {
  
  NNpredsIS = PredictNearestNeighbors1D(yval=yobsIS,xval=xvalIS,xpred=xvalIS,NNnumber=NNnumbervec2[KK])
  NNpredsOOS = PredictNearestNeighbors1D(yval=yobsIS,xval=xvalIS,xpred=xvalOOS,NNnumber=NNnumbervec2[KK])
  
  TrainingMSE[KK] = mean( (NNpredsIS - yobsIS)^2 )
  ValidationMSE[KK] = mean( (NNpredsOOS - yobsOOS)^2 )
  
}

plot(NNnumbervec2, TrainingMSE, main='Training vs Validation MSE',ylim = c(0,5), type='n',
     ylab='MSE',xlab='K')
lines(NNnumbervec2, TrainingMSE, col='blue', type="p", pch=1)
lines(NNnumbervec2, ValidationMSE, col='red', type="p", pch=1)
abline(v=which(ValidationMSE==min(ValidationMSE)), col='darkgreen')
legend(y=5,x=6, legend=c('Training MSE','Validation MSE','Optimal K'), col=c('blue','red','darkgreen') , lty=c(0,0,1), pch=c(1,1,NaN))



