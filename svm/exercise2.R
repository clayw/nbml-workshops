
#load the e1071 library
library("e1071")

# This function generates nonlinearly-separable data in two dimensions.
#  a,b,c: the coefficients of x, y, and the bias
#  xmin, xmax: bounds on x values
#  ymin, ymax: bounds on y values
#  npts: number of sample points to generate
sample_nonlinear_data = function(a, b, c, xmin, xmax, ymin, ymax, npts)
{
	#initialize arrays to hold data
	xy = array(0, dim=c(npts, 2))
	cls = array(0, dim=c(npts, 1))
	nGenPnts = 0;
	#create sample data points
	while (nGenPnts < npts) {
		#generate random (x,y) location within bounds
		x = runif(1, min=xmin, max=xmax) #generate uniform random numbers between xmin and xmax
		y = runif(1, min=ymin, max=ymax) #generate uniform random numbers between ymin and ymax
		
		#compute margin of (x,y)
		z = a*x^2 + b*y + c
		
		#compute class of (x,y)
		xyCls = sign(z)
		
		#save point
		xy[nGenPnts+1, 1] = x
		xy[nGenPnts+1, 2] = y
		cls[nGenPnts+1] = xyCls
		nGenPnts = nGenPnts + 1
	}
	#convert locations and classes into a data frame
	ds = data.frame(xy, cls=as.factor(cls))

	return(ds)
}


# Plot a dataset returned from sample_linear_data
#  ds: the dataset
#  title: the plot title
plot_data = function(ds, title="Dataset")
{
	x = ds[ , 1]
	y = ds[ , 2]
	cls = ds$cls
	
	c1 = cls == levels(cls)[1]
	c2 = cls == levels(cls)[2]
	
	plot(x[c1], y[c1], type="p", pch=0, main=title, xlab="x", ylab="y", ylim=c(-2, 2), xlim=c(-2, 2))
	points(x[c2], y[c2], pch=17)	
}


#compute the zero-one loss, the proportion of mis-classified values
zero_one_loss = function(pred_vals, real_vals)
{
   #convert factors to numeric values
   pred_vals = as.numeric(pred_vals)
   real_vals = as.numeric(real_vals)

   loss = 0	
	#each mis-classification adds one to the sum
	for (i in 1:length(pred_vals)) {
		if (pred_vals[i] != real_vals[i]) {
			loss = loss + 1
		}	
	}
	
	#normalize the loss by the # of predicted values, turning it into a proportion
	result = loss/length(pred_vals)
   return(result)
}


#generate some sample nonlinear data
a = -0.5
b = -0.2
c = 0.05
npts = 100
trainingData = sample_nonlinear_data(a, b, c, -2, 2, -2, 2, npts)

#visualize the sample data
plot_data(trainingData, "Nonlinear Training Data")

#train a simple linear SVM on the data
svmModel = svm(cls ~ ., data=trainingData, kernel="radial")
summary(svmModel)

#use the built-in plot.svm function to display decision boundary
#and support vectors
X11() #quartz() on mac, windows() on MS windows - opens a new plot window
plot(svmModel, trainingData, X2 ~ X1, ylim=c(-2, 2), xlim=c(-2, 2))

#create 30 test data points for validation
testData = sample_nonlinear_data(a, b, c, -2, 2, -2, 2, 30)

#predict the output of the test data
predClasses = predict(svmModel, testData)

#compute the prediction performance
perf = zero_one_loss(predClasses, testData$cls)

perfStr = sprintf('SVM Performance: %0.1f%% incorrect predictions on test data', perf*100)
print(perfStr)


