# In this exercise, we'll be using multi-class
# classification and the e1071 tune() function
# to classify images of handwritten digits.

library("e1071")

#Loads the semeion dataset.
load_semeion = function(filePath)
{
   #get the raw data into a data.frame object
   rawData = read.table(filePath, header=FALSE, sep=" ")
   
   #ensure the pixels are represented as a matrix
   imgs = as.matrix(rawData[ , 1:256])
   
   #convert the digits, which are in a one-of-k representation,
   #into an integer representation from 0-9
   dclass = as.matrix(rawData[ , 257:266])   
   digs = (dclass %*% 1:10) - 1

   #create a new data frame from the preprocessed data, making
   #sure that the digits are converted into factors
   ds = data.frame(imgs, classes=as.factor(digs))

   return(ds)
}

#split the dataset into a training and test set, guided
#by the specified proportion
split_dataset = function(ds, trainProportion)
{
   numRows = nrow(ds)

   #create a random sampling of indicies
   sindx = sample(1:numRows)
   
   #compute # of training samples
   numTrain = round(numRows*trainProportion)

   #use the sample indicies to index into the dataset,
   #creating training and test sets      
   dsTrain = ds[sindx[1:numTrain], ]
   dsTest = ds[sindx[(numTrain+1):numRows], ]

   #return a list containing training and test sets
   sds = list(train=dsTrain, test=dsTest)
   return(sds)
}

#display a single row of the data.frame created from load_semeion
display_row = function(ds, rowNum)
{
   x = y = 1:16

   #get pixel values, convert from binary to numeric
   zn = as.numeric(ds[rowNum, 1:256]) 

   #reshape pixel vector into 16x16 matrix
   z = matrix(zn, 16, 16) 
   z = z[, ncol(z):1] #flip for display

   #get associated digit
   digit = as.numeric(ds$classes[rowNum]) - 1 

   #make a plot of the handwritten digit
   image(x, y, z, main=sprintf('Row %d | Digit=%d', rowNum, digit))
}


#compute the zero-one loss, the proportion of mis-classified digits
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

#Load the semeion dataset
data = load_semeion("semeion.data")

#For fun, display a row
display_row(data, 20)

#Split the dataset, use 70% for training, 30% for testing
splitData = split_dataset(data, 0.70)
trainData = splitData$train
testData = splitData$test

#Use the tune.svm() function to find the best parameters
# Scaling is turned off because the input data is binary
print("Training and finding the best SVM...")
tval = tune('svm', train.x=classes ~ ., data=trainData, scale=FALSE)

#Get the best model and predict on the test data
bestModel = tval$best.model
summary(bestModel)
testPred = predict(bestModel, testData)

#Compute the loss on the test data
loss = zero_one_loss(testPred, testData$classes)
lstr = sprintf("Zero-one loss: %f", loss)
print(lstr)
