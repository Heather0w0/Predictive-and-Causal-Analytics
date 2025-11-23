
install.packages('earth')
library('earth')

setwd('/Users/heather/Desktop') 

#Replace X with the last digit of your student number 
bankData = read.csv('Student Data 6.csv') 


#The function implements K-Fold cross validation for a given model fit. 
#It takes a model fit as an input, and gives an output of the K-Fold measured error.
getBankDataKFoldRMSE = function(testFit){
  set.seed(354987)
  totalFold = 5
  
  #Get the fold number for each row 
  foldNum = floor(runif(nrow(bankData))*totalFold)+1
  
  #Make a place to store the errors for each fold 
  thisModelRMSE = rep(NA,totalFold)
  
  for(thisFold in 1:totalFold){
	#Training Data is everything not in the fold 
    trainingData = subset(bankData,foldNum!=thisFold)
	
	#Validation data is everything in the fold 
    validationData = subset(bankData,foldNum==thisFold)
	
	#Update model fit based on training data 
    thisModel = update(testFit,data=trainingData)
	
	#Get error in validation data 
    thisFit = mean((predict(thisModel,validationData) - validationData$card)^2)^.5
	
	#Store the RMSE 
    thisModelRMSE[thisFold] = thisFit
  }
  
  #Return the error rate 
  return(mean(thisModelRMSE))
}


###################################################
#Train the model for question 1A below.  Remember to not use 'expenditure'.  Use just lm and earth models.
#I provide a few samples below - estimate more.
getBankDataKFoldRMSE(lm(card~reports+age,data=bankData))
getBankDataKFoldRMSE(lm(card~selfemp+owner,data=bankData))
getBankDataKFoldRMSE(lm(card ~ reports + age + income + owner, data = bankData))
getBankDataKFoldRMSE(earth(card ~ reports + age + income + owner, data = bankData, thresh = 0.01) )
getBankDataKFoldRMSE(earth(card ~ reports + age + income + owner, data = bankData, thresh = 0.05))

#Store your final model for 1A below. Delete the line below, and replace it with your own, storing your chosen, final model into the variable 'model1A'
model1A = lm(card~selfemp+owner,data=bankData) 
model1A_lm <- lm(card ~ reports + age + income + owner, data = bankData)
model1A_earth <- earth(card ~ reports + age + income + owner, data = bankData, thresh = 0.01) 
model1A_earth_1 <- earth(card ~ reports + age + income + owner, data = bankData, thresh = 0.05)

###################################################
#Train the model for question 1A below.  Remember to not use 'expenditure'.  Use just lm and earth models.
#I provide a few samples below - estimate more.
getBankDataKFoldRMSE(lm(card~expenditure+age,data=bankData))
getBankDataKFoldRMSE(lm(card~selfemp+owner,data=bankData))
getBankDataKFoldRMSE(lm(card ~ reports + age + income + share + owner + selfemp + dependents +
                          months + majorcards + active + expenditure, data = bankData))
getBankDataKFoldRMSE(earth(card ~ reports + age + income + share + owner + selfemp + dependents +
                             months + majorcards + active + expenditure, data = bankData, thresh = 0.01))
getBankDataKFoldRMSE(earth(card ~ reports + age + income + share + owner + selfemp + dependents +
                             months + majorcards + active + expenditure, data = bankData, thresh = 0.05))

#Store your final model for 1A below. Delete the line below, and replace it with your own, storing your chosen, final model into the variable 'model1B'
model1B = lm(card~expenditure+age,data=bankData) 
model1B_lm <- lm(card ~ reports + age + income + share + owner + selfemp + dependents +
                   months + majorcards + active + expenditure, data = bankData) 
model1B_earth <- earth(card ~ reports + age + income + share + owner + selfemp + dependents +
                         months + majorcards + active + expenditure, data = bankData, thresh = 0.01)
model1B_earth_1 <- earth(card ~ reports + age + income + share + owner + selfemp + dependents +
                           months + majorcards + active + expenditure, data = bankData, thresh = 0.05)







##################################################################
#Run the following code after you've completed training.  This 
#will store you models to an RData file called 'MyModels.Rdata',
#which you will submit through blackboard.  Error messages will 
#print if these models aren't saved correctly - email a TA for 
#help  
##################################################################

model1A$cv.list = NULL
model1A$cv.oof.fit.tab = NULL
model1A$varmod = NULL

model1B$cv.list = NULL
model1B$cv.oof.fit.tab = NULL
model1B$varmod = NULL

isError = FALSE

if(!(class(model1A)=='earth'|class(model1A)=='lm')){
	print('model1A is not an earth or lm model.  Be sure to submit a statistical model here.')
  isError = TRUE}
	
if(!(class(model1B)=='earth'|class(model1B)=='lm')){
	print('model1B is not an earth or lm model.  Be sure to submit a statistical model here.')
  isError = TRUE}


if(class(model1A)=='earth'){
	noExpenditure = !(grepl('expenditure',paste(rownames(model1A$coefficients),collapse=' ')))
}  else {
	noExpenditure = !(grepl('expenditure',paste(names(model1A$coefficients),collapse=' ')))
}

if(!noExpenditure){
	print('Make sure you do not use the expenditure variable in model1A')
  isError = TRUE
}

if(!isError){
  save(model1A, model1B, file = 'MyModels.Rdata')
  print('MyModels.Rdata generated!  Please submit this file via blackboard.')
} else {
  print('Your code does not produce the right output.  Please correct your code, or get in touch with the TA.')
}
