#Frameworks & Methods Midterm FALL 2017
#STUDENT NAME: Thibault Joseph Twahirwa


#You are provided with a file "BMIclass.csv" which contains data on
#age, weight, height, body mass index (BMI), and BMI classification (Class).
#You will be asked to load the dataset into R from this file, perform
#some data cleaning, and develop some basic diagnostics on this dataset. 

#This midterm is worth 60 points. Please submit an executable 
#R file with all the required libraries installed. 
#As much as you can, please document your code.

#BEGIN--------------------------------------------------------------#

#Section I: Reading and Loading data from .csv files


#Q1.
#Read the file "bmiclass.csv" 
#Load the contents in an dataset called "bmiclass"

bmiclass = read.csv( file = "BMIclass.csv" ) 

#Q2. View the contents of bmiclass and examine the structure
#get data type of the dataset provide 
typeof(bmiclass) # " list "
sapply(bmiclass, typeof) #typeof each column
head (bmiclass) 
str(bmiclass)
#Q3. Summarize the dataset bmiclass

summary(bmiclass)
lapply(bmiclass, mean ) # explore mean for each column 
lapply (bmiclass, length) # explore size of eah column 
boxplot(bmiclass) # provide a visual summary 
#-----------------------------------------------------------------#

#SECTION II:Data Cleaning


#Q4. Identify data entry inconsistencies/errors in the variable "Class".
#for example, some entries maybe in upper case and some in lower case.
unique(bmiclass$Class)
#==> by using unique () you get to see the summary of variables present in Class column as a result : NORMAL is found to be different to normal 

#Q5. Clean the data entry errors to ensure consistency of text, 
#i.e. all either upper case or all lower case

levels(bmiclass$Class) <- tolower(levels(bmiclass$Class))
#Dealing with Missing values


#Q6. Identify location of missing values i.e. list the rows containing missing values

bmiclass[!complete.cases(bmiclass),]

#Q7. Compute the mean of BMI while retaining missing values

mean(bmiclass$BMI)
#Q8. Impute by replacing all the missing values with a value = 21

bmiclass$BMI[is.na(bmiclass$BMI)] <- 21

# ==> after removing NA 
mean(bmiclass$BMI)
#------------------------------------------------------------------#

#SECTION III: Data Visualization


#Q9. Graph a scatterplot between the variables of bmiclass

pairs(~age+ht_cm+wt_kg+BMI, data = bmiclass, main = " scatterplot between the variables of bmiclass")
# this could be also done by 
library(car)
scatterplotMatrix(~age+ht_cm+wt_kg+BMI, reg.line = lm, smooth = TRUE, spread = FALSE, span=0.5, id.n=0, diagonal = 'density', data = bmiclass)
# other possibility 
scatterplotMatrix(~age+ht_cm+wt_kg+BMI, data = bmiclass, main =  "scatterplot between the variables of bmiclass" )
 
#Q10. Identifying any outliers in the dataset bmiclass

# different approach could be used here 
# 1. Univariate approach : for a given variable, outliers are the observation that lie outside 1.5 * IQR where IQR, the 'Inter Quartile Range' is the
#    difference between 75th and 25 quartiles. 

outlier_values <- boxplot.stats(bmiclass$BMI)$out  # outlier values 

outlier_values

boxplot(bmiclass$BMI, main = " BMI outlier graph ", boxwex = 0.1) 
mtext( paste("Outliers: ", paste( outlier_values, collapse= ",")), cex = 0.6 )

#Q11. Create a bargraph of average BMI by Class
barplot(tapply(bmiclass$BMI, bmiclass$Class, mean ), main = " Average BMI by class ", xlab = " Class " , ylab = " BMI " ) 


#Q12. Create a scatterplot of age and BMI, differentiated by Class
#Provide an interpretation of your output.

library(lattice)
xyplot(BMI~age,
data = bmiclass,
groups = bmiclass$Class,
auto.key = list ( corner = c(1,1)))

# or it could be done this way as well 

library(ggplot2)
library(easyGgplot2)
ggplot2.scatterplot(data = bmiclass, xName = 'age',yName = 'BMI', groupName="Class", size = 3, backgroundColor = "white")

print ( "#Q12: my scattering plot here just show different BMI and age that are represented in each class. I have used colors and legend on graph to make it clear on how my differentiation works " ) 

#Q13. Add a smoothing line to the scatterplot created in Q12.

library(ggplot2)
library(easyGgplot2)
ggplot2.scatterplot(data = bmiclass, xName = 'age',yName = 'BMI', groupName="Class", size = 3, backgroundColor = "white",addRegLine = TRUE, reglineColor = "blue", addConfidenceInterval = TRUE, smoothingMethod = "loess")


#Q14. Create a histogram of BMI, differentiated by Class.
#Provide an interpretation of your output.

ggplot(bmiclass, aes( x = BMI, fill = Class )) + geom_histogram(binwidth = 5, position =  "dodge")

print ( " #Q14 :  The histogram generated, illustrates each class with its associated BMI and its associated count for each BMI number ") 
#Q15. Create a boxplots of BMI by Class.
#Provide an interpretation of your output.


library(ggplot2) 
ggplot(bmiclass, aes( x = Class, y = BMI, fill = Class )) + geom_boxplot()


print (" Q15 :  different class has their own grouped BMI and they are represented by different colors. Looking at the graph generated, there is a lot of outliers once it comes to obese class and the under is less represented." )
#END----------------------------------------------------------------#
