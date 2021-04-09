# Pima Diabetes Datasets       

+ Description       
Pima Indians are known as the most dangerous tribe for diabetes.      
The dataset especially is about to female of Pima Indians.       
I analyzed their features and made a model to classify them whether someone has a diabetes or not.        


## Dataset      

Number of columns : 9 (one of columns is a Output variable(1: positive, 0: negative))        
Number of rows : 768         

![피마기술통계량](https://user-images.githubusercontent.com/62729363/114139578-302fca00-994a-11eb-8052-3f907fd72f74.png)       


# What I did      

## Preprocessing       

+ Imputing Missing value using mean, linear regression.         
It's just a personal project so It didn't include a domain knowledge. only exist data-driven imputation.         

+ Visualization       
I visualize every variable associated with the outcome.      
I found some helpful features for classifying them so I added 4 composite variables in dataframe.        

## Training and validation        

+ Training model : Random Forest       
+ Training accuracy :          
+ Testing accuracy :          

## Pipelining          

Apart from increasing the performance of the model, we have established a preprocessing and learning process pipeline so that when new data other than that dataset is given, it can be inserted directly into the model.          
