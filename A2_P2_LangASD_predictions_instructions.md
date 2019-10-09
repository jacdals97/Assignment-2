Welcome to the second exciting part of the Language Development in ASD exercise
-------------------------------------------------------------------------------

In this exercise we will delve more in depth with different practices of
model comparison and model selection, by first evaluating your models
from last time against some new data. Does the model generalize well?
Then we will learn to do better by cross-validating models and
systematically compare them.

The questions to be answered (in a separate document) are: 1- Discuss
the differences in performance of your model in training and testing
data 2- Which individual differences should be included in a model that
maximizes your ability to explain/predict new data? 3- Predict a new
kid’s performance (Bernie) and discuss it against expected performance
of the two groups

Learning objectives
-------------------

-   Critically appraise the predictive framework (contrasted to the
    explanatory framework)
-   Learn the basics of machine learning workflows: training/testing,
    cross-validation, feature selections

Let’s go
--------

N.B. There are several datasets for this exercise, so pay attention to
which one you are using!

1.  The (training) dataset from last time (the awesome one you produced
    :-) ).
2.  The (test) datasets on which you can test the models from last time:

-   Demographic and clinical data:
    <a href="https://www.dropbox.com/s/ra99bdvm6fzay3g/demo_test.csv?dl=1" class="uri">https://www.dropbox.com/s/ra99bdvm6fzay3g/demo_test.csv?dl=1</a>
-   Utterance Length data:
    <a href="https://www.dropbox.com/s/uxtqqzl18nwxowq/LU_test.csv?dl=1" class="uri">https://www.dropbox.com/s/uxtqqzl18nwxowq/LU_test.csv?dl=1</a>
-   Word data:
    <a href="https://www.dropbox.com/s/1ces4hv8kh0stov/token_test.csv?dl=1" class="uri">https://www.dropbox.com/s/1ces4hv8kh0stov/token_test.csv?dl=1</a>

### Exercise 1) Testing model performance

How did your models from last time perform? In this exercise you have to
compare the results on the training data () and on the test data. Report
both of them. Compare them. Discuss why they are different.

-   recreate the models you chose last time (just write the model code
    again and apply it to your training data (from the first
    assignment))
-   calculate performance of the model on the training data: root mean
    square error is a good measure. (Tip: google the function rmse())
-   create the test dataset (apply the code from assignment 1 to clean
    up the 3 test datasets)
-   test the performance of the models on the test data (Tips: google
    the functions “predict()”)
-   optional: predictions are never certain, can you identify the
    uncertainty of the predictions? (e.g. google predictinterval())

### Exercise 2) Model Selection via Cross-validation (N.B: ChildMLU!)

One way to reduce bad surprises when testing a model on new data is to
train the model via cross-validation.

In this exercise you have to use cross-validation to calculate the
predictive error of your models and use this predictive error to select
the best possible model.

-   Use cross-validation to compare your model from last week with the
    basic model (Child MLU as a function of Time and Diagnosis, and
    don’t forget the random effects!)
-   (Tips): google the function “createFolds”; loop through each fold,
    train both models on the other folds and test them on the fold)

-   Now try to find the best possible predictive model of ChildMLU, that
    is, the one that produces the best cross-validated results.

-   Bonus Question 1: What is the effect of changing the number of
    folds? Can you plot RMSE as a function of number of folds?
-   Bonus Question 2: compare the cross-validated predictive error
    against the actual predictive error on the test data

``` r
#Create the basic model of ChildMLU as a function of Time and Diagnosis (don't forget the random effects!).

basic <- lmer(CHI_MLU ~ Visit*Diagnosis + (1+Visit|Child.ID), clean_train, REML = FALSE)

#- Make a cross-validated version of the model. (Tips: google the function "createFolds";  loop through each fold, train a model on the other folds and test it on the fold)

#setting seed for replication
set.seed(12)

folds1 <- fold(clean_train, 5, cat_col = "Diagnosis", id_col = "Child.ID") #creating 5 folds using groupdata2 package

#Setting up loop for simple model
result <- c()

loop <- {for (i in unique(folds1[[".folds"]])){
  test <- subset(folds1, .folds == i)
  train <- subset(folds1, .folds != i)
  model <- lmer(CHI_MLU ~ Visit*Diagnosis + (1+Visit|Diagnosis), train, REML = F)
  result[i] <- rmse(test$CHI_MLU, predict(model,test))
  print(result[i])
}
  print(c("result" = mean(result)))
}
```

    ## boundary (singular) fit: see ?isSingular

    ##         1 
    ## 0.7153401

    ## boundary (singular) fit: see ?isSingular

    ##         5 
    ## 0.8450805

    ## boundary (singular) fit: see ?isSingular

    ##         3 
    ## 0.6081902

    ## boundary (singular) fit: see ?isSingular

    ##         4 
    ## 0.7688448

    ## boundary (singular) fit: see ?isSingular

    ##         2 
    ## 0.8708951 
    ##    result 
    ## 0.7616702

``` r
#Setting up loop for our model
result <- c()
 
loop <- {for (i in unique(folds1[[".folds"]])){
  test <- subset(folds1, .folds == i)
  train <- subset(folds1, .folds != i)
  model <- lmer(CHI_MLU ~ Visit*Diagnosis + verbalIQ1 + MOT_MLU + (1+Visit|Child.ID), train, REML =F,)
  result[i] <- rmse(test$CHI_MLU, predict(model,test, allow.new.levels = T))
  print(result[i])
}
  print(c("result" = mean(result)))
}
```

    ##         1 
    ## 0.4495225 
    ##         5 
    ## 0.6171194 
    ##         3 
    ## 0.5962119

    ## boundary (singular) fit: see ?isSingular

    ##         4 
    ## 0.5670975

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl =
    ## control$checkConv, : Model failed to converge with max|grad| = 0.00489159
    ## (tol = 0.002, component 1)

    ##         2 
    ## 0.5452881 
    ##    result 
    ## 0.5550479

``` r
#- Report the results and comment on them. 



#- Now try to find the best possible predictive model of ChildMLU, that is, the one that produces the best cross-validated results.

#Since AIC has been our way of determining our models in part 1 of the assignment and since AIC is a good approximation of out-of-sample errors, 
#we compared our our dredged model and our chosen model using cross-validation to see if their AIC values predicted their mean RMSE for cross validation 

#The dredged model used
result <- c()
 
loop <- {for (i in unique(folds1[[".folds"]])){
  test <- subset(folds1, .folds == i)
  train <- subset(folds1, .folds != i)
  model <- lmer(CHI_MLU ~ Visit*Diagnosis*verbalIQ1 + MOT_MLU + verbalIQ1:MOT_MLU + (1+Visit|Child.ID), train, REML =F, control = lmerControl(
  optimizer = "nloptwrap",
  calc.derivs = F,
  optCtrl = list(
    ftol_abs = 1e-10,
    xtol_abs = 1e-10,
    maxeval = 10000
  )))
  result[i] <- rmse(test$CHI_MLU, predict(model,test, allow.new.levels = T))
  print(result[i])
}
  print(c("result" = mean(result)))
}
```

    ##         1 
    ## 0.3905329 
    ##         5 
    ## 0.5836208 
    ##         3 
    ## 0.5607326 
    ##         4 
    ## 0.5298649 
    ##         2 
    ## 0.4958383 
    ##    result 
    ## 0.5121179

``` r
#As predicted the dredged model with lower AIC was better at predicting 



#loop for extracting RMSE for multiple folds, that we wil use to check the  "optimal" number of folds for both our models
result2 <- c()

{for (n in 2:29){
  result <- c()
  folds1 <- fold(clean_train, n, cat_col = "Diagnosis", id_col = "Child.ID")
  for (i in unique(folds1[[".folds"]])){
  test <- subset(folds1, .folds == i)
  train <- subset(folds1, .folds != i)
  model <- lmer(CHI_MLU ~ Visit*Diagnosis + verbalIQ1 + MOT_MLU + (1+Visit|Child.ID), train, REML =F,control = lmerControl(
  optimizer = "nloptwrap",
  calc.derivs = F,
  optCtrl = list(
    ftol_abs = 1e-10,
    xtol_abs = 1e-10,
    maxeval = 10000
  )))
  result[i] <- rmse(test$CHI_MLU, predict(model,test, allow.new.levels = T))
  }
  result2[n] <- mean(result)
  }
}


res <- as.data.frame(result2)

result2 <- c()

{for (n in 2:29){
  result <- c()
  folds1 <- fold(clean_train, n, cat_col = "Diagnosis", id_col = "Child.ID")
  for (i in unique(folds1[[".folds"]])){
  test <- subset(folds1, .folds == i)
  train <- subset(folds1, .folds != i)
  model <- lmer(CHI_MLU ~ Visit*Diagnosis*verbalIQ1 + MOT_MLU + verbalIQ1:MOT_MLU + (1+Visit|Child.ID), train, REML =F,control = lmerControl(
  optimizer = "nloptwrap",
  calc.derivs = F,
  optCtrl = list(
    ftol_abs = 1e-10,
    xtol_abs = 1e-10,
    maxeval = 10000
  )))
  result[i] <- rmse(test$CHI_MLU, predict(model,test, allow.new.levels = T))
  }
  result2[n] <- mean(result)
  }
}
```

    ## Warning: Model failed to converge with 1 negative eigenvalue: -6.8e+01

    ## Warning: Model failed to converge with 1 negative eigenvalue: -2.8e+01

``` r
res$dredge <- result2
res$fold <- c(seq(2:30))
res <- filter(res, res$dredge != "NA")

#Optimal number of folds for dredged model
res[which(res$dredge == min(res$dredge)),]
```

    ##      result2    dredge fold
    ## 24 0.5330742 0.4726582   25

``` r
#Optimal number of folds for our model
res[which(res$result2 == min(res$result2)),]
```

    ##      result2    dredge fold
    ## 12 0.5162219 0.4965269   13

### Exercise 3) Assessing the single child

Let’s get to business. This new kiddo - Bernie - has entered your
clinic. This child has to be assessed according to his group’s average
and his expected development.

Bernie is one of the six kids in the test dataset, so make sure to
extract that child alone for the following analysis.

You want to evaluate:

-   how does the child fare in ChildMLU compared to the average TD child
    at each visit? Define the distance in terms of absolute difference
    between this Child and the average TD.

-   how does the child fare compared to the model predictions at Visit
    6? Is the child below or above expectations? (tip: use the predict()
    function on Bernie’s data only and compare the prediction with the
    actual performance of the child)

``` r
#Filtering Bernie from test data
bernie<-filter(clean_test,Child.ID=="2")
View(bernie)
berniev6 <- bernie[6,]
berniev6
```

    ##   Child.ID Visit Ethnicity Diagnosis Gender  Age ADOS MullenRaw
    ## 6        2     6     White        TD      M 49.3   NA        50
    ##   ExpressiveLangRaw Socialization  MOT_MLU  CHI_MLU types_MOT types_CHI
    ## 6                36            92 3.818557 3.448413       361       203
    ##   tokens_MOT tokens_CHI Ados1 verbalIQ1 nonVerbalIQ1 Socialization1
    ## 6       1696        766     7        28           33            105

``` r
#Measuring mean MLU from TD kids in train data

tds<-filter(clean_train,Diagnosis=="TD")

#Means of average TD kid pr visit 
means<-aggregate(CHI_MLU~Visit,data=tds,mean)
print(means)
```

    ##   Visit  CHI_MLU
    ## 1     1 1.304476
    ## 2     2 1.443683
    ## 3     3 1.770635
    ## 4     4 1.858708
    ## 5     5 1.598512
    ## 6     6 1.891907

``` r
means$bernie_abs<-bernie$CHI_MLU-means$CHI_MLU

means
```

    ##   Visit  CHI_MLU bernie_abs
    ## 1     1 1.304476  0.6799798
    ## 2     2 1.443683  1.1007610
    ## 3     3 1.770635  1.5825561
    ## 4     4 1.858708  1.3243901
    ## 5     5 1.598512  1.5747398
    ## 6     6 1.891907  1.5565052

``` r
#How does the child fare compared to the model predictions at Visit 6? Is the child below or above expectations? (tip: use the predict() function on Bernie's data only and compare the prediction with the actual performance of the child)



rmse(berniev6$CHI_MLU,predict(model1,berniev6))
```

    ## [1] 0.001704957

### OPTIONAL: Exercise 4) Model Selection via Information Criteria

Another way to reduce the bad surprises when testing a model on new data
is to pay close attention to the relative information criteria between
the models you are comparing. Let’s learn how to do that!

Re-create a selection of possible models explaining ChildMLU (the ones
you tested for exercise 2, but now trained on the full dataset and not
cross-validated).

Then try to find the best possible predictive model of ChildMLU, that
is, the one that produces the lowest information criterion.

-   Bonus question for the optional exercise: are information criteria
    correlated with cross-validated RMSE? That is, if you take AIC for
    Model 1, Model 2 and Model 3, do they co-vary with their
    cross-validated RMSE?

### OPTIONAL: Exercise 5): Using Lasso for model selection

Welcome to the last secret exercise. If you have already solved the
previous exercises, and still there’s not enough for you, you can expand
your expertise by learning about penalizations. Check out this tutorial:
<a href="http://machinelearningmastery.com/penalized-regression-in-r/" class="uri">http://machinelearningmastery.com/penalized-regression-in-r/</a>
and make sure to google what penalization is, with a focus on L1 and
L2-norms. Then try them on your data!
