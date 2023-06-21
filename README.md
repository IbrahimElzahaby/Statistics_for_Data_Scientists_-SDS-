# Statistics_for_Data_Scientists (SDS) course assignments

The repository contains five tasks according to the following structure:


### 1- Logistic regression.

[Assignment 1](https://github.com/IbrahimElzahaby/Statistics_for_Data_Scientists_-SDS-/tree/de266842554fdf70f1886d60145c4f58b3021669/logistic_regression) was about fitting a logistic regression for the following:

    - The effect of the ozone and cultivar on the infection
    
    # Logistic regression model formulation:
    p(x) = 1/(1+exp(beta0 + beta1*ozone + beta2*cultivar + beta3(cultivar*ozone)))
    
    - The interaction between ozone and cultivar by comparing a full and reduced model
    
    # Full model:
    lgF <- glm(cbind(Infected, 1-Infected)~cultivar+ ozone+ cultivar:ozone, data = data, binomial)
    
    # Reduced model:
    lgR <- glm(cbind(Infected, 1-Infected)~cultivar+ ozone, data = data, binomial)
    
A brief explanation of the data:

Botrytis cinerea is a pathogen that can infect beans (Phaseolus vulgarus). One expects that 
the damage is more severe in plants that are weakened due to ozone gas. To investigate this, 
20 days old plants of 3 cultivars (Strat, Pros, and Lit) are taken and given an 
ozone treatment with concentrations 0, 120, 180, or 270 ppm. For each concentration, 30 plants of 
each cultivar were used. Subsequently, each plant was inoculated with botrytis and after a few 
days, it was observed if a plant was infected or not. The data can be found in the file 
[botrytis.csv](https://github.com/IbrahimElzahaby/Statistics_for_Data_Scientists_-SDS-/blob/b72f36e346b84e380502bfcbf1a9bffe4285d323/logistic_regression/botrytis.csv).


### 2- Regularized regression.

The goal of [Assignment 2](https://github.com/IbrahimElzahaby/Statistics_for_Data_Scientists_-SDS-/tree/749b88957105b05086052b492c5e823f6141a9cb/regularized_regression) is to predict ash content from spectra by creating a model with a good fit using cross-validation, regularized regression, and lasso regression techniques.

A brief explanation of the dataset: 

The dataset in this exercise consists of ash content (in percentages) and fluorescence spectra 
from 324.5 to 560.0 nm of 265 samples of sugar. The Ash content of refined granulated sugar must 
not exceed 0.015% by most standards. Ash content can be reduced by maintaining proper 
filtration, sufficient washing during centrifugation of the sugar, and proper handling of the 
sugar during drying and screening. The data are in the file 
[SugarData.Rdata](https://github.com/IbrahimElzahaby/Statistics_for_Data_Scientists_-SDS-/blob/de266842554fdf70f1886d60145c4f58b3021669/regularized_regression/SugarData.Rdata).


### 3- Linear mixed models.

In [Assignment 3](https://github.com/IbrahimElzahaby/Statistics_for_Data_Scientists_-SDS-/tree/749b88957105b05086052b492c5e823f6141a9cb/linear_mixed_models), we explored mixed models through using ANOVA and REML.

A brief explanation of the experiment:

experiment to investigate the influence of two levels of feed ratio on the growth of    
animals. It is also investigated whether the feed ratio has a similar effect for males and 
females. In setting the experiment, 24 animals were selected from 12 representative litters. 
Pairs of 6 males and 6 females were randomly selected from the representatives and two feed 
ration levels are randomly assigned to the pairs making the design a split plot. The growth 
per animal over a fixed period of time is predicted using the feed ratio and sex level 
variables. Since the representatives are from the same parent, this introduces dependence in 
the paired variable and thus the choice for mixed models. Summary of 
[data](https://github.com/IbrahimElzahaby/Statistics_for_Data_Scientists_-SDS-/blob/4adaa52b5a0548e731ae9901c496d35d05197d00/linear_mixed_models/DATARationSplitPlot.txt) shows the mean growth of animal is 31.45. 


### 4- General additive models (GAMs).

[Assignment4](https://github.com/IbrahimElzahaby/Statistics_for_Data_Scientists_-SDS-/tree/749b88957105b05086052b492c5e823f6141a9cb/general_additive_models_(GAMs)) focused on applying generalized additive 
models (GAMs) for predicting grain number in a specific maize hybrid. Our analysis focused on 
a single maize hybrid grown in 25 different environments(observations) under 14 different 
environmental characterizations(variables).


### 5- Trre-based methods.

In [Assignment 5](https://github.com/IbrahimElzahaby/Statistics_for_Data_Scientists_-SDS-/tree/749b88957105b05086052b492c5e823f6141a9cb/tree-based_classification), we predicted patients with the liver disease either positive or negative using tree-based methods such as Random Forest, Bagging, and Boosting algorithms with different settings and hyperparameters to investigate the different accuracy in each model, Also, the specificity and sensitivity to measure the performance of all classification models. Moreover, logistic regression was used to predict positive cases of liver disease, and the comparison was applied with the tree-based methods. The provided datasets contain 400 patient profiles which include the gender, age, and other eight enzyme ratios (10 observations in total).


