# Statistics_for_Data_Scientists (SDS) course assignments:

The repository contains five tasks according to the following structure:


### 1- Logistic regression.

[Assignment 1](https://github.com/IbrahimElzahaby/Statistics_for_Data_Scientists_-SDS-/tree/de266842554fdf70f1886d60145c4f58b3021669/logistic_regression) was about fitting a logistic regression for the following:

    The effect of the ozone and cultivar on the infection
    # Logistic regression model formulation:
    p(x) = 1/(1+exp(beta0 + beta1*ozone + beta2*cultivar + beta3(cultivar*ozone)))
    
    the interaction between ozone and cultivar by comparing a full and reduced model
    
    # Full model:
    lgF <- glm(cbind(Infected, 1-Infected)~cultivar+ ozone+ cultivar:ozone, data = data, binomial)
    
    # Reduced model:
    lgR <- glm(cbind(Infected, 1-Infected)~cultivar+ ozone, data = data, binomial)
    
Brief explaination about the data:
Botrytis cinerea is a pathogen that can infect beans (Phaseolus vulgarus). One expects that 
the damage is more severe in plants that are weakened due to ozone-gas. To investigate this, 
20 days old plants of 3 cultivars (Strat, Pros, and Lit) are taken and given an 
ozone-treatment with concentration 0, 120, 180 or 270 ppm. For each concentration 30 plants of 
each cultivar were used. Subsequently each plant was inoculated with botrytis and after a few 
days it was observed if a plant was infected or not. The data can be found in the file  
[botrytis.csv](https://github.com/IbrahimElzahaby/Statistics_for_Data_Scientists_-SDS-/blob/b72f36e346b84e380502bfcbf1a9bffe4285d323/logistic_regression/botrytis.csv).


### 2- Regularized regression.

The goal of [Assignment 2](https://github.com/IbrahimElzahaby/Statistics_for_Data_Scientists_-SDS-/tree/de266842554fdf70f1886d60145c4f58b3021669/regularized_regression) is to predict ash content from spectra by creating a  
model with a good fit using cross validation, regularized regression and lasso regression techniques.

Brief explaination about the dataset:
The dataset in this exercise consists of ash content (in percentages) and fluorescence spectra from 324.5 to 560.0 nm of 265 samples of sugar. Ash content of refined granulated sugar must not exceed 0.015% by most standards. Ash content can be reduced by maintaining proper filtration, sufficient washing during centrifugation of the sugar, and proper handling of the sugar during drying and screening. The data are in the file [SugarData.Rdata](https://github.com/IbrahimElzahaby/Statistics_for_Data_Scientists_-SDS-/blob/de266842554fdf70f1886d60145c4f58b3021669/regularized_regression/SugarData.Rdata).


### 3- Linear mixed models.

[Assignment 3](https://github.com/IbrahimElzahaby/Statistics_for_Data_Scientists_-SDS-/tree/de266842554fdf70f1886d60145c4f58b3021669/linear_mixed_models) 


### 4- General additive models (GAMs).

[Assignment 4](https://github.com/IbrahimElzahaby/Statistics_for_Data_Scientists_-SDS-/tree/de266842554fdf70f1886d60145c4f58b3021669/general_additive_models_(GAMs)) 


### 5- Trre-based methods.

[Assignment 5](https://github.com/IbrahimElzahaby/Statistics_for_Data_Scientists_-SDS-/tree/de266842554fdf70f1886d60145c4f58b3021669/tree-based_classification) 


