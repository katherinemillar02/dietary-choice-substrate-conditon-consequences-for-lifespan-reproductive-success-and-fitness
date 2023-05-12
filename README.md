# The effect of mating status and substrate condition on dietary choice and consequences for lifespan and reproductive success.


 

# Project Description

<img title="droso pic" alt="drosopAlt text" src="/images/RLogo.png" width=100 height=100>

A GitHub repistory and R-project including data analysis and visualisation for my third year research project.

This data included follows on from a previous project completed through a summer studentship (Titled: Effects of dietary choice on Lifespan in *Drosophila melanogaster*:  https://github.com/katherinemillar02/effects-of-dietary-choice-on-lifespan-in-drosophila-melanogaster).


This project began with using a 'patch preference assay' to test different food conditions on lifespan and fitness in fruitflies. Using both varying protein: carbohydrate diet patches as well as varying food hardness. 
<img title="droso pic" alt="drosopAlt text" src="/images/experimentlayout.png" width=400 height=200>

**__Figure 1__** Figure shows schematic of experimental assay plates, of how the experiments were planned out. 
Experiments involved observations of where the flies liked to feed/ spend time as well as oviposition analysis of where they liked to lay their eggs. 

<img title="droso pic" alt="drosopAlt text" src="/images/exp1assay.png" width=200 height=200>

**__Figure 2__** Image shows an experimental assay from one of the experiments. 

## Description of scripts 

Scripts for each experiment are in sections of: 

**"feeding-overalldiet-analysis"**
This explains scripts with analysis of the feeding observation count data of diets which had both varying P:C ratios and soft and hard foods - before being split into variables. 
For example variables could be: **1:2H, 1:2S, 1:8H, 1:8S**

**"feeding-overalldiet-visualisation"**
This explains scripts with visualisation using barplots of the feeding observation count data of diets which had both varying P:C ratios and soft and hard foods - before being split into variables. 
For example variables could be: **1:2H, 1:2S, 1:8H, 1:8S**

**"feeding-condition-analysis"**
This explains scripts with analysis of the feeding observation count data of diets which had varying P:C ratios and soft and hard foods, split into variables of "Nutrient Composition" (P:C ratios)
and "Food Hardness" (soft and hard foods). 
For example variables could be: **1:2, 1:8, Hard, Soft**

**"feeding-condition-visualisation"**
This explains scripts with visualisation using boxplots and barplots of the feeding observation count data of diets which had varying P:C ratios and soft and hard foods, split into variables of "Nutrient Composition" (P:C ratios) and "Food Hardness" (soft and hard foods). 
For example variables could be: **1:2, 1:8, Hard, Soft**

**"oviposition-overalldiet-analysis"**
This explains scripts with analysis of the oviposition count observation data of diets which had both varying P:C ratios and soft and hard foods - before being split into variables. 
For example variables could be: **1:2H, 1:2S, 1:8H, 1:8S**

**"oviposition-overalldiet-visualisation"**
This explains scripts with visualisation using barplots of the oviposition count observation count data of diets which had both varying P:C ratios and soft and hard foods - before being split into variables. 
For example variables could be: **1:2H, 1:2S, 1:8H, 1:8S**

**"oviposition-condition-analysis"**
This explains scripts with analysis of the oviposition observation count data of diets which had varying P:C ratios and soft and hard foods, split into variables of "Nutrient Composition" (P:C ratios)
and "Food Hardness" (soft and hard foods). 
For example variables could be: **1:2, 1:8, Hard, Soft**

**"oviposition-condition-visualisation"**
This explains scripts with visualisation using boxplots and barplots of the oviposition observation count data of diets which had varying P:C ratios and soft and hard foods, split into variables of "Nutrient Composition" (P:C ratios) and "Food Hardness" (soft and hard foods). 
For example variables could be: **1:2, 1:8, Hard, Soft**


## Description of "old-scripts" 

**"Experiment1.R" (1:8 and 1:2 hard and soft diets)**

A script with two repeated experiments. This experiment looked at using the protein: carbohydrate ratios 1:8 and 1:2 with both a soft and hard version of each diet, to test whether mated female *drosophila melanogaster* showed a particular preference for a diet. 
The preferred diet in which the *drosophila* chose to lay their eggs was also calculated. 

**"Experiment2.R" (8:1 and 1:2 hard and soft diets)**

An experiment to test a known preferred diet (8:1) with a known least preferred diet (1:2) to see if *drosophila melanogaster* will favour food type over dietary levels. 

<img title="droso pic" alt="drosopAlt text" src="/images/drosophila.schematic.png" width=400 height=200>

**"Experiment3.R" (8:1 and 1:8 hard and soft diets)**

Oviposition preference based experiment.
Both feeding behaviour and oviposition preference was measured in this experiment, using a diet which is known that *d. melanogaster* like to lay their eggs on, and a diet which is known to be a least preferred diet for oviposition preference. 

<img title="droso pic" alt="drosopAlt text" src="/images/graphimage.png" width=400 height=300>

**"qPCR data analysis" (qPCR 8:1 and 1:8 larvae gene expression experiments)**
Effect of maternal oviposition choice on larval gene expression.  This experiment analyses the qPCR data collected to look at the effect of oviposition choice on larval gene expression using target genes *dilp3* and *dFOXO*. 
