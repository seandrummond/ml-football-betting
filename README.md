# ml-football-betting

This repository details the methods used to develop a quantitative strategy to bet on markets in professional football (soccer) matches. The strategy has three unique characteristics; feature engineering, use of look-up-tables for confident bet selection, and a statistical method to maximize return. Machine Learning algorithms (Random Forest Classifier), probabilistic & statistical methods and football knowledge were all used to develop this strategy.

Raw data was obtained from www.footballdata.co.uk on nine of the top European Football Leagues and odds pricing on upcoming matches in those leagues from BetFair’s API. Data on each team’s match performance was used, rather than data on individual players. The strategy analysed nine different markets for each match.

The strategy was trialled over twelve matchweeks from the 09/02/2018 to the 27/04/2018 and resulted in 89% of the 27 placed bets winning. This generated an accumulative return of 38%.

This strategy was developed and tested by Sean Drummond. I am a MSc in Computing (Machine Learning) student studying at Imperial College London with a keen interest in sports and data science. I am currently seeking employment in a data science role based in London beginning in September 2019.
(https://www.linkedin.com/in/sean-drummond-a83918a7/)

This repository contains the code, instructions and a brief paper on the results.

The data can be obtained at [https://drive.google.com/open?id=1ER31w4F_wUeidBUWOHkTy9JqgcUURtFa].

Big thanks to www.football-data.co.uk for the open-sourced data and to Thomas Heslop for creating the betfair R API.


## File Description
DataPrep.r - Feature Engineering and Data Preparation

Model.r - Modelling

CAP.r - Confidence Accuracy Proportion Look Up Table Generation

POR.r - Prediction Odd Request Function

ROS.r - Return Maximization Strategy

Flowchart.pdf - Flowchart showing how files fit together

Paper.pdf - Paper on Results

