# Kaggle_Telstra
Please download the data sets from the [Kaggle competition page](https://www.kaggle.com/c/telstra-recruiting-network).

## Solution
1. Built an XGBoost using one-hot encoded raw features, along with some feature engineering using the '[magic feature](https://www.kaggle.com/c/telstra-recruiting-network/forums/t/19092/is-there-a-magic-feature-in-the-data)'. Code can be found in **_model_xgb.R_**. This gives CV of 0.40220 and public/private LB score of 0.40812/0.40585 (rank 14).

2. I ensembled few XGBoosts (with different parameters / subsets of features / subsets of observations) for my final score of 0.40735 / 0.40267, which ranked 10th / 9th on the public / private LB. The username on the LB is 'Vopani'. I was competing as 'Anonymous Ghost'.
