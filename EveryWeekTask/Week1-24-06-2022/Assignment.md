# Assignment (deadline 24/06/2022)

### The synthetic data is generated 

- Task 1: plot the distribution of the original data and the synthetic data for general comparison.

With one-way utility measure, we try to plot the compare histogram plots included in package *synthpop* to have a general overview of the synthetic dataset and the original dataset. With too many variables involved in both datasets, we also manage to subset 5 of the variables from the original columns and show you the comparison analysis done by the __compare__ function. To be more specific, the 5 variables are selected randomly, i.e., B3 and B1_1 from the "symptoms" group, B7 from the "testing" group, D1 and D2 from group "module B", E2 and E3 from group "demographics".

For var B3, the table of selected utility measures is shown as follows: 
| Utility measures | pMSE     | S_pMSE  | 
|:----------------:|:--------:|:-------:|
| B3               | 0.000555 | 4034.21 

![Compare plot for var B3](F:/Master-Thesis-DifferentialPrivacy/EveryWeekTask/Week1-24-06-2022/B3_compare_plot.png)


For var B1_1,
| Utility measures | pMSE     | S_pMSE  |
|:----------------:|:--------:|:-------:|
| B1_1               | 0.000122 | 890.2343 

![Compare plot for var B1_1](F:/Master-Thesis-DifferentialPrivacy/EveryWeekTask/Week1-24-06-2022/B11_compare_plot.png)

For var B7,
| Utility measures | pMSE     | S_pMSE  |
|:----------------:|:--------:|:-------:|
| B7               | 4.9e-05 | 358.3944 

![Compare plot for var B7](F:/Master-Thesis-DifferentialPrivacy/EveryWeekTask/Week1-24-06-2022/B7_compare_plot.png)

For var B7,
| Utility measures | pMSE     | S_pMSE  |
|:----------------:|:--------:|:-------:|
| B7               | 4.9e-05 | 358.3944 

![Compare plot for var B7](F:/Master-Thesis-DifferentialPrivacy/EveryWeekTask/Week1-24-06-2022/B7_compare_plot.png)

For var D1,
| Utility measures | pMSE     | S_pMSE  |
|:----------------:|:--------:|:-------:|
| D1               | 0.000562 | 4089.81 

![Compare plot for var D1](F:/Master-Thesis-DifferentialPrivacy/EveryWeekTask/Week1-24-06-2022/D1_compare_plot.png)

For var D2,
| Utility measures | pMSE     | S_pMSE  |
|:----------------:|:--------:|:-------:|
| D2             | 0.000617 | 4488.802 

![Compare plot for var D2](F:/Master-Thesis-DifferentialPrivacy/EveryWeekTask/Week1-24-06-2022/D2_compare_plot.png)

For var E2,
| Utility measures | pMSE     | S_pMSE  |
|:----------------:|:--------:|:-------:|
| E2             | 0.002015 | 9770.918 

![Compare plot for var E2](F:/Master-Thesis-DifferentialPrivacy/EveryWeekTask/Week1-24-06-2022/E2_compare_plot.png)

For var E3,
| Utility measures | pMSE     | S_pMSE  |
|:----------------:|:--------:|:-------:|
| E3             | 0.001159 | 4214.772 

![Compare plot for var E3](F:/Master-Thesis-DifferentialPrivacy/EveryWeekTask/Week1-24-06-2022/E3_compare_plot.png)

- Task 2: evaluate the propensity mean squared error from the synthetic data to the original data (hint: use the utility.gen function implemented)