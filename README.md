# Assessing Long-Run Price Convergence in Retailing

This folder has the code files for the paper "Assessing Long-Run Price Convergence in Retailing" by Fernando Borraz and Leandro Zipitría.

The original database has information of:

- an index of each store, product, time, city, and department (Uruguayan states)
- the median price for each triple (store/product/month)
- a count variable of the number of competition and brands for each triple


The order of files, and its explanation, is as follow:

1) 0.Bases.Convergence.R: run the regressions of the price database to filter the series and store the residuals. 

2) 1.CreateSDbase_2023.R: estimate standard deviation and mode for price and residual series.

3) 2.figures_statistics_2023.R: create table 1 (can be safely ignored) 

4) 3.RegressionsSD_2023.R: all regressions and figures in the paper