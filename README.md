# Long Run Price or Basket Convergence?

This folder has the code files for the paper "Long Run Price or Basket Convergence?" by Fernando Borraz and Leandro Zipitría.
The original database has information of:

- an index of each store, product, time, city, and department (Uruguayan states)
- the median price for each triple (store/product/month)
- a count variable of the number of competition and brands for each triple


The order of files, and its explanation, is as follow:

1) 0.Bases.Convergence.R: run the regressions of the price database to filter the series and store the residuals. 

2) 1.CreateSDbase.R: estimate standard deviation and mode for price and residual series.

3) 2.Info.general.R: create the figures (can be safely skipped)

4) 3.Regressions.R: regressions of price (equation 1 of the Paper)

5) 4.RegressionSD.R: the main regressions of the paper (equation 2)

