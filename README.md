# Premature_Hierarchical

Codes used for paper "How to measure premature mortality? A proposal combining “relative” and “absolute” approaches" with Marc Suhrcke and Lucia Zanotto

- create_table.R creates a table with life table data (number of deaths by age) for a group of countries. Data are taken from LAMBdA database (https://www.ssc.wisc.edu/cdha/latinmortality2/)
- tables_LAMBDA.csv is the output of create_table.R
- Lambda_hierarchic_m.R is the R code estimating the hierarchical model using STAN
- hierarchical_model.stan is the related stan code
- create_figure.R takes the output of hierarchial model and plot the observed vs fitted data
