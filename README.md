This repository provides the R routines used in the article [Improving crime count forecasts in the city of Rio de Janeiro via reconciliation](https://doi.org/10.1057/s41284-024-00433-5). It is a joint work with Leonardo M. Barreto, published in the _Security Journal_.

Crime prediction based on reliable data and sound statistical methodologies can provide valuable input for the tactical deployment of police resources in so-called crime hot spots and effective planning of police operations. In Rio de Janeiro, the Public Safety Secretariat uses criminal activity forecasts to detect crime patterns and evaluate police performance. The paper evaluates the impact of reconciliation on the forecasts of 271 series of registered criminal occurrences in the city of Rio de Janeiro on a monthly basis from January 2003 to December 2019. We verify that reconciliation improves crime count forecasts, especially on the most disaggregated series.

The repo includes:

- (Step 1) **data_processing.R** : code that manipulates the raw data set to generate useful data objects for the base forecasts and reconciliation;
- (Step 2) **base_forecast.R** : code that generates base forecasts;
- (Step 3) **reconciliation.R** : code that reconciles base forecasts;
- Directory **./data_base** contains the raw data set;
- Directory **./data_treated** contains data objects generated in **data_processing.R**.
