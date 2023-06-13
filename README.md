# Cure Violence Program in NYC
## Neighborhood Investments and Public Safety

### Backgroud
In the wake of a rebound in shootings and homicides since the Covid-19 pandemic, as well as high profile cases of gun violence across the country, violent crime has become a top concern for many New Yorkers. Overall, New York City has a lower violent crime rate than most other large American cities—a rate that is near historic lows, with shootings and homicides down 23.2% and 11.4% respectively from last year. However, violent crime remains an ongoing issue in several hotspots around the city.

Cure Violence takes an evidence-based, public health approach to gun violence by attempting to detect and interrupt conflicts before they escalate, identify and treat high risk individuals (those most likely to commit gun violence and/or become victims of a shooting), and change social norms. According to the NYC DOH “this differs from a criminal justice approach, which does not address how inequity and structural racism can diminish these factors and lead to gun violence in a community.”

### Goal
The Council’s Data Team presents a comprehensive in-house statistical analysis of the Cure Violence program which analyzes impacts across all involved precincts. Methodology and data are presented, with the results suggesting that the program is effective at reducing shootings in New York City.

### Data
- ['NYPD Shooting Incident Data (Historic)'](https://data.cityofnewyork.us/Public-Safety/NYPD-Shooting-Incident-Data-Historic-/833y-fsy8)
- ['NYPD Arrests Data (Historic)'](https://data.cityofnewyork.us/Public-Safety/NYPD-Arrests-Data-Historic-/8h9b-rp9u)
- ['NYC Precinct Population Data'](https://johnkeefe.net/nyc-police-precinct-and-census-data)

### Methods
#### Structure and Model
We model the impact of Cure through the use of a linear model. Precincts entered the Cure Violence program at different points in time from 2012 and 2019, which produces a time-varying introductory process known as a step-wedge design.

Longitudinal data on shootings spanning from 2006 to 2020 are analyzed. Because of the impact of Covid-19 on crime levels, the 2020 shooting and arrest rates are extrapolated using the pre-Covid data as well as the past 7-year annual time distribution of shootings. What culminated was a segmented regression model, which is used for “statistically modeling interrupted time series data to draw more formal conclusions about the impact of an intervention or event on the measure of interest.” More specifically, a segmented negative-binomial model was used.

#### Outcome of Interest
Our measure of interest is variation in the number of shootings in each precinct. We include a random effect for precincts to account for correlation within group samples. We are also interested in accounting for anything that may have impacted overall tendency for crime to occur. This could include economic or policy changes. Arrest data is used as a proxy for this, and included as a covariate in the model.

There are two parameters of interest: the level and the trend impact of Cure. The level parameter, i.e. immediate impact, refers to the change in shootings immediately and consistently present since the first year that the program was introduced into the precinct. The trend impact estimates any time-varying rate of change in shootings since introduction to the program, i.e. a strengthening or weakening of the impact of the intervention over time.

### Results

