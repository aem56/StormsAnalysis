US Storm Database Analysis
========================================================
author: Aristide Mooyaart
date: 16/04/16
autosize: true

Introduction
========================================================

Storms and other severe weather events can cause both public health and economic problems for communities and municipalities. Many severe events can result in fatalities, injuries, and property damage, and preventing such outcomes to the extent possible is a key concern.

In this project I have:

- Tidied the database to facilitate analysis
- Analysed the database to work out the worst storm types for public health and the economy
- Created an interactive web app to allow a user to explore the database further

Data Tidying and Analysis
========================================================

The Storms database required extensive tidying to correct miss-labelled variables. As an example there are 50 possible types of storm but the database described storms in 985 ways, the majority of which were misspelled or incomplete.

Separating out the storm events that had the highest number of fatalities and injuries and the highest number of crop and property damage showed that tornadoes have the highest impact to population health, whilst floods have the highest economic impact.

![plot of chunk unnamed-chunk-1](StormAnalysis-figure/unnamed-chunk-1-1.png)![plot of chunk unnamed-chunk-1](StormAnalysis-figure/unnamed-chunk-1-2.png)

Interactive web app
========================================================

![alt text](images/capture.jpg)

***

Created an easy to use web app for exploring the database further.

The web app allows the user to:

- Select which years to display  
  
- Select what storm types to display
  
It shows graphs of:

- Number of recorded storms by year  
  
- Population impact (fatalities and deaths) by year  
  
- Economic impact (crop and property damage) by year

Summary
========================================================
- The storm database required extensive tidying to facilitate analysis
- Storm database analysis shows the types of storms that have the greatest impact to public health and the economy
- An easy to use web app has been created that allows for trend analysis for any particular storm type and year range
- The data tidying, analysis and web app could easily be used on updates to the storm database
- My code is available on [github](https://github.com/aem56/StormsAnalysis), please [email me](mailto:aristide.mooyaart@gmail.com) if you find any bugs!