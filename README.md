# Florence Nightingale
#### by Edward Gunning

Celebration of Florence Nightingale's Bicentenary by recreating some of her work with `ggplot2` and other R packages.

I have uploaded a scanned (color) pdf containing some of the original diagrams and data called  `MortalityofTheBritishArmy.pdf`. It's available at [this archive](https://archive.org/details/mortalityofbriti00lond/page/n41/mode/2up).


## 1 Nightingale's Coxcomb/ Rose Diagram
Perhaps Nightingale's most famous known as the _Coxcomb_, the _Polar Area Chart_ or the _Rose Diagram_. See [Understanding Uncertainty](https://understandinguncertainty.org/node/214) for a good description of the mathematics behind the coxcomb. All code is [here](https://github.com/edwardgunning/FlorenceNightingale/blob/master/Rose%20Diagram%20Code.R).

![](causesofmortality.png)

## 2 Nightingale's Lines Diagram

An aim to create to recreate [this classic bar chart](https://archive.org/details/mortalityofbriti00lond/page/n33/mode/2up).
I've transcribed the data from  [here](https://archive.org/details/mortalityofbriti00lond/page/12/mode/2up) and uploaded it [here](https://github.com/edwardgunning/FlorenceNightingale/blob/master/EnglishMortalityData.xlsx).

The code to create the line chart is in [this script](https://github.com/edwardgunning/FlorenceNightingale/blob/master/Lines%20Diagram%20B.R).

![](LinesDiagramB.png)

## 3 Nightingale's Area Diagrams

Original [here](https://archive.org/details/mortalityofbriti00lond/page/n39/mode/2up). Certainly my favorite to recreate. [Code](https://github.com/edwardgunning/FlorenceNightingale/blob/master/Area%20Charts.R) contains a really easy data manipulation from the [life table](https://github.com/edwardgunning/FlorenceNightingale/blob/master/EnglishMortalityData.xlsx) and a `facet_wrap` to create the plot:

![](AreaChart.png)


