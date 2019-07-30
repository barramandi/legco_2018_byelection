# LegCo 2018 By-Election Analysis
Political alignment and demographics in 2018 Hong Kong Legislative Council (LegCo) By-election

## What it does
An attempt to find out the correlation between demogrpahics and political support 
by combining data from the 2016 by-census and the vote counts of every counting station
in the 2018 LegCo By-election.

## What they are
Three interactive maps:
1. Winning political camp and margin of votes in each District Council constituency
2. Monthly median income in each District Council constituency
3. Home ownership rate in each District Council constituency

and an R script that creates them.

## How it works
Each counting station in the 2018 LegCo By-election has a code with a prefix letter 
that corresponds to the District Council Contituency Areas (DCCA). Geographical demographics
from the government census is also based on DCCA. By mapping the political support and demographics 
in each DCCA, it provides a glimpse of the relationship between the two.

This method is not perfect. Some DCCA do not have their own counting stations and their votes are 
counted in stations in neighbouring DCCA instead. This explains the grey out area in the map,
most notably Sai Kung Islands. This affects the validity of the result.

## What you need
* [Shapefile](https://accessinfo.hk/zh_HK/request/shapefile_for_2015_district_coun) of the 2015 DCCA
(kudos to [accessinfo.hk](https://accessinfo.hk))
* [helixcn/HK80](https://github.com/helixcn/HK80)

The R script automatically fetches the the [2018 By-Election result](https://www.elections.gov.hk/legco2018by/eng/results.html)
and [2016 By-census data](https://www.bycensus2016.gov.hk/en/index.html).

All other packages are obtainable from CRAN.

## Reference
The function to implement the shapefile in R is taking reference to [stanyip/DC2015](https://github.com/stanyip/DC2015).
