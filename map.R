library(leaflet)
library(rgdal)
library(HK80)
library(stringr)
library(xlsx)
library(magrittr)
library(dplyr)
library(colorRamps)
library(htmlwidgets)

###-------------Initialisation-------------
dcca2015 <- readOGR("./DC_2015_poly/GIH3_DC_2015_POLY.shp", layer = "GIH3_DC_2015_POLY", GDAL1_integer64_policy = TRUE) #Load boundaries of District Council constituencies
dcca2015$ENAME %<>% as.character(.) #Remove factor

result2018 <- data.frame(gc = '', zone = dcca2015$CACODE, zone_name = dcca2015$ENAME, district = dcca2015$DISTRICT_E, camp = NA, stringsAsFactors = FALSE) #Create data frame containing election result and demographic

lc1_prefix <- c("A", "B", "C", "D") #Initial character of code of HK Island constituencies
lc2_prefix <- c("E", "F", "G") #Kowloon W
lc5_prefix <- c("N", "P", "Q", "R") #NT E

result2018 <- sapply(1:nrow(result2018), function(x) { #Delete constituencies not in by-election
  if(str_extract_all(result2018$zone[x], '^[A-Z]{1}')[[1]] %in% c(lc1_prefix, lc2_prefix, lc5_prefix)) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}) %>% result2018[., ]

result2018$gc <- sapply(result2018$zone, function(x) { #Assign LegCo election area code
  if(str_extract_all(x, '^[A-Z]{1}')[[1]] %in% lc1_prefix) {
    return("LC1")
  } else if(str_extract_all(x, '^[A-Z]{1}')[[1]] %in% lc2_prefix){
    return("LC2")
  } else if(str_extract_all(x, '^[A-Z]{1}')[[1]] %in% lc5_prefix) {
    return("LC5")
  }
})

## Convert map grid system from HK1980 to WGS84 and remove irrelevant consituency
## Reference: https://stanyipdotcom.wordpress.com/2015/12/18/%E5%85%A9%E5%B1%86%E5%8D%80%E8%AD%B0%E6%9C%83%E9%81%B8%E5%8D%80%E5%88%86%E7%95%8C%E4%BA%92%E5%8B%95%E5%9C%B0%E5%9C%96/
dcca_poly <-  list()
for (i in 1:length(dcca2015)) {
  dcca_poly[[i]] <-  data.frame(t(apply(dcca2015[i, ]@polygons[[1]]@Polygons[[1]]@coords, 1, function(x) rev(unlist(HK1980GRID_TO_WGS84GEO(x[2], x[1]))))))
}
dcca2015 <- SpatialPolygons(sapply(1:length(dcca2015$CACODE), function(x) Polygons(list(Polygon(dcca_poly[[x]])), dcca2015$CACODE[x])))

###-------------Process data-------------
## Function to fetch election result for each district
find_result <- function(url, pro_dem, pro_est, df) { #url of election result, pro-dem candidate(s) number(s), pro-dest candidate(s) number(s), dataframe to contain result
  download.file(url, 'result.xls', mode = "wb")
  raw <- read.xlsx('result.xls', sheetIndex = 1, startRow = 6)
  for(i in 4:ncol(raw)) { #Convert factor to numeric
    if(is.factor(raw[, i])){
      raw[, i] %<>% as.numeric(levels(.))[.]
      raw[, i][is.na(raw[, i])] <- 0
    }
  }
  raw[, 1] %<>% sapply(., function(x) { #Convert polling station code into constituency code
    str_extract_all(x, '^[A-Z]{1}[0-9]{2}')[[1]] #First 3 characters of polling station code = constituency code
  }) %>% as.character
  raw %<>% .[.[, 1] %in% df$zone, ] #Drop rows with no contituency code (i.e. irrelevant rows)
  for(i in 1:nrow(raw)) { #Find proportion of votes for respective camps
    if(is.na(df$camp[df$zone == raw[i, 1]])) { #Only deal with uncalculated constituency
      n <- length(raw[raw[ ,1] %in% raw[i ,1], 1]) - 1 #Find number of polling stations in the constituency - 1
      pro_dem_votes <- 0
      for(j in 1:length(pro_dem)) { #Find proportion of votes for pro-democracy candidates from ALL polling stations in the constituency
        pro_dem_votes  <- pro_dem_votes + raw[i:(i + n), (pro_dem[j] + 3)]
      }
      df$pro_dem_prop[df$zone == raw[i, 1]] <- sum(pro_dem_votes) / sum(raw$Total.總數[i:(i + n)])
      pro_est_votes <- 0
      for(j in 1:length(pro_est)) { #Find proportion of votes for pro-establishment candidates
        pro_est_votes  <- pro_est_votes + raw[i:(i + n), (pro_est[j] + 3)]
      }
      df$pro_est_prop[df$zone == raw[i, 1]] <- sum(pro_est_votes) / sum(raw$Total.總數[i:(i + n)])
      df$margin[df$zone == raw[i, 1]] <- df$pro_est_prop[df$zone == raw[i, 1]] - df$pro_dem_prop[df$zone == raw[i, 1]] #Margin = % difference in votes
      if(df$pro_est_prop[df$zone == raw[i, 1]] > df$pro_dem_prop[df$zone == raw[i, 1]]) {
        df$camp[df$zone == raw[i, 1]] <- "Pro-Beijing" #Political camp
      } else {
        df$camp[df$zone == raw[i, 1]] <- "Pro-Democracy"
      }
    }
  }
  return(df)
}

##Fetch election result
result2018 %<>% find_result('https://www.elections.gov.hk/legco2018by/pdf/2018_cs_result_LC1.xls', 1, 4, .) #HK Island
result2018 %<>% find_result('https://www.elections.gov.hk/legco2018by/pdf/2018_cs_result_LC2.xls', 1, 2, .) #Kowloon W
result2018 %<>% find_result('https://www.elections.gov.hk/legco2018by/pdf/2018_cs_result_LC5.xls', 6, 4, .) #NT E

result2018$camp[is.na(result2018$camp)] <- "No Data" #Contintuency with no polling station
result2018[result2018$camp == "No Data", 5:6] <-  NA

## Find median income of each contituency
download.file('http://www.bycensus2016.gov.hk/Page/Maintables/source/eng/C305c/Result.xlsx', 'income_2016.xlsx', mode = "wb")
raw <- read.xlsx('income_2016.xlsx', sheetIndex = 1, startRow = 6)
raw %<>% .[1:431, ] #Drop useless rows
raw[, 2:7] <- NULL #Drop useless columns
raw[, 1] %<>% sapply(., function(x) {
  gsub('^[ A-Za-z]+- ', '', x) %>% #Remove district name and preserve constituency name
    gsub(' and ', ' & ', .) #Replace 'and' with '&'
})
colnames(raw) <- c("zone_name", "median_income")
result2018 %<>% left_join(., raw, by = "zone_name") #Merge median income with main data frame by constituency name

## Find proportion of property ownership of each constituency
download.file('http://www.bycensus2016.gov.hk/Page/Maintables/source/eng/E303/Result.xlsx', "own_2016.xlsx", mode = "wb")
raw <- read.xlsx("own_2016.xlsx", sheetIndex = 1, startRow = 6)
raw %<>% .[1:431, ] #Drop useless rows
for(i in 3:ncol(raw)) { #Convert factor to numeric
  if(is.factor(raw[, i])){
    raw[, i] %<>% as.numeric(levels(.))[.]
    raw[, i][is.na(raw[, i])] <- 0
  }
}
for(i in 1:nrow(raw)) {
  raw[i, 16] <- (raw[i, 3] + raw[i, 5]) / raw[i, 15] #Calculate proportion of property ownership
}
raw[, 2:15] <- NULL #Drop useless columns
raw[, 1] %<>% sapply(., function(x) {
  gsub('^[ A-Za-z]+- ', '', x) %>% #Remove district name and preserve constituency name
    gsub(' and ', ' & ', .) #Replace 'and' with '&'
})
colnames(raw) <- c("zone_name", "own_prop")
result2018 %<>% left_join(., raw, by = "zone_name") #Merge with main data frame by constituency name

###-------------Draw Maps-------------
## Function to visualise a variable on map with two layers representing each political camp
draw_map <- function(df, iv, title, prefix, suffix) { #Dataframe with election result, independent variable (vector), title of legend, prefix, suffix of number
  #Pro-establishment
  var1 <- iv[df$camp %in% "Pro-Beijing"]
  var_meta1 <- df %$% .[camp %in% "Pro-Beijing", ]
  gc1 <- sapply(var_meta1$gc, function(x) {
    if(x == "LC1") {
      return("Hong Kong Island")
    } else if(x == "LC2") {
      return("Kowloon West")
    } else if(x == "LC5") {
      return("New Territories East")
    }
  })
  popup1 <- var_meta1 %$% paste0("<b> ", gc1, "</b><br><b>", district, "</b> - <b>", zone_name, "</b><br>Political Camp: Pro-Beijing<br>% Difference in Votes: ", abs(round(margin *100, 2)), "%<br>Median Income: $", median_income, "<br>Home Ownership Rate: ", round(own_prop * 100, 2), "%")
  #Recreate map with only the constituencies of pro-est majority
  dcca_poly <- list()
  n <- 1
  for(i in 1:length(dcca2015)) {
    if(dcca2015@polygons[[i]]@ID %in% var_meta1$zone) {
      dcca_poly[[n]] <- data.frame(longitude = dcca2015@polygons[[i]]@Polygons[[1]]@coords[, 1], latitude = dcca2015@polygons[[i]]@Polygons[[1]]@coords[, 2])
      n <- n + 1
    }
  }
  dcca1 <- SpatialPolygons(sapply(1:length(var1), function(x) Polygons(list(Polygon(dcca_poly[[x]])), var_meta1$zone[x])))
  #Pro-democracy
  var2 <- iv[df$camp %in% "Pro-Democracy"]
  var_meta2 <- df %$% .[camp %in% "Pro-Democracy", ]
  gc2 <- sapply(var_meta2$gc, function(x) {
    if(x == "LC1") {
      return("Hong Kong Island")
    } else if(x == "LC2") {
      return("Kowloon West")
    } else if(x == "LC5") {
      return("New Territories East")
    }
  })
  popup2 <- var_meta2 %$% paste0("<b> ", gc2, "</b><br><b>", district, "</b> - <b>", zone_name, "</b><br>Political Camp: Pro-Democracy<br>% Difference in Votes: ", abs(round(margin *100, 2)), "%<br>Median Income: $", median_income, "<br>Home Ownership Rate: ", round(own_prop * 100, 2), "%")
  #Recreate map with only the constituencies of pro-dem majority
  dcca_poly <- list()
  n <- 1
  for(i in 1:length(dcca2015)) {
    if(dcca2015@polygons[[i]]@ID %in% var_meta2$zone) {
      dcca_poly[[n]] <- data.frame(longitude = dcca2015@polygons[[i]]@Polygons[[1]]@coords[, 1], latitude = dcca2015@polygons[[i]]@Polygons[[1]]@coords[, 2])
      n <- n + 1
    }
  }
  dcca2 <- SpatialPolygons(sapply(1:length(var2), function(x) Polygons(list(Polygon(dcca_poly[[x]])), var_meta2$zone[x])))
  #Draw Map
  m <- leaflet() %>% addTiles() %>% addProviderTiles(providers$CartoDB.Positron)
  m %<>% addPolygons(group = "Pro-Beijing", data = dcca1, weight = 3, popup = popup1, stroke = TRUE, color = "Grey", fillColor = pal(var1), fillOpacity = 0.2,  highlightOptions = highlightOptions(color = "black", weight = 2, bringToFront = TRUE)) %>% 
    addPolygons(group = "Pro-Democracy", data = dcca2, weight = 3, popup = popup2, stroke = TRUE, color = "Grey", fillColor = pal(var2), fillOpacity = 0.2,  highlightOptions = highlightOptions(color = "black", weight = 2, bringToFront = TRUE)) %>% 
    addLegend("bottomright", pal = pal, values = iv[!is.na(iv)], title = title, labFormat = labelFormat(prefix = prefix, suffix = suffix, big.mark = ','), opacity = 0.7) %>%
    addLayersControl(overlayGroups = c("Pro-Beijing", "Pro-Democracy"), options = layersControlOptions(collapsed = FALSE), position = "bottomleft")
  m
}

## Draw map to show the political majority of each constituency
pal <- result2018 %$% colorNumeric(palette = matlab.like2(length(unique(margin * 100))), na.color = "Grey", domain = -60:60)
#HK Island
lc1 <- result2018[result2018$gc == "LC1", ]
lc1_popup <- lc1 %$% paste0("<b> ", district, "</b> - <b>", zone_name, "</b><br>Political Camp: ", camp, "<br>Margin: ", abs(round(margin *100, 2)), "%<br>Median Income: $", median_income, "<br>Home Ownership Rate: ", round(own_prop * 100, 2), "%")
lc1_popup[is.na(lc1$margin)] %<>% sapply(., function(x) { #Replace NA with 'No Data' in popups
  gsub('[$]*NA[%]*', 'No Data', x)
})
#Recreate map with only the constituencies of HK Island
dcca_poly <- list()
n <- 1
for(i in 1:length(dcca2015)) {
  if(dcca2015@polygons[[i]]@ID %in% lc1$zone) {
    dcca_poly[[n]] <- data.frame(longitude = dcca2015@polygons[[i]]@Polygons[[1]]@coords[, 1], latitude = dcca2015@polygons[[i]]@Polygons[[1]]@coords[, 2])
    n <- n + 1
  }
}
dcca_lc1 <- SpatialPolygons(sapply(1:length(lc1$zone), function(x) Polygons(list(Polygon(dcca_poly[[x]])), lc1$zone[x])))
#Kowloon W
lc2 <- result2018[result2018$gc == "LC2", ]
lc2_popup <- lc2 %$% paste0("<b> ", district, "</b> - <b>", zone_name, "</b><br>Political Camp: ", camp, "<br>Margin: ", abs(round(margin *100, 2)), "%<br>Median Income: $", median_income, "<br>Home Ownership Rate: ", round(own_prop * 100, 2), "%")
lc2_popup[is.na(lc2$margin)] %<>% sapply(., function(x) { #Replace NA with 'No Data' in popups
  gsub('[$]*NA[%]*', 'No Data', x)
})
#Recreate map with only the constituencies of Kowloon W
dcca_poly <- list()
n <- 1
for(i in 1:length(dcca2015)) {
  if(dcca2015@polygons[[i]]@ID %in% lc2$zone) {
    dcca_poly[[n]] <- data.frame(longitude = dcca2015@polygons[[i]]@Polygons[[1]]@coords[, 1], latitude = dcca2015@polygons[[i]]@Polygons[[1]]@coords[, 2])
    n <- n + 1
  }
}
dcca_lc2 <- SpatialPolygons(sapply(1:length(lc2$zone), function(x) Polygons(list(Polygon(dcca_poly[[x]])), lc2$zone[x])))
#NT E
lc5 <- result2018[result2018$gc == "LC5", ]
lc5_popup <- lc5 %$% paste0("<b> ", district, "</b> - <b>", zone_name, "</b><br>Political Camp: ", camp, "<br>Margin: ", abs(round(margin *100, 2)), "%<br>Median Income: $", median_income, "<br>Home Ownership Rate: ", round(own_prop * 100, 2), "%")
lc5_popup[is.na(lc5$margin)] %<>% sapply(., function(x) { #Replace NA with 'No Data' in popups
  gsub('[$]*NA[%]*', 'No Data', x)
})
#Recreate map with only the constituencies of NT E
dcca_poly <- list()
n <- 1
for(i in 1:length(dcca2015)) {
  if(dcca2015@polygons[[i]]@ID %in% lc5$zone) {
    dcca_poly[[n]] <- data.frame(longitude = dcca2015@polygons[[i]]@Polygons[[1]]@coords[, 1], latitude = dcca2015@polygons[[i]]@Polygons[[1]]@coords[, 2])
    n <- n + 1
  }
}
dcca_lc5 <- SpatialPolygons(sapply(1:length(lc5$zone), function(x) Polygons(list(Polygon(dcca_poly[[x]])), lc5$zone[x])))
#Draw Map
m <- leaflet() %>% addTiles() %>% addProviderTiles(providers$CartoDB.Positron)
m %>% addPolygons(group = "Hong Kong Island", data = dcca_lc1, weight = 3, popup = lc1_popup, stroke = TRUE, color = "Grey", fillColor = pal(lc1$margin * 100), fillOpacity = 0.2,  highlightOptions = highlightOptions(color = "black", weight = 2, bringToFront = TRUE)) %>% 
  addPolygons(group = "Kowloon East", data = dcca_lc2, weight = 3, popup = lc2_popup, stroke = TRUE, color = "Grey", fillColor = pal(lc2$margin * 100), fillOpacity = 0.2,  highlightOptions = highlightOptions(color = "black", weight = 2, bringToFront = TRUE)) %>% 
  addPolygons(group = "New Territories East", data = dcca_lc5, weight = 3, popup = lc5_popup, stroke = TRUE, color = "Grey", fillColor = pal(lc5$margin * 100), fillOpacity = 0.2,  highlightOptions = highlightOptions(color = "black", weight = 2, bringToFront = TRUE)) %>% 
  addLegend("bottomright", pal = pal, values = -60:60, title = "% Difference<br>in Votes", labFormat = labelFormat(suffix = '%'), na.label = "No Data", opacity = 0.7) %>%
  addLayersControl(overlayGroups = c("Hong Kong Island", "Kowloon East", "New Territories East"), options = layersControlOptions(collapsed = FALSE), position = "bottomleft") %>% 
  saveWidget(., file="margin.html")
                                   
## Draw map showing median income, by political camp
pal <- colorBin("Spectral", result2018$median_income)
result2018 %$% draw_map(., median_income, "Median Income", '$', '') %>% 
  saveWidget(., file="income.html")

## Draw map showing property ownership, by political camp
pal <- colorBin("Spectral", result2018$own_prop * 100)
result2018 %$% draw_map(., own_prop * 100, "Home Ownership", '', '%') %>% 
  saveWidget(., file="ownership.html")
