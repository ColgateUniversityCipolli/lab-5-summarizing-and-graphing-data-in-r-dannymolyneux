install.packages("tidyverse")
library(tidyverse)
essentia.data = read.csv("data/essentia.data.csv") #data for all the tracks other than allentown
#view(essentia.data)
allentown.data = read.csv("data/essentia.data.allentown.csv")  #data for allentown
#view(allentown.data)

#function that determines whether a given feature in Allentown is out of range, unusual, or within the range for each band
check_range = function(feature){
  essentia.stats = essentia.data |> #defining stats for each feature, grouped by artist
    group_by(artist) |>
    summarize(min = min(get(feature), na.rm = TRUE), LF = median(get(feature), na.rm = TRUE) - 2*IQR(get(feature), na.rm = TRUE),
              UF = median(get(feature), na.rm = TRUE) + 2*IQR(get(feature), na.rm = TRUE), 
              max = max(get(feature), na.rm = TRUE))
  essentia.stats |>
    mutate(
      out.of.range = (allentown.data[[feature]] > max | allentown.data[[feature]] < min),  #considered out of range
      unusual = (allentown.data[[feature]] > UF | allentown.data[[feature]] < LF),  #considered unusual
      description = case_when(out.of.range == TRUE ~ "Out of Range",  
                              unusual == TRUE ~ "Outlying",
                              TRUE ~ "Within Range")  #within range if not unusual or out of range
    )
}

#step 2 (apply the function to all of the data)
numeric.cols = select_if(essentia.data, is.numeric)  #list of only numeric columns in essentia.data
range_results <- list()  #empty list that will store the results for each feature

for(feature in colnames(numeric.cols)) {  #goes through every numeric column
  range_results[[feature]] <- check_range(feature)  #adds the info for that feature to the results list
}



