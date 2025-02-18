install.packages("tidyverse")
library(tidyverse)
essentia.data = read.csv("data/essentia.data.csv") #data for all the tracks other than allentown
#view(essentia.data)
allentown.data = read.csv("data/essentia.data.allentown.csv")  #data for allentown
#view(allentown.data)

#function that determines whether Allentown is out of range, unusual, or within the range for each band
check_range = function(data){
  data |>
    group_by(artist) |>
    summarize(min = min(overall_loudness), LF = median(overall_loudness) - 2*IQR(overall_loudness), UF = median(overall_loudness) + 2*IQR(overall_loudness), max = max(overall_loudness)) |>
    mutate(out.of.range = (allentown.data$overall_loudness > max | allentown.data$overall_loudness < min)) |>
    mutate(unusual = (allentown.data$overall_loudness > UF | allentown.data$overall_loudness < LF)) |>
    mutate(description = case_when(out.of.range == TRUE ~ "Out of Range",
                                   unusual == TRUE ~ "Outlying",
                                   TRUE ~ "Within Range"))
}





#names = colnames(essentia.data)
#numeric.cols = select_if(essentia.data, is.numeric)
#for(feature in colnames(numeric.cols)){
 # check_range(essentia.data, feature)
#}
  



