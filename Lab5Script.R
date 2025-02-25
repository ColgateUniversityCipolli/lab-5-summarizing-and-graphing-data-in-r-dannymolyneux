install.packages("tidyverse")
library(tidyverse)
essentia.data = read.csv("data/essentia.data.csv") #data for all the tracks other than allentown
view(essentia.data)
allentown.data = read.csv("data/essentia.data.allentown.csv")  #data for allentown
view(allentown.data)

#function that determines whether a given feature in Allentown is out of range, unusual, or within the range for each band
check_range = function(data, feature){
  essentia.stats = data |> #defining stats for each feature, grouped by artist
    group_by(artist) |>
    summarize(min = min(get(feature), na.rm = TRUE), LF = median(get(feature), na.rm = TRUE) - 2*IQR(get(feature), na.rm = TRUE),
              UF = median(get(feature), na.rm = TRUE) + 2*IQR(get(feature), na.rm = TRUE), 
              max = max(get(feature), na.rm = TRUE)) |>
    mutate(
      #pulls the value for that feature in allentown and stores it as a variable
      allentown_feature = allentown.data |> pull(get(feature)), 
      out.of.range = (allentown_feature > max | allentown_feature < min),  #considered out of range
      unusual = (allentown_feature > UF | allentown_feature < LF),  #considered unusual
      description = case_when(out.of.range == TRUE ~ "Out of Range",  
                              unusual == TRUE ~ "Outlying",
                              TRUE ~ "Within Range")  #within range if not unusual or out of range
    )
  return(essentia.stats)
}

#step 2 (apply the function to all of the data)
numeric.cols = names(essentia.data)[sapply(essentia.data, is.numeric)] #list of only numeric columns in essentia.data
not.numeric = setdiff(names(essentia.data), numeric.cols)
not.numeric
range_results <- data.frame()  #empty list that will store the results for each feature
for(feature_name in numeric.cols) {  #goes through every numeric column
  feature_data = check_range(essentia.data, feature_name)  #adds the info for that feature to a data frame
  feature_data$feature = feature_name #adds a column that denotes the feature
  #keeps adding the data for each feature to the overall data set
  range_results = bind_rows(range_results, feature_data)
}
view(range_results)

#step 3 (xtable)
library(xtable)
print(xtable(range_results,                       # Table to print
             caption = "Results of each essentia feature vs Allentown", # Caption for the table
             label = "range_results:reference"),   # Label to reference 
      table.placement = "H")  
installed.packages("dplyr")
library(dplyr)

#step 4 (plots)

#groups by artist and description so that I can calculate proportions
description_counts <- range_results %>%
  group_by(artist, description) %>%
  summarise(count = n(), .groups = "drop")

#gets proportion of each description for each artist
description_counts <- description_counts %>%
  group_by(artist) %>%
  mutate(proportion = count / sum(count))

#side by side pie charts of description proportions for each artist
pie.chart<-ggplot(description_counts)+
  geom_bar(aes(x="", 
               y=proportion,
               fill=description), stat = "identity")+
  coord_polar("y", start=0)+
  theme_bw()+
  xlab("")+
  ylab("")+
  labs(fill="")+
  facet_wrap(~artist)

#side by side pie charts of description proportions for each artist
doughnut.plot<-ggplot(description_counts)+
  geom_bar(aes(x=2, 
               y=proportion,
               fill=description), stat = "identity")+
  coord_polar("y", start=0)+
  xlim(0.2, 2.5)+
  theme_bw()+
  xlab("")+
  ylab("")+
  labs(fill="")+
  facet_wrap(~artist)+
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank())

#side-by-side column plot of the proportions of each description for each artist
column.plot <- ggplot(data=description_counts)+
  geom_col(aes(x=artist, y = proportion, fill=description),    
           position = position_dodge(.9)) + 
  geom_hline(yintercept = 0)+
  xlab("Artist")+                        
  ylab("Proportion")+                       
  ylim(0,1)+                          
  theme_bw() 

#Four features I am interested in
significant_features = c("overall_loudness", "danceability", "duration", "emotion")
significant_results = range_results |>
  filter(feature %in% significant_features) |> #filters out the other features
  select(artist, feature, min, max, LF, UF, allentown_feature)  #stats needed for box plot


#side by side box plots for the 4 features I chose
box.plot = ggplot(data=significant_results,   
       aes(x=artist, y=allentown_feature)) +    
  geom_boxplot(aes(ymin = min, lower = LF, upper = UF, middle = (LF+UF)/2, ymax = max), stat = "identity", width = 0.25) +
  theme_bw()+ 
  geom_point(size = 3, color = "blue")+  #places a point where the allentown value is
  facet_wrap(~feature, scales = "free")+
  xlab("Artist")+                       
  ylab("Feature value")+   
  ggtitle("Boxplot Comparison of Artist's For Important Features")



pie.chart
doughnut.plot
column.plot
box.plot





  


