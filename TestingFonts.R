library(devtools)
library(tidyverse)
library(ggpomological)

MB <- readxl::read_xlsx("C:\\Users\\ikenn\\Downloads\\MarketBasketLocations.xlsx")

FinalData %>%
  filter(state_abbv == "MI") %>%
  ggplot(aes(COUNTYNM, CORP, fill = FAMILY)) +
     geom_col() + 
  scale_fill_pomological() +
  theme_pomological("Homemade Apple", )

# State-by Violin & Jittered Scatter Plot for log10(MCF)
TPO_Data %>% 
  filter(MILL_STATE %in% c("CT", "MA", "ME", "RI", "NH", "VT", "NY")) %>%
ggplot(aes(0, y = TOT_MCF_LOG, color = MILL_STATE)) + 
  geom_point() + 
  # Width controls 'width' of jittered points, maximum of .5 (i.e. .5 in each direction = 1)
  geom_jitter(width = 0.5, size = 2.5) +
  scale_color_pomological() +
  scale_y_continuous(breaks = c(seq(-2,5,1)), limits = c(-2,5)) + 
  ggtitle("Log(10) of MCF by State") +  
  #theme_fivethirtyeight(base_size = 30, base_family = 'serif') +
  theme_pomological_fancy(base_family = "Agency FB", 16) +
  # theme(axis.title = element_text(family = 'serif', size = 20), 
  #       legend.position = "none",
  #       axis.text.x = element_blank(), 
  #       panel.grid.major.x = element_blank(), 
  #       axis.text.y = element_text(size = 20)) + 
  ylab("Log(10) MCF") + xlab('State') + 
  facet_wrap(facets = vars(MILL_STATE), ncol = 8)

library(sysfonts)
library(extrafont)


# Create a Violin Plot of Mill Type by Volume
ggplot(MTC_DF, aes(x = MTC_Tidy, y = TOT_MCF_LOG, fill = MTC_Tidy)) + 
  # Draw 25, 50, and 75 % quantiles, alpha = .5 for semi-transparent plot
  geom_violin(alpha = .5, draw_quantiles = c(.25, .5, .75)) +
  scale_y_continuous(breaks = c(seq(-2,5,.5)), limits = c(-2,5)) + 
  labs(title = "Log(10) MCF by Mill Type", y = "Log(10) MCF", 
       subtitle = "Lines within plots refer to 25th, 50th, & 75th quantiles.", 
       x = "Mill Type") +
  scale_fill_pomological() +
  theme_pomological_fancy(base_family = "Homemade Apple", 16) +
  #theme_fivethirtyeight(base_size = 20, base_family = 'serif') +
  theme(axis.title = element_text(size = 25),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 20),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 15),
        axis.title.x = element_blank()) +
  # Create a legend for Mill Types
  guides(fill = guide_legend(title = "Mill Type"))


# Create a Bar Plot of Mill Types by Count (Sample-Frame wide)
ggplot(MTCCounts, aes(x = MTC_Tidy, y = Count, fill = MTC_Tidy)) + 
  geom_col(width = .7) +
  scale_y_continuous(breaks = c(seq(0,2500,250)), limits = c(0,2700)) + 
  scale_fill_pomological() +
  labs(title = "Mill Count by Mill Type", x = "Mill Type", y = "Mill Count") +
  theme_pomological("Homemade Apple", 16) +
  #theme_fivethirtyeight(base_size = 60, base_family = 'serif') + 
  theme(axis.title = element_text(size = 20), axis.text.y = element_text(size = 15),
        axis.text.x = element_blank(), axis.title.x = element_blank(), panel.grid.major.x = element_blank(), 
        legend.text = element_text(size = 10), legend.title = element_text(size = 15)) +
  geom_text(aes(label = Count, family = "Homemade Apple"), size = 4 , nudge_y = 75) +
  # Create a legend for Mill Type
  guides(fill = guide_legend(title = "Mill Type"))

Omit_DF %>%
  ggplot(aes(Percent_Omitted, Omit_Volume_Vector, 
             # Change color of points by omission threshold
             color = as.character(round(Omit_Vec, 2)))) +
  geom_point(size = 1) +
  # Create a smooth line for connecting all points...No error bars of course!
  geom_smooth(size = .8, se = FALSE, color = 'black') +
  # Limit x-scale (Percent of Mill Count lost) to 0-28%, this may need to be changed in the future!
  scale_x_continuous(breaks = c(seq(0,28,2)), limits = c(0,28)) +
  # Limit y-scale (Volume Omitted - MCF) to 0-2,100 MCF, this may need to be changed in the future!
  scale_y_continuous(labels = scales::comma, limits = c(0,2100), breaks = c(seq(0,2100,100)),
                     # Create a secondary y-axis (right side) that displays volume lost as a percentage of the aggregate sample volume
                     sec.axis =  sec_axis(~.*100/TOT_Volume_Vector, breaks = c(seq(0,.16,.01)), name = "% of Aggregate Volume (MCF) Omitted")) +
  # Create a threshold-volume based scale for the legend, thresholds will be ordered from smallest to largest in the legend.
  scale_color_discrete(name = "Omission Threshold (MCF)", labels = function(x) format(sort(as.numeric(x)), big.mark = ",", scientific = FALSE)) +
  labs(title = "Omitted Volumes (MCF) & Mill Counts at Varying Thresholds", subtitle = "Colored #'s refer to the # of mills dropped at each threshold") +
  # Label each point by the count of mills dropped at each threshold, nudge_y = y-adjustment from point for the label, nudge_x = x-adjustment from point for the label
  geom_text(aes(label = Omit_Mill_Count_Vector), size =  4, nudge_y = 100, show.legend = FALSE) +
  #theme_fivethirtyeight(base_size = 20, base_family = 'serif') +
  theme_fivethirtyeight() +
  # plot.margin allows for additional space outside the plot for placement of the secondary y-axis (volume lost as a percentage of the aggregate sample volume)
  theme(axis.title = element_text(family = 'serif', size = 15), plot.margin = unit(c(1,1,1,1), "cm"), 
        axis.title.y.right = element_text(vjust=unit(1.5, "cm")), axis.text = element_text(size = 15),
        legend.text = element_text(size = 10), legend.title = element_text(size = 15)) + 
  ylab("Aggregate Volume (MCF) Omitted") + xlab('% of Mills Omitted')  
