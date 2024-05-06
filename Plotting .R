library(ggplot2)



# Create histogram using ggplot2
ggplot(average_finalcost_by_dshospid_DRG807, aes(x = AverageFinalCost)) +
  geom_histogram(binwidth = 200, fill = "blue", color = "black") +  
  labs(title = "Histogram of Average Final Cost for Low-Risk Vaginal Delivery",
       x = "Average Facility Cost per Hospital",
       y = "Number of Hospitals") +
  theme_minimal()


ggplot(average_finalcost_by_dshospid_DRG788, aes(x = AverageFinalCost)) +
  geom_histogram(binwidth = 200, fill = "blue", color = "black") +  
  labs(title = "Histogram of Average Final Cost for Low-Risk Cesearean Section",
       x = "Average Facility Cost per Hospital",
       y = "Number of Hospitals") +
  theme_minimal()



