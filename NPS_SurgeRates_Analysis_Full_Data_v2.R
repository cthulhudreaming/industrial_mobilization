###NPS Surge Data Analysis###


library(dplyr)
library(ggplot2)
library(ggthemes)
library(grid)
library(tidyr)


#Importing Data#

surge_34 <- read.csv("Surge_Dataset_34.csv", 
                     colClasses=c("Year"="character", "Investment.Type"="character"))

##Creating variables##

#Full data excluding anomalous values and NAs#

current_no_anomalous <- select(filter(surge_34, Anomalous == 0, Year == 2020), 
                               c(Year:Agg.Max.Years))

current_no_anomalous_nas <- current_no_anomalous %>% drop_na(Adjusted.Surge.Rate.Years)

no_anomalous <- select(filter(surge_34, Anomalous == 0), c(Year:Agg.Max.Years))

no_anomalous_nas <- no_anomalous %>% drop_na(Adjusted.Surge.Rate.Years)

#MDAP data and non-MDAP data#
all_mdaps_data <- select(filter(surge_34, MDAP == "MDAP", Anomalous == 0), 
                         c(Year:Agg.Max.Years))

current_mdaps_data <- select(filter(surge_34, MDAP == "MDAP", Anomalous == 0, Year == 2020), 
                             c(Year:Agg.Max.Years))

past_mdaps_data <- select(filter(all_mdaps_data_nas, Year == 1999), 
                          c(Year:Agg.Max.Years))
summary(past_mdaps_data$Adjusted.Surge.Rate.Years)

current_non_mdaps_data <- select(filter(surge_34, 
                                        MDAP == "Non-MDAP", 
                                        Anomalous == 0, 
                                        Year == 2020), 
                                 c(Year:Agg.Max.Years))

current_non_mdaps_data_nas <- current_non_mdaps_data %>% drop_na(Adjusted.Surge.Rate.Years)

all_mdaps_data_nas <- all_mdaps_data %>% drop_na(Adjusted.Surge.Rate.Years)



##Question 1##
#Histogram of 2020 MDAPS at 1-8-5 Rate#
ggplot(current_mdaps_data, 
       aes(Efficiency.Production)) + 
  geom_histogram(binwidth = 1.2, 
                 colour="black", 
                 fill="dodgerblue1") + 
  scale_x_continuous(name = "Years to Replace Inventory", 
                     breaks = seq(0,30, 5), 
                     limits = (c(0,30))) + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  ylab("Number of Programs") +
  theme_economist()

#Density plot of 2020 MDAPS at 1-8-5 Rate#
ggplot(current_mdaps_data, 
       aes(Efficiency.Production)) + 
  geom_density(alpha=.5, 
               colour="black", 
               fill="dodgerblue1") + 
  scale_x_continuous(name = "Years to Replace Inventory", 
                     breaks = seq(0,30, 5), 
                     limits = (c(0,30))) + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  ylab("Proportion of Programs") +
  theme_economist()

##Question 2##
#Histogram of 2020 MDAPS at Adjusted Surge Rate#
ggplot(current_mdaps_data, 
       aes(Adjusted.Surge.Rate.Years)) + 
  geom_histogram(binwidth = 1.2, 
                 colour="black", 
                 fill="dodgerblue1") + 
  scale_x_continuous(name = "Years to Replace Inventory", 
                     breaks = seq(0,30, 5), 
                     limits = (c(0,30))) +
  theme(plot.title = element_text(hjust = 0.5)) + 
  ylab("Number of Programs") +
  theme_economist()

#Density plot of 2020 MDAPS at Adjusted Surge Rate#
ggplot(current_mdaps_data, 
       aes(Adjusted.Surge.Rate.Years)) + 
  geom_density(alpha=.5, 
               colour="black", 
               fill="dodgerblue1") + 
  scale_x_continuous(name = "Years to Replace Inventory", 
                     breaks = seq(0,30, 5), 
                     limits = (c(0,30))) +
  theme(plot.title = element_text(hjust = 0.5)) + 
  ylab("Proportion of Programs") +
  theme_economist()

##Question 3##
#2020 MDAPs vs. 1999 MDAPs#
ggplot(all_mdaps_data_nas, 
       aes(Adjusted.Surge.Rate.Years, 
           group = Year, 
           fill = Year)) + 
  geom_density(alpha=.5) + 
  scale_x_continuous(name = "Years to Replace Inventory", 
                     breaks = seq(0,30, 5), 
                     limits = (c(0,30))) +
  theme(plot.title = element_text(hjust = .5)) + 
  ylab("Proportion of Programs") +
  theme_economist()

ggplot(all_mdaps_data_nas, 
       aes(Adjusted.Surge.Rate.Years, 
           group = Year, 
           fill = Year)) + 
  geom_histogram(alpha=.5) + 
  scale_x_continuous(name = "Years to Replace Inventory", 
                     breaks = seq(0,30, 5), 
                     limits = (c(0,30))) +
  ggtitle("2020 vs. 1999: Time to Replace MDAP Inventories at Adjusted Surge Rate") + 
  theme(plot.title = element_text(hjust = .5)) + 
  ylab("Proportion of Programs") +
  theme_economist()

##Question 4##
##Different Program Types##
ggplot(current_no_anomalous_nas, 
       aes(Adjusted.Surge.Rate.Years, 
           group = Investment.Type, fill = Investment.Type)) + 
  geom_density(alpha=.5) + 
  scale_x_continuous(name = "Years to Replace Inventory", 
                     breaks = seq(0,30, 5), 
                     limits = (c(0,30))) +
  theme(plot.title = element_text(hjust = .5)) + 
  ylab("Proportion of Programs") +
  facet_wrap(. ~ Investment.Type) +
  theme_economist()

#By Investment.Type#
ars_data <- select(filter(current_no_anomalous_nas, Investment.Type == "ARS"), 
                   c(Year:Agg.Max.Years))
summary(ars_data$Adjusted.Surge.Rate.Years)

c4i_data <- select(filter(current_no_anomalous_nas, Investment.Type == "C4I"), 
                   c(Year:Agg.Max.Years))
summary(c4i_data$Adjusted.Surge.Rate.Years)

gs_data <- select(filter(current_no_anomalous_nas, Investment.Type == "GS"), 
                  c(Year:Agg.Max.Years))
summary(gs_data$Adjusted.Surge.Rate.Years)

mm_data <- select(filter(current_no_anomalous_nas, Investment.Type == "MM"), 
                  c(Year:Agg.Max.Years))
summary(mm_data$Adjusted.Surge.Rate.Years)

msa_data <- select(filter(current_no_anomalous_nas, Investment.Type == "MSA"), 
                   c(Year:Agg.Max.Years))
summary(msa_data$Adjusted.Surge.Rate.Years)

sbs_data <- select(filter(current_no_anomalous_nas, Investment.Type == "SBS"), 
                   c(Year:Agg.Max.Years))
summary(sbs_data$Adjusted.Surge.Rate.Years)


##Question 5##
#Density plot of 2020 MDAP vs. Non-MDAP
ggplot(current_no_anomalous_nas, 
       aes(Adjusted.Surge.Rate.Years, group = MDAP, fill = MDAP)) + 
  geom_density(alpha=.5) + 
  scale_x_continuous(name = "Years to Replace Inventory", 
                     breaks = seq(0,30, 5), 
                     limits = (c(0,30))) +
  theme(plot.title = element_text(hjust = 0.5)) + 
  ylab("Proportion of Programs") +
  theme_economist()

non_mdaps <- filter(select(current_no_anomalous_nas, MDAP == "MDAP"), 
                    c(Year:Agg.Max.Years))

#Wheeled vehicles#

ggplot(current_no_anomalous_nas, aes(Adjusted.Surge.Rate.Years, 
                                     group = Wheeled.Vehicles, fill = Wheeled.Vehicles)) + 
  geom_density(alpha=.5) + 
  scale_x_continuous(name = "Years to Replace Inventory", 
                     breaks = seq(0,30, 5), limits = (c(0,30))) +
  theme(plot.title = element_text(hjust = 0.5)) + 
  ylab("Proportion of Programs") +
  theme_economist()


wheeled_data <- select(filter(current_no_anamlous_nas, 
                              Wheeled.Vehicles == "Wheeled Vehicles"), 
                       c(Year:Agg.Max.Years))
summary(wheeled_data$Adjusted.Surge.Rate.Years)

other_programs_data <- select(filter(current_no_anamlous_nas, 
                                     Wheeled.Vehicles == "Other Programs"), c(Year:Agg.Max.Years))
summary(other_programs_data$Adjusted.Surge.Rate.Years)








