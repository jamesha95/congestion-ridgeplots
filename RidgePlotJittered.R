#----- Set up and reading in the data -----

library(tidyverse)
library(ggplot2)
library(grattanCharts)
library(grattantheme)
library(ggridges)

RidgePlotJitter <- function(city_abbrev,              # Melb or Syd
                         x_lim = 100,              # The longest distance we want to plot
                         roof_raise = 0.15,        # Adjusts padding in the plot to avoid cutting off the top ridge
                         wide_break = 20,          # The major x axis breaks
                         narrow_break = 10,        # The minor x axis breaks
                         pt_alpha = 0.01,
                         pt_size = 0.01,
                         overlap = 1,
                         cbd = FALSE,              # Drivers who work in the CBD, or all drivers who live in the city?
                         gridlines = TRUE,         # Adds vertical gridlines
                         dual_x_axis = FALSE,      # Adds another x axis at the top for ease of reading without gridlines
                         plot_only = FALSE){       # Plots using the last run simulation; time-saving when adjusting aesthetics only
  
  city <- city_abbrev
  
  city_full <- switch(city_abbrev,
                      "Melb" = "Melbourne",
                      "Syd" = "Sydney")
  
  file <- if_else(cbd, paste0("Data/", city, "CBDFTDriverCats.csv"),
                  paste0("Data/", city, "FTDriverCats.csv"))
  
  data <- read.csv(file , header = TRUE) %>%
    rename("Negative income" = X0,
           "Nil income" = X1,
           "$1-$149" = X150,
           "$150-$299" = X300,
           "$300-$399" = X400,
           "$400-$499" = X500,
           "$500-$649" = X650,
           "$650-$799" = X800,
           "$800-$999" = X1000,
           "$1,000-$1,249" = X1250,
           "$1,250-$1,499" = X1500,
           "$1,500-$1,749" = X1750,
           "$1,750-$1,999" = X2000,
           "$2,000-$2,999" = X3000,
           "Over $3,000 per week" = more)
  
  #----- Simulating the distances to work -----   
  
  if(plot_only == FALSE){ 
    
    maxi = 76 #(choose 76 for distances up to 3000 km)
    #(choose 62 for up to 100 km)
    #(choose 43 for up to 50 km)
    
    maxj = 15 #(choose 15 to include top bracket, which I've said is $3k-$4k per week)
    #(choose 14 to include the $2k-$3k bracket)
    #(choose 13 to max out at $2k, as using a runif on $2k-$3k is unrealistic)
    
    mini = 1 #(We can now only simulate distances >0km)
    
    minj = 1 #(choose 1 to include negative incomes as $0 incomes)
    #(choose 2 to exclude negative incomes but keep $0)
    #(choose 3 to exclude nil incomes, forcing all incomes to be >$1)
    
    all_obs <- NULL # there's probably a more elegant solution, but I've opted for the for-for
    
    for(i in mini:maxi){
      for(j in minj:maxj){
        n <- data[i + 1, j + 1] # we ignore the first row of data (the 0km trips)
        if(n > 0){
          a <- data.frame(dist = runif(n, 
                                       min = data[i, 1],
                                       max = data[i + 1, 1]),
                          inc = names(data)[j + 1])
          all_obs <- rbind(all_obs, a)
          assign("all_obs", value = all_obs, envir = globalenv())
        }
      }
    } # This will take a while, generating values for the >80,000 drivers
    
    #----- Attaching sample sizes, proportions and labels -----
    
    totals <- all_obs %>% # this is to get the number of drivers in each income bracket
      group_by(inc) %>%
      summarise(total = n()) %>%
      mutate(frac = total/sum(total)) 
    
    # Now we need to sort our data into the right order
    
    brackets <- tribble(
      ~cat,  ~inc,                   ~inc_agg,
      1,     "Negative income",      "Below minimum wage",
      2,     "Nil income",           "Below minimum wage",
      3,     "$1-$149",              "Below minimum wage",
      4,     "$150-$299",            "Below minimum wage",
      5,     "$300-$399",            "Below minimum wage",
      6,     "$400-$499",            "Below minimum wage",
      7,     "$500-$649",            "Below minimum wage",
      8,     "$650-$799",            "$650-$799",
      9,     "$800-$999",            "$800-$999",
      10,    "$1,000-$1,249",        "$1,000-$1,249",
      11,    "$1,250-$1,499",        "$1,250-$1,499",
      12,    "$1,500-$1,749",        "$1,500-$1,749",
      13,    "$1,750-$1,999",        "$1,750-$1,999",
      14,    "$2,000-$2,999",        "$2,000-$2,999",
      15,    "Over $3,000 per week", "Over $3,000 per week"
    )
    
    # To plot correctly, I need the income brackets as ordered factors in the right order
    
    totals <- totals %>%
      mutate(inc = as.character(inc)) # dplyr seems to not like joining on factors, only characters
    
    totals <- inner_join(brackets, totals, by = "inc")
    totals <- totals %>%
      group_by(inc_agg) %>%
      mutate(frac.agg = sum(frac)) %>%
      ungroup() # we need to get proportions for the aggregated groups too
    
    # we want to know the number of observations in the income brackets too
    
    all_obs <- all_obs %>%
      mutate(inc = as.character(inc)) # dplyr seems to not like joining on factors, only characters
    
    all_obs <- left_join(totals, all_obs, by = "inc") # and back to factors for the sake of plotting
    all_obs$inc <- factor(all_obs$inc, levels = totals$inc[order(totals$cat)])
    all_obs$inc_agg <- factor(all_obs$inc_agg, # this is a bit clunky but we need the aggregated brackets as factors
                              levels = unique(totals$inc_agg))
    
    all_obs <- all_obs %>% 
      mutate(rel.weight.disagg = overlap*frac/max(frac)) %>% # this helps us distinguish large groups for setting alpha (fade)
      mutate(rel.weight.agg = overlap*frac.agg/max(frac.agg)) # this is for the aggregated groups
    
    extremes <- totals %>% # this is relevant so we can include the sample sizes in our chart caption
      filter(frac.agg %in% c(max(frac.agg), min(frac.agg))) %>% 
      group_by(inc_agg) %>%
      summarise(group_sample_size = sum(total), proportion = max(frac.agg)) %>%
      arrange(desc(proportion))
    
    assign("all_obs", value = all_obs, envir = globalenv()) # necessary for using the plot_only feature
    assign("extremes", value = extremes, envir = globalenv()) # necessary for using the plot_only feature
    
  }
  
  #----- Plotting ------ 
  
  print(extremes) # helpful to know what our largest and smallest income brackets are; also printed in the caption
  
  max_x = x_lim # what's our cut-off distance
  
  ggplot(all_obs, aes(x = dist, y = inc_agg)) +
    
    geom_density_ridges(col = gpal(1),
                        fill = gpal(1),
                        alpha = 0,
                        aes(scale = rel.weight.agg), # make this larger for overlapping ridges, e.g. 1.5
                        rel_min_height = 0.01,  #kills the density when it's too low, like a japanese painting
                        jittered_points = TRUE,
                        position = "points_sina",
                        point_alpha = pt_alpha,
                        point_size = pt_size,
                        color = gpal(3),
                        quantile_lines = TRUE) +
   
    theme_grattan() +
    
    theme(axis.text.y = element_text(vjust = 0)) +
    {if(gridlines) theme(panel.grid.minor.x = element_line(colour = grattan_gridlinegrey), # toggle x gridlines
                         panel.grid.major.x = element_line(colour = grattan_gridlinegrey))} +
    
    scale_x_continuous(limits = c(0, max_x), # sets the domain
                       breaks = seq(0, max_x, wide_break), #sets the major x axis 
                       minor_breaks = seq(0, max_x, narrow_break), # sets the minor x axis
                       expand = c(0.01, 0)) + # brings y axis closer and sets domain
                       {if(dual_x_axis) scale_x_continuous(limits = c(0, max_x), # sets the domain
                                                           breaks = seq(0, max_x, wide_break), #sets the major x axis 
                                                           minor_breaks = seq(0, max_x, narrow_break), # sets the minor x axis
                                                           expand = c(0.01, 0),
                                                           sec.axis = sec_axis(~., breaks = seq(0, max_x, wide_break)))} + # adds a duplicate x axis at the top
    
    scale_y_discrete(expand = expand_scale(mult = c(0, roof_raise))) + # puts bottom curve on the x axis and raises roof to avoid cut-off; set to 0.1 for reports
    
    xlab("Distance (km)") +
    
    ylab("") +
    
    # Here we have alternative captions/titles for the CBD charts vs non-CBD charts
    labs(title = paste0("Distances driven to work in ", 
                        if_else(cbd, 
                                paste0("the ", city_full, " CBD"), # Finishes the title if CBD
                                city_full)),                       # Finishes the title if non-CBD
         subtitle = "Total personal income, weekly",
         caption = paste0("Data is for full-time workers who ",
                          if_else(cbd,
                                  paste0("work in the ", city_full, " CBD"),
                                  paste0("live in Greater ", city_full)),
                          " and commuted by vehicle (as defined by the ABS) on census day, 2016. ",
                          "Lines indicate the median and 80th percentile. The '",
                          extremes[1, 1],
                          "' income bracket is the largest and contains ",
                          formatC(round(extremes[1, 2], digits = -2), format = "d", big.mark = ","),
                          " drivers, representing around ",
                          round(extremes[1, 3]*100, 0), 
                          "% of all drivers; the '",
                          extremes[2, 1],
                          "' income bracket is the smallest, containing ",
                          formatC(round(extremes[2, 2], digits = -2), format = "d", big.mark = ","),
                          " drivers and representing around ",
                          round(extremes[2, 3]*100, 0),
                          "% of all drivers."))
  
  # Save the output. This is the end of the function.
  grattan_save(filename = paste0("Ridgeplots/", "fullslide", "/Jitter", city, if_else(cbd, "CBD", ""), "Ridge.png"), 
               type = "fullslide")
  grattan_save(filename = paste0("Ridgeplots/", "normal", "/Jitter", city, if_else(cbd, "CBD", ""), "Ridge.png"), 
               type = "normal")
  
}

#----- Running the function and generating charts -----  

# Choose "normal" for reports, "fullslide" for presentation
# may need to play around with roof_raise depending on how many categories are used
# we need roof_raise because the top density in the ridge plot often gets cut-off
# this piece of code puts some padding at the top of the chart to avoid the issue

#if using "normal", minor gridlines won't display. So just use all major gridlines and set wide_break to 10

for(j in c(TRUE, FALSE)){
  for(i in c("Melb", "Syd")){
    RidgePlotJitter(city_abbrev = i,           # Melb or Syd
                 x_lim = 50,                # The longest distance we want to plot
                 roof_raise = 0.15,         # Adjusts padding in the plot to avoid cutting off the top ridge
                 wide_break = 10,           # The major x axis breaks
                 narrow_break = 10,         # The minor x axis breaks
                 overlap = 1,               # The extent to which the ridges overlap (1 = just touching)
                 cbd = j,                   # Drivers who work in the CBD, or all drivers who live in the city?
                 gridlines = FALSE,          # Adds vertical gridlines
                 dual_x_axis = TRUE,       # Adds another x axis at the top for ease of reading without gridlines
                 plot_only = FALSE)         # Plots using the last run simulation; time-saving when adjusting aesthetics only
  }
}
