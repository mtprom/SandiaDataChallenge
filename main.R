setwd("/Users/bezaitis/Desktop/school/FA_25/Sandia Data Challenge/")
library(ggplot2)


all_data_recycled <- read.csv('AllData_PreEDM_Recycled_RowColIDs.csv')
all_data_virgin <- read.csv('AllData_PreEDM_Virgin_RowColIDs.csv')
recycled_vol <- read.csv('recycled_volume.csv')
virgin_vol_1 <- read.csv('virgin_volume_pt1.csv')
virgin_vol_2 <- read.csv('virgin_volume_pt2.csv')



all_data <- rbind(all_data_recycled, all_data_virgin)
all_data

renamed <- list('LID', # Lip interior diameter
                'LED', # Lip exterior diameter            
                'FH',  # Floor height
                'LH',  # Lip height
                'LT1', # Lip thickness 1
                'LT2', # Lip thickness 2
                'LT3', # Lip thickness 3
                'LT4') # Lip thickness 4

colnames(all_data)[2:9] <- renamed


specs <- list(
  LID = c(0.415, 0.435),      # Lip interior diameter
  LED = c(0.445, 0.469),            # Lip exterior diameter  
  FH = c(0.049, 0.069),    # Floor height
  LH = c(0.261, 0.281),       # Lip height
  LT1 = c(0.010, 0.017),       # Lip thickness 1
  LT2 = c(0.010, 0.017),       # Lip thickness 2
  LT3 = c(0.010, 0.017),       # Lip thickness 3
  LT4 = c(0.010, 0.017)        # Lip thickness 4
)

check_in_spec <- function(value, lower, upper) {
  value >= lower & value <= upper
}

for (param in names(specs)) {
  lower <- specs[[param]][1]
  upper <- specs[[param]][2]
  
  col_name <- paste0(param, "_in_spec")
  all_data[[col_name]] <- check_in_spec(all_data[[param]], lower, upper)
}

spec_columns <- paste0(names(specs), "_in_spec")
all_data$overall_in_spec <- apply(all_data[, spec_columns], 1, all)

all_data$is_scrap <- !all_data$overall_in_spec


total_6x6 <- sum(all_data$Layout == '6X6')
total_6x6_scrap <- sum(all_data$Layout == '6X6' & all_data$is_scrap == 'TRUE')
ratio6x6 <- total_6x6_scrap/total_6x6

total_6x6TA <- sum(all_data$Layout == '6X6TA')
total_6x6TA_scrap <- sum(all_data$Layout == '6X6TA' & all_data$is_scrap == 'TRUE')
ratio6x6TA <- total_6x6TA_scrap/total_6x6TA

total_11x11TA <- sum(all_data$Layout == '11X11TA')
total_11x11TA_scrap <- sum(all_data$Layout == '11X11TA' & all_data$is_scrap == 'TRUE')
ratio11x11 <- total_11x11_scrap/total_11x11

names(ratios) <- c("ratio6x6","ratio6x6TA","ratio11x11")
layout_scrap <- barplot(ratios, 
                        main = 'Scrap % by plate layout', 
                        xlab = 'Layout', 
                        ylab = '% scrap', 
                        col = c('steelblue', 'white', 'purple'))











