setwd("/Users/bezaitis/Desktop/school/FA_25/Sandia Data Challenge/")
library(ggplot2)
library(patchwork)



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

recycled_total <- sum(all_data$Powder == 'Recycled')
recycled_total_scrap <- sum(all_data$Powder == 'Recycled' & all_data$is_scrap == 'TRUE')
ratio_recycled <- recycled_total_scrap / recycled_total

virgin_total <- sum(all_data$Powder == 'Virgin')
virgin_total_scrap <- sum(all_data$Powder == 'Virgin' & all_data$is_scrap == 'TRUE')
ratio_virgin <- virgin_total_scrap / virgin_total

powder_scrap_ratios <- c(ratio_recycled, ratio_virgin)
names(powder_scrap_ratios) <- c("Recycled", "Virgin")

powder_scrap_ratios <- barplot(powder_scrap_ratios, 
                        main = 'Scrap % by powder', 
                        xlab = 'Powder Type', 
                        ylab = '% scrap', 
                        col = c('steelblue', 'white'))

total_6x6 <- sum(all_data$Layout == '6X6')
total_6x6_scrap <- sum(all_data$Layout == '6X6' & all_data$is_scrap == 'TRUE')
ratio6x6 <- total_6x6_scrap/total_6x6

total_6x6TA <- sum(all_data$Layout == '6X6TA')
total_6x6TA_scrap <- sum(all_data$Layout == '6X6TA' & all_data$is_scrap == 'TRUE')
ratio6x6TA <- total_6x6TA_scrap/total_6x6TA

total_11x11TA <- sum(all_data$Layout == '11X11TA')
total_11x11TA_scrap <- sum(all_data$Layout == '11X11TA' & all_data$is_scrap == 'TRUE')
ratio11x11TA <- total_11x11TA_scrap/total_11x11TA

layout_scrap_ratios <- c(ratio6x6,ratio6x6TA,ratio11x11TA)

names(layout_scrap_ratios) <- c("ratio6x6","ratio6x6TA","ratio11x11TA")
chart_layout_scrap <- barplot(layout_scrap_ratios, 
                        main = 'Scrap % by plate layout', 
                        xlab = 'Layout', 
                        ylab = '% scrap', 
                        col = c('steelblue', 'white', 'purple'))

total_layout_count <- c(total_6x6TA,total_11x11TA,total_6x6)
names(total_layout_count) <- c("6x6TA","11x11TA", "6x6")
chart_pie <- pie(total_layout_count, main = "CUPs produced by layout")


assign_week <- function(dat) {
  # define groups
  week1 <- c("A", "B", "C", "D", "E")
  week2 <- c("F", "G", "H", "I", "J")
  week4v <- c("K")
  week4r <- c("L", "M")
  week5r <- c("N", "O", "P", "Q", "R")
  
  # start a new column
  dat$week <- NA
  
  # match against Layout
  dat$week[dat$PlateID %in% week1]  <- 1
  dat$week[dat$PlateID %in% week2]  <- 2
  dat$week[dat$PlateID %in% week4v] <- 4  # or 41 if you want to separate
  dat$week[dat$PlateID %in% week4r] <- 4  # same week, different subtype
  dat$week[dat$PlateID %in% week5r] <- 5
  
  dat
}

all_data <- assign_week(all_data)

plots <- lapply(names(specs), function(var) {
  range_vals <- specs[[var]]
  
  ggplot(all_data, aes_string(x = var, y = "week", color = "Layout")) +
    geom_point(size = 2.5) +
    geom_vline(xintercept = range_vals, linetype = "dashed", color = "black") +
    labs(
      title = paste(var, "vs Week"),
      x = var,
      y = "Week",
      color = "Layout"
    ) +
    theme_minimal()
})
names(plots) <- names(specs)
wrap_plots(plots, ncol = 2)


model <- glm(is_scrap ~ factor(Layout) + Powder + Nonconformity,
             data = all_data,
             family = binomial)
summary(model)

colnames(all_data)


sapply(all_data[, grep("_in_spec", names(all_data))], function(x) table(all_data$is_scrap, x))
