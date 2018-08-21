# Visualization Practice
# Set environment ------------------------------------------------
library(dplyr)
library(ggplot2)
library(data.table)

climate_folder <- "D:\\Datasets\\India\\Climate\\"

# Load data ------------------------------------------------------
India_Temp <- fread(input = paste(climate_folder, "month_seas_ann_mean_temp_India_1901_2016.csv", sep = ""))
India_Rain <- fread(input = paste(climate_folder, "sub-division_rainfall_act_dep_1901-2015.csv", sep = ""))

# Visualize Temperature Trends ------------------------------------------------------
India_Temp[, c("ANNUAL", "JAN-FEB", "MAR-MAY", "JUN-SEP", "OCT-DEC") := NULL]
India_Temp_melt <- melt(data = India_Temp, id.vars = "YEAR", variable.name = "Month", value.name = "Temp")
India_Temp_melt[, Date := as.Date(paste(YEAR, Month, "01", sep = "-"), format = "%Y-%b-%d")]
India_Temp_melt[, Month_num := month(Date)]
India_Temp_mean_annual <- India_Temp_melt %>% group_by(YEAR) %>% summarise(Mean_Temp = mean(Temp))

# Visualize Mean Annual Temperature
ggplot(data = India_Temp_mean_annual, 
       aes(x = YEAR, 
           y = Mean_Temp, 
           colour = Mean_Temp)) +
  geom_point(size = 2) +
  geom_line(colour = "grey", 
            size = 1.4, 
            alpha = 0.5) +
  geom_smooth(colour = "maroon", 
              se = F, 
              size = 1.5,
              method = "loess") +
  scale_color_gradient2(midpoint = median(range(India_Temp_mean_annual$Mean_Temp)), 
                        low = "blue", 
                        mid = "orange", 
                        high = "red") +
  labs(x = "Year", 
       y = "Mean Annual Temperature (°C)", 
       title = "Mean Annual Temperature of India from 1901-2016",
       caption = "Data Source: https://data.gov.in", 
       colour = "Colour Scale (°C)") +
  theme(plot.title = element_text(size = 15, 
                                  hjust = 0.5), 
        legend.title = element_text(size = 10),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.text = element_text(size = 6, angle = 45),
        plot.caption = element_text(size = 7, 
                                    face = "italic"),
        panel.background = element_rect(colour = "white",
                                        size = 0.9,
                                        fill = "lightgrey"),
        panel.grid.major = element_line(colour = "lavender", 
                                        size = 0.3),
        panel.grid = element_blank())

# Visualize Monthly temperature trends
ggplot(data = India_Temp_melt, 
       aes(x = Date, 
           y = Temp, 
           colour = Month)) +
  geom_line(size = 1, 
            alpha = 0.5) +
  scale_color_brewer(type = "qualitative", palette = "Paired") +
  labs(x = "Year", 
       y = "Mean Monthly Temperature (°C)",
       title = "Mean Monthly Temperatures of India from 1901-2016",
       caption = "Data Source: https://data.gov.in", 
       colour = "Month") + 
  theme(plot.title = element_text(size = 15, 
                                  hjust = 0.5), 
        legend.title = element_text(size = 10),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.text = element_text(size = 6, angle = 0),
        plot.caption = element_text(size = 7, 
                                    face = "italic"),
        panel.background = element_rect(colour = "white",
                                        size = 0.9,
                                        fill = "lightgrey"),
        panel.grid.major = element_line(colour = "lavender", 
                                        size = 0.1),
        panel.grid = element_blank())

# Visualize Cyclical trends through the year
ggplot(data = India_Temp_melt, 
       aes(x = Month, 
           y = Temp, 
           colour = YEAR)) + 
  geom_line(aes(group = YEAR), 
            size = 1.2, 
            alpha = 0.3) + 
  scale_color_gradient2(midpoint = mean(India_Temp_melt$YEAR),
                        low = "blue", 
                        mid = "light yellow",
                        high = "red") +
  labs(x = "Month", 
       y = "Mean Temperature (°C)",
       title = "Seasonal Temperature trends in India from 1901-2016",
       caption = "Data Source: https://data.gov.in", 
       colour = "Year") + 
  theme(plot.title = element_text(size = 15, 
                                  hjust = 0.5), 
        legend.title = element_text(size = 10),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.text = element_text(size = 6, angle = 45),
        plot.caption = element_text(size = 7, 
                                    face = "italic"),
        panel.background = element_rect(colour = "white",
                                        size = 0.9,
                                        fill = "lightgray"),
        panel.grid.major = element_line(colour = "lavender", 
                                        size = 0.1),
        panel.grid = element_blank())

# Visualize Rainfall Trends ------------------------------------------------
# Remove unwanted rows
India_Rain <- India_Rain[YEAR != "1901-2015" & Parameter != "No. of districts"]
India_Rain[, c("ANNUAL", "JF", "MAM", "JJAS", "OND") := NULL]
# Convert Year from chr to num
India_Rain[, YEAR := as.numeric(YEAR)]
# Melt the data table to long
India_Rain_melt <- melt(data = India_Rain, 
                        id.vars = c("SUBDIVISION", "YEAR", "Parameter"), 
                        variable.name = "Month", 
                        value.name = "Value")
# Recode some factor levels
India_Rain_melt$SUBDIVISION <- recode(India_Rain_melt$SUBDIVISION, "MATATHWADA" = "MARATHWADA")
# Calculate normal rainfall for each region and month
India_Rain_cast <- dcast(data = India_Rain_melt, 
                         formula = SUBDIVISION + Month + YEAR ~ Parameter, 
                         value.var = "Value")
India_Rain_cast[, Normal := (Actual * 100)/(`Percentage departure` + 100)]
# NaN values are present. Find mean of rest of values by region and month
India_Rain_normal <- India_Rain_cast %>% 
  group_by(SUBDIVISION, Month) %>%
  summarise(Average = mean(Normal, na.rm = T))
# Map back to cast
India_Rain_cast <- as.data.table(left_join(x = India_Rain_cast,
                             y = India_Rain_normal, 
                             by = c("SUBDIVISION", "Month")))
# Rename columns
India_Rain_cast[, Normal := Average]
India_Rain_cast[, Average := NULL, `Percentage departure` := NULL]

# Melt back
India_Rain_melt <- melt(data = India_Rain_cast, 
                        id.vars = c("SUBDIVISION", "YEAR", "Month"), 
                        variable.name = "Parameter", 
                        value.name = "Value")

# Find mean annual rainfall by Region
India_Rain_annual_division <- India_Rain_melt %>% 
  group_by(SUBDIVISION, YEAR, Parameter) %>%
  summarise(Value = sum(Value, na.rm = T))
# Find annual rainfall overall
India_Rain_Overall <- India_Rain_melt %>% 
  group_by(YEAR, Parameter) %>%
  summarise(Value = sum(Value, na.rm = T))
# Find overall monthly rainfall by year
India_Rain_Overall_month <- India_Rain_melt %>% 
  group_by(YEAR, Month, Parameter) %>%
  summarise(Value = sum(Value, na.rm = T))

# Visualize annual rainfall trend
ggplot(data = India_Rain_Overall, aes(x = YEAR, y = Value, colour = Parameter)) +
  geom_line(size = 1.4,
            alpha = 0.5) +
  labs(x = "Year", 
       y = "Total Rainfall",
       title = "Annual ranfall in India from 1901-2015",
       caption = "Data Source: https://data.gov.in", 
       colour = "Category") + 
  theme(plot.title = element_text(size = 15, 
                                  hjust = 0.5), 
        legend.title = element_text(size = 10),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.text = element_text(size = 6, angle = 45),
        plot.caption = element_text(size = 7, 
                                    face = "italic"),
        panel.background = element_rect(colour = "white",
                                        size = 0.9,
                                        fill = "lightgray"),
        panel.grid.major = element_line(colour = "lavender", 
                                        size = 0.1),
        panel.grid = element_blank())
