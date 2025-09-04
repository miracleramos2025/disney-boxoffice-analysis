# STAT 302 Final Project
# sandbox for visualizations

# load packages
library(tidyverse)
library(ggplot2)
library(dplyr)
library(lubridate)
library(readr)
library(ggimage)  
library(knitr)
library(scales)

# load data
disney_data <- read_csv("data/disney_boxoffice_history.csv")

#############################################################################
# Visualization 1: Time-Series Line Chart                                   #
#############################################################################
# rename columns to remove spaces
disney_data <- disney_data |>
  rename(Movie_Title = "Movie Title",
         Date_Released = "Date Released",
         MPAA_Rating = "MPAA Rating",
         Total_Gross = "Total Gross",
         Inflation_Adjusted_Gross = "Inflation Adjusted Gross")

# convert Date Released to Year
disney_data <- disney_data |>
  mutate(Year = as.numeric(format(dmy(Date_Released), "%Y")))

# filter for 2005 and later
disney_recent <- disney_data |>
  filter(Year >= 2005)

# gggregate revenue per year
annual_revenue <- disney_recent |>
  group_by(Year) |>
  summarize(total_revenue = sum(Inflation_Adjusted_Gross, na.rm = TRUE))

# match acquisition years with logos
acquisitions <- data.frame(
  Year = c(2006, 2009, 2012),
  label = c("Pixar", "Marvel", "Lucasfilm"),
  logo = c("logos/pixar.png", "logos/marvel.png", "logos/lucasfilm.png")
) |>
  left_join(annual_revenue, by = "Year") |>
  mutate(total_revenue = ifelse(is.na(total_revenue), max(annual_revenue$total_revenue) * 0.9, total_revenue))

# make acquisition plot
acquisition_plot <- ggplot(annual_revenue, 
                           aes(x = Year, 
                               y = total_revenue)) +
  geom_line(color = "lightblue", linewidth = 1.8) +  
  geom_vline(data = acquisitions, 
             aes(xintercept = Year), 
             linetype = "dashed", 
             linewidth = 1, 
             color = "red") +  
  geom_point(data = acquisitions, 
             aes(x = Year, 
                 y = total_revenue), 
             size = 6, 
             color = "red") +
  geom_image(data = acquisitions, 
             aes(x = Year, 
                 y = total_revenue * 0.88, 
                 image = logo), 
             size = 0.25) +  
  labs(title = "The Impact of Disney's Major Acquisitions on Box Office Revenue",
       subtitle = "How Pixar, Marvel, and Lucasfilm Transformed Disney's Financial Success",
       x = "Year of Acquisition", 
       y = "Total Revenue (Billions)") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5, color = "white"),
    plot.subtitle = element_text(face = "bold", size = 15, hjust = 0.5, color = "white"),
    axis.title = element_text(face = "bold", size = 18, color = "white"),
    axis.text.x = element_text(face = "bold", angle = 35, hjust = 1, color = "white", size = 15),
    axis.text.y = element_text(face = "bold", angle = 35, hjust = 1, color = "white", size = 18),
    legend.position = "none"
  ) +
  scale_x_continuous(breaks = seq(2005, 2025, 2))   

# save acquisition plot
ggsave("figures/acquisitions_plot.png", plot = acquisition_plot, width = 10, height = 6, dpi = 300)

### acquisition year $ Table
# Define acquisition years
acquisition_years <- c(2006, 2009, 2012)

# revenue for year after each acquisition
acquisition_years <- c(2006, 2009, 2012)

# earnings for acquisition year and the year after
earnings_data <- disney_data %>%
  filter(Year %in% c(acquisition_years, acquisition_years + 1)) %>%
  group_by(Year) %>%
  summarize(Earnings_Billion = sum(Inflation_Adjusted_Gross, na.rm = TRUE) / 1e9)  # Convert to billions

# table with acquisition year vs next year
earnings_table <- data.frame(
  Acquisition = c("Pixar", "Marvel", "Lucasfilm"),
  Acquisition_Year = acquisition_years,
  Earnings_Acquisition_Year = earnings_data$Earnings_Billion[match(acquisition_years, earnings_data$Year)],
  Earnings_Next_Year = earnings_data$Earnings_Billion[match(acquisition_years + 1, earnings_data$Year)]
)

# percentage growth
earnings_table <- earnings_table |>
  mutate(Percent_Growth = ((Earnings_Next_Year - Earnings_Acquisition_Year) / Earnings_Acquisition_Year) * 100) 

earnings_table <- earnings_table |>
  rename(
    Acquisition = Acquisition,
    `Year Acquired` = Acquisition_Year,
    `First Year Earnings (Billions, USD)` = Earnings_Acquisition_Year,
    `Next Year Earnings (Billions, USD)` = Earnings_Next_Year,
    `% Change` = Percent_Growth
  )


earnings_table

# save / write out 
save(earnings_table, file = "data/earnings_table.RData")

#############################################################################
# Visualization 2: Boxplot of Revenue by Genre                              #
#############################################################################

#  total revenue per genre
genre_revenue <- disney_data |>
  group_by(Genre) |>
  summarize(Total_Revenue = sum(Total_Gross, na.rm = TRUE)) |>
  arrange(desc(Total_Revenue))  

# bar plot 
genre_barplot <- ggplot(genre_revenue, aes(x = reorder(Genre, -Total_Revenue), y = Total_Revenue)) +
  geom_col(fill = "lightblue", width = 0.7) +  
  labs(
    title = "Disney's Top-Grossing Genres",
    subtitle = "Total Box Office Revenue by Genre (Nominal)",
    x = "Genre",
    y = "Total Revenue (Billions USD)"
  ) +
  scale_y_continuous(labels = scales::dollar_format(scale = 1e-9, suffix = "B")) +  
  theme_minimal() +
  theme(
    plot.title = element_text(size = 28, face = "bold", hjust = 0.5, color = "white"),
    plot.subtitle = element_text(face = "bold", size = 22, hjust = 0.5, color = "white"),
    axis.title = element_text(face = "bold", size = 20, color = "white"),
    axis.text.x = element_text(face = "bold", angle = 35, hjust = 1, color = "white", size = 18),
    axis.text.y = element_text(face = "bold", angle = 35, hjust = 1, color = "white", size = 18),
  )

# save / write out
ggsave("figures/revenue_by_genre_plot.png", plot = genre_barplot, width = 14, height = 8, dpi = 300)


### highest grosing movie by genre table
top_movies <- disney_data |>
  group_by(Genre) |>
  slice_max(order_by = Total_Gross, n = 1) |>
  select(Genre, Movie_Title, Total_Gross) |>
  arrange(desc(Total_Gross)) |>
  mutate(Total_Gross = label_comma()(Total_Gross))

colnames(top_movies) <- c("Genre", "Highest-Grossing Movie", "Total Revenue (USD)")

# save / write out 
save(top_movies, file = "data/top_movies_by_genre.RData")

#############################################################################
# Visualization 3: Revenue Share by Acquisition (Stacked Bar Chart)         #
#############################################################################

# define acquisition costs (in billions)
acquisition_costs <- tibble(
  Studio = c("Pixar", "Marvel", "Lucasfilm"),
  Acquisition_Cost = c(7.4, 4.2, 4.1)  
)

# define acquisition years
acquisition_years <- tibble(
  Studio = c("Pixar", "Marvel", "Lucasfilm"),
  Acquisition_Year = c(2006, 2009, 2012)
)

# convert Date Released to Year
disney_data <- disney_data |>
  mutate(Year = as.numeric(format(lubridate::dmy(Date_Released), "%Y")))

# define movie lists explicitly for accuracy
pixar_movies <- c(
  "Cars", "Ratatouille", "WALL-E", "Up", "Toy Story 3", "Cars 2", "Brave", "Monsters University", 
  "Inside Out", "The Good Dinosaur", "Finding Dory", "Cars 3", "Coco", "Incredibles 2", "Toy Story 4", 
  "Onward", "Soul", "Luca", "Turning Red", "Lightyear", "Elemental", "Inside Out 2"
)

lucasfilm_movies <- c(
  "Star Wars Ep. VII: The Force Awakens", "Rogue One: A Star Wars Story"
)

marvel_movies <- c(
  "The Avengers", "Iron Man 3", "Thor: The Dark World", "Captain America: The Winter Soldier",
  "Guardians of the Galaxy", "Avengers: Age of Ultron", "Ant-Man", "Captain America: Civil War",
  "Doctor Strange", "Guardians of the Galaxy Vol. 2", "Spider-Man: Homecoming", "Thor: Ragnarok",
  "Black Panther", "Avengers: Infinity War", "Ant-Man and the Wasp", "Captain Marvel",
  "Avengers: Endgame", "Spider-Man: Far From Home"
)

# assign studios explicitly
disney_data <- disney_data |>
  mutate(Studio = case_when(
    Movie_Title %in% pixar_movies ~ "Pixar",
    Movie_Title %in% lucasfilm_movies ~ "Lucasfilm",
    Movie_Title %in% marvel_movies ~ "Marvel",
    TRUE ~ NA_character_  
  ))

# merge with acquisition years
disney_data <- disney_data |>
  left_join(acquisition_years, by = "Studio")

# filter movies released AFTER their acquisition year
post_acquisition_movies <- disney_data |>
  filter(!is.na(Studio) & Year >= Acquisition_Year)

# count movies & sum total revenue per studio
studio_summary <- post_acquisition_movies |>
  group_by(Studio) |>
  summarize(Total_Gross_Billions = sum(Inflation_Adjusted_Gross, na.rm = TRUE) / 1e9,  
            Movie_Count = n()) |>
  left_join(acquisition_costs, by = "Studio") |>
  pivot_longer(cols = c(Acquisition_Cost, Total_Gross_Billions), 
               names_to = "Metric", values_to = "Value")

# bar Chart: Acquisition Cost vs. Total Gross Revenue
bar_chart <- ggplot(studio_summary, aes(x = Studio, y = Value, fill = Metric)) +
  geom_col(position = "dodge", width = 0.6) +
  geom_text(aes(label = ifelse(Metric == "Total_Gross_Billions", paste0(Movie_Count, " movies"), ""), y = Value + 1), 
            position = position_dodge(width = 0.6), size = 5, vjust = -0.5, color = "white") +  
  scale_fill_manual(values = c("Acquisition_Cost" = "lightblue", "Total_Gross_Billions" = "#1f78b4"),
                    labels = c("Acquisition Cost", "Total Box Office Revenue")) +
  scale_y_continuous(labels = scales::dollar_format(scale = 1, suffix = "B")) +
  labs(title = "Disney Acquisitions: Cost vs. Total Box Office Earnings",
       subtitle = "Includes All Movies Produced Post-Acquisition up to 2016",
       x = "Studio",
       y = "Amount (Billions USD)",
       fill = "Metric") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5, color = "white"),
    plot.subtitle = element_text(size = 14, hjust = 0.5, color = "white"),
    axis.title = element_text(size = 14, face = "bold", color = "white"),
    axis.text = element_text(size = 12, color = "white"),
    legend.title = element_text(size = 12, face = "bold", color = "white"),  
    legend.text = element_text(size = 12, color = "white"),  
    legend.position = "top"
  )

# display plot
print(bar_chart)

# save the plot
ggsave("figures/acquisition_vs_revenue.png", plot = bar_chart, width = 10, height = 6, dpi = 300)



#############################################################################
# Visualization 4: Scatter Plot of Revenue vs. Release Year                 #
#############################################################################

disney_data <- disney_data |>
  mutate(Total_Gross_M = Total_Gross / 1e6)  

scatter_plot <- ggplot(disney_data, aes(x = Year, y = Total_Gross_M)) +
  geom_point(color = "lightblue", alpha = 0.7) +  
  geom_smooth(method = "loess", color = "cornflowerblue", linewidth = 1.2, se = FALSE) +  
  scale_y_continuous(labels = scales::dollar_format(scale = 1, suffix = "M")) +  
  labs(
    title = "Disney Movie Revenue vs. Release Year",
    subtitle = "How Individual Film Revenues Have Changed Over Time",
    x = "Release Year",
    y = "Total Gross Revenue (Millions USD)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5, color = "white"),
    plot.subtitle = element_text(face = "bold", size = 15, hjust = 0.5, color = "white"),
    axis.title = element_text(face = "bold", size = 18, color = "white"),
    axis.text = element_text(face = "bold", color = "white", size = 14)
  )

# save / write out plot
ggsave("figures/revenue_vs_release_year.png", plot = scatter_plot, width = 10, height = 6, dpi = 300)



