# disney box office portfolio plots

# load packages
library(tidyverse)
library(lubridate)
library(readr)
library(scales)
library(ggimage)

# load data
disney_data <- read_csv("data/disney_boxoffice_history.csv")

# rename columns
disney_data <- disney_data |>
  rename(
    Movie_Title = "Movie Title",
    Date_Released = "Date Released",
    MPAA_Rating = "MPAA Rating",
    Total_Gross = "Total Gross",
    Inflation_Adjusted_Gross = "Inflation Adjusted Gross"
  )

# create year
disney_data <- disney_data |>
  mutate(
    Year = dmy(Date_Released) |> year()
  )

# define movie lists
pixar_movies <- c(
  "Cars", "Ratatouille", "WALL-E", "Up", "Toy Story 3", "Cars 2", "Brave",
  "Monsters University", "Inside Out", "The Good Dinosaur", "Finding Dory",
  "Cars 3", "Coco", "Incredibles 2", "Toy Story 4", "Onward", "Soul",
  "Luca", "Turning Red", "Lightyear", "Elemental", "Inside Out 2"
)

lucasfilm_movies <- c(
  "Star Wars Ep. VII: The Force Awakens",
  "Rogue One: A Star Wars Story"
)

marvel_movies <- c(
  "The Avengers", "Iron Man 3", "Thor: The Dark World",
  "Captain America: The Winter Soldier", "Guardians of the Galaxy",
  "Avengers: Age of Ultron", "Ant-Man", "Captain America: Civil War",
  "Doctor Strange", "Guardians of the Galaxy Vol. 2",
  "Spider-Man: Homecoming", "Thor: Ragnarok", "Black Panther",
  "Avengers: Infinity War", "Ant-Man and the Wasp", "Captain Marvel",
  "Avengers: Endgame", "Spider-Man: Far From Home"
)

# rebuild studio group using title patterns
# rebuild studio group using title patterns
disney_data <- disney_data |>
  mutate(
    Studio_Group = case_when(
      str_detect(
        Movie_Title,
        "Toy Story$|Toy Story 2$|Toy Story 3$|Cars$|Cars 2$|Ratatouille|WALL-E|Monsters, Inc\\.|Monsters University|Finding Nemo|Finding Dory|The Incredibles|A Bug's Life|Inside Out|The Good Dinosaur"
      ) ~ "Pixar",
      str_detect(Movie_Title, "Star Wars|Rogue One") ~ "Lucasfilm",
      str_detect(Movie_Title, "Avengers|Iron Man|Thor|Captain America|Guardians of the Galaxy|Doctor Strange|Ant-Man") ~ "Marvel",
      TRUE ~ "Disney / Other"
    )
  )

disney_data |>
  count(Studio_Group)

disney_data |>
  filter(Studio_Group != "Disney / Other") |>
  select(Movie_Title, Studio_Group) |>
  arrange(Studio_Group, Movie_Title)

disney_data |>
  arrange(desc(Inflation_Adjusted_Gross)) |>
  select(Movie_Title, Year, Genre, Inflation_Adjusted_Gross, Studio_Group) |>
  slice_head(n = 20)

disney_data |>
  group_by(Studio_Group) |>
  summarize(
    total_revenue_b = sum(Inflation_Adjusted_Gross, na.rm = TRUE) / 1e9,
    movie_count = n(),
    avg_revenue_m = mean(Inflation_Adjusted_Gross, na.rm = TRUE) / 1e6
  ) |>
  arrange(desc(total_revenue_b))

disney_data |>
  filter(Year >= 2005) |>
  group_by(Year) |>
  summarize(
    total_revenue_b = sum(Inflation_Adjusted_Gross, na.rm = TRUE) / 1e9,
    movie_count = n(),
    avg_revenue_m = mean(Inflation_Adjusted_Gross, na.rm = TRUE) / 1e6
  )

disney_data |>
  filter(Studio_Group == "Pixar") |>
  select(Movie_Title, Year) |>
  arrange(Year)

disney_data |>
  filter(Studio_Group == "Marvel") |>
  select(Movie_Title, Year) |>
  arrange(Year)

disney_data |>
  filter(Studio_Group == "Lucasfilm") |>
  select(Movie_Title, Year) |>
  arrange(Year)

# create folder path assumption:
# "portfolio plots"

#############################################################################
# portfolio plot 1: average revenue per film by studio (modern era)         #
#############################################################################
studio_avg <- disney_data |>
  mutate(
    Studio_Group = if_else(Studio_Group == "Disney / Other", "Disney", Studio_Group)
  ) |>
  filter(Year >= 2006) |>
  group_by(Studio_Group) |>
  summarize(
    avg_revenue_m = mean(Inflation_Adjusted_Gross, na.rm = TRUE) / 1e6,
    movie_count = n(),
    .groups = "drop"
  ) |>
  arrange(desc(avg_revenue_m))

avg_studio_plot <- ggplot(
  studio_avg,
  aes(
    x = reorder(Studio_Group, avg_revenue_m),
    y = avg_revenue_m,
    fill = Studio_Group
  )
) +
  geom_col(width = 0.68, show.legend = FALSE) +
  geom_text(
    aes(
      label = paste0("$", round(avg_revenue_m), "M"),
      color = after_scale(fill)
    ),
    hjust = -0.2,
    size = 6,
    fontface = "bold"
  ) +
  coord_flip() +
  scale_fill_manual(values = c(
    "Disney"    = "#104E8B",
    "Pixar"     = "#62B7FD",
    "Marvel"    = "#C0132C",
    "Lucasfilm" = "#000000"
  )) +
  scale_y_continuous(
    labels = label_dollar(suffix = "M"),
    expand = expansion(mult = c(0, 0.18))
  ) +
  labs(
    title = "Average Box Office Revenue per Film by Studio",
    x = NULL,
    y = "Average Inflation-Adjusted Gross (Millions USD)",
    caption = "Source dataset covers Disney films through 2016"
  ) +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "#F2F3F4", color = NA),
    panel.background = element_rect(fill = "#F2F3F4", color = NA),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = alpha("#0F2656", 0.3)),
    plot.title = element_text(size = 29, face = "bold", hjust = 0, color = "#0F2656"),
    plot.subtitle = element_text(size = 12.5, face = "bold", hjust = 0.5, color = "#0F2656"),
    axis.title.x = element_text(size = 24, face = "bold", color = "#0F2656", margin = margin(t = 15)),
    axis.text = element_text(size = 18, face = "bold", color = "#0F2656"),
    plot.caption = element_text(size = 10, color = "#0F2656"),
    plot.margin = margin(20, 20, 20, 20)
  )

ggsave(
  "portfolio-plots/avg_revenue_by_studio_modern.png",
  plot = avg_studio_plot,
  width = 12,
  height = 7,
  dpi = 300,
  bg = "#F2F3F4"
)

#############################################################################
# portfolio plot 2: disney avg revenue per film over time                   #
#############################################################################
disney_trend <- disney_data |>
  mutate(
    Studio_Group = if_else(Studio_Group == "Disney / Other", "Disney", Studio_Group)
  ) |>
  filter(Year >= 1995) |>
  group_by(Year) |>
  summarize(
    avg_revenue_m = mean(Inflation_Adjusted_Gross, na.rm = TRUE) / 1e6,
    movie_count = n(),
    .groups = "drop"
  )

acquisitions <- tibble(
  Year = c(2006, 2009, 2012),
  label = c("Pixar", "Marvel", "Lucasfilm"),
  logo = c(
    "logos/pixar.png",
    "logos/marvel.png",
    "logos/lucasfilm.png"
  ),
  y_pos = c(170, 220, 270),
  x_pos = c(2006, 2009, 2012)
)

disney_trend_plot <- ggplot(
  disney_trend,
  aes(x = Year, y = avg_revenue_m)
) +
  geom_area(fill = "#87CEFF", alpha = 0.25) +
  geom_vline(
    data = acquisitions,
    aes(xintercept = Year),
    linetype = "dashed",
    color = "#0F2656",
    alpha = 0.4,
    linewidth = 1
  ) +
  geom_image(
    data = acquisitions,
    aes(x = x_pos, y = y_pos, image = logo),
    inherit.aes = FALSE,
    size = 0.16,
    asp = 12/7
  ) +
  geom_line(color = "#4F94CD", linewidth = 1.9) +
  geom_point(color = "#0F2656", size = 6) +
  scale_x_continuous(breaks = seq(1995, 2016, by = 3)) +
  scale_y_continuous(
    labels = label_dollar(suffix = "M"),
    expand = expansion(mult = c(0.02, 0.15)),
    limits = c(0, 280)
  ) +
  labs(
    title = "Disney's Average Film Revenue: 1995–2016",
    x = NULL,
    y = "Avg. Inflation-Adjusted Gross (Millions USD)"
  ) +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "#F2F3F4", color = NA),
    panel.background = element_rect(fill = "#F2F3F4", color = NA),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = alpha("#0F2656", 0.25)),
    plot.title = element_text(size = 30, face = "bold", hjust = 0, color = "#0F2656"),
    plot.subtitle = element_text(size = 18, face = "bold", hjust = 0, color = "#0F2656"),
    axis.title.y = element_text(size = 20, face = "bold", color = "#0F2656", margin = margin(r = 15)),
    axis.title.x = element_text(size = 24, face = "bold", color = "#0F2656"),
    axis.text = element_text(size = 20, face = "bold", color = "#0F2656"),
    plot.margin = margin(20, 20, 20, 20)
  )

ggsave(
  "portfolio-plots/disney_avg_revenue_trend.png",
  plot = disney_trend_plot,
  width = 12,
  height = 8.5,
  dpi = 300,
  bg = "#F2F3F4"
)

#############################################################################
# portfolio table 3: top 10 films post-2006 colored by studio                #
#############################################################################
library(gt)

top10_table <- disney_data |>
  filter(Year >= 2006) |>
  mutate(
    Studio_Group = if_else(Studio_Group == "Disney / Other", "Disney", Studio_Group)
  ) |>
  arrange(desc(Inflation_Adjusted_Gross)) |>
  slice_head(n = 10) |>
  mutate(
    Rank = row_number(),
    Revenue = paste0("$", round(Inflation_Adjusted_Gross / 1e6), "M")
  ) |>
  select(Rank, Movie_Title, Year, Studio_Group, Revenue)

top10_gt <- top10_table |>
  gt() |>
  cols_label(
    Rank = "#",
    Movie_Title = "Film",
    Year = "Year",
    Studio_Group = "Studio",
    Revenue = "Revenue (Inflation-Adjusted)"
  ) |>
  cols_align(align = "left") |>
  cols_width(
    Rank ~ px(80),
    Movie_Title ~ px(340),
    Year ~ px(80),
    Studio_Group ~ px(120),
    Revenue ~ px(240)
  ) |>
  tab_header(
    title = md("**Top 10 Highest-Grossing Disney Films Since 2006**"),
  ) |>
  # general body style FIRST
  tab_style(
    style = cell_text(weight = "bold", color = "#0F2656"),
    locations = cells_body()
  ) |>
  # studio colors AFTER to override
  tab_style(
    style = list(
      cell_fill(color = "#000000"),
      cell_text(color = "white", weight = "bold")
    ),
    locations = cells_body(columns = Studio_Group, rows = Studio_Group == "Lucasfilm")
  ) |>
  tab_style(
    style = list(
      cell_fill(color = "#C0132C"),
      cell_text(color = "white", weight = "bold")
    ),
    locations = cells_body(columns = Studio_Group, rows = Studio_Group == "Marvel")
  ) |>
  tab_style(
    style = list(
      cell_fill(color = "#62B7FD"),
      cell_text(color = "white", weight = "bold")
    ),
    locations = cells_body(columns = Studio_Group, rows = Studio_Group == "Pixar")
  ) |>
  tab_style(
    style = list(
      cell_fill(color = "#104E8B"),
      cell_text(color = "white", weight = "bold")
    ),
    locations = cells_body(columns = Studio_Group, rows = Studio_Group == "Disney")
  ) |>
  # title styling
  tab_style(
    style = cell_text(color = "#0F2656", size = px(24),align = "center"),
    locations = cells_title(groups = "title")
  ) |>
  tab_style(
    style = cell_text(color = "#0F2656", size = px(14), align = "left"),
    locations = cells_title(groups = "subtitle")
  ) |>
  # column headers
  tab_style(
    style = cell_text(weight = "bold", color = "#0F2656", size = px(14)),
    locations = cells_column_labels()
  ) |>
  # indent everything
  tab_style(
    style = cell_text(indent = px(20)),
    locations = list(
      cells_title(),
      cells_column_labels(),
      cells_body(),
      cells_source_notes()
    )
  ) |>
  # cell borders
  tab_style(
    style = cell_borders(
      sides = "all",
      color = "#0F2656",
      weight = px(1)
    ),
    locations = cells_body()
  ) |>
  opt_table_outline(style = "none") |>
  tab_options(
    table.background.color = "#F2F3F4",
    table.border.top.color = "#0F2656",
    table.border.bottom.color = "#0F2656",
    column_labels.border.bottom.color = "#0F2656",
    data_row.padding = px(8),
    heading.padding = px(12),
    source_notes.font.size = px(11)
  ) |>
  tab_source_note("Source dataset covers Disney films through 2016")

gtsave(
  top10_gt,
  "portfolio-plots/top10_table.png",
  vwidth = 1050,
  vheight = 650
)

#############################################################################
# portfolio plot 4: film share vs revenue share by studio                   #
#############################################################################
share_long <- share_data |>
  select(Studio_Group, film_share, revenue_share) |>
  pivot_longer(
    cols = c(film_share, revenue_share),
    names_to = "Metric",
    values_to = "Share"
  ) |>
  mutate(
    Metric = if_else(Metric == "film_share", "Share of Films", "Share of Revenue")
  )

grouped_plot <- ggplot(
  share_long,
  aes(
    x = reorder(Studio_Group, -Share),
    y = Share,
    fill = Metric
  )
) +
  geom_col(position = position_dodge(width = 0.75), width = 0.75) +
  geom_text(
    aes(label = paste0(round(Share), "%")),
    position = position_dodge(width = 0.75),
    vjust = -0.5,
    size = 5,
    fontface = "bold",
    color = "#0F2656"
  ) +
  scale_fill_manual(values = c(
    "Share of Films"   = "#62B7FD",
    "Share of Revenue" = "#0F2656"
  )) +
  scale_y_continuous(
    labels = function(x) paste0(x, "%"),
    expand = expansion(mult = c(0, 0.12))
  ) +
  labs(
    title = "Box Office Revenue by Disney Studio",
    x = "Studio",
    y = "Share (%)",
    fill = NULL
  ) +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "#F2F3F4", color = NA),
    panel.background = element_rect(fill = "#F2F3F4", color = NA),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = alpha("#0F2656", 0.15)),
    plot.title = element_text(size = 29, face = "bold", hjust = 0, color = "#0F2656"),
    plot.subtitle = element_text(size = 18, face = "bold", hjust = 0, color = "#0F2656"),
    axis.title.x = element_text(size = 24, face = "bold", color = "#0F2656", margin = margin(r = 15)),
    axis.title.y = element_text(size = 24, face = "bold", color = "#0F2656", margin = margin(r = 15)),
    axis.text.x = element_text(size = 20, face = "bold", color = "#0F2656", margin = margin(t = 12)),
    axis.text.y = element_text(size = 20, face = "bold", color = "#0F2656"),
    legend.text = element_text(size = 20, face = "bold", color = "#0F2656"),
    legend.position = "top",
    legend.justification = "left",
    plot.margin = margin(20, 20, 20, 60)
  )

ggsave(
  "portfolio-plots/film_vs_revenue_share.png",
  plot = grouped_plot,
  width = 13,
  height = 8,
  dpi = 300,
  bg = "#F2F3F4"
)