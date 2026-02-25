library(ggplot2)
library(dplyr)
library(tidyr)
library(tidyverse)

setwd("/Users/abdulazeez/Documents/Annual report/Preparation/2025")

####data for island-wise resource############
df <- tribble(
  ~Resource, ~Agatti, ~Amini, ~Androth, ~Bitra, ~Chetlat, ~Kadmat, ~Kalpeni, ~Kavaratti, ~Kiltan, ~Minicoy,
  "Tuna", 3698815, 180436, 3086046, 114088, 193161, 243402, 327965, 3213798, 215340, 2884464,
  "Billfishes", 14681, 74590, 93800, 7112, 4270, 4965, 4413, 55671, 5486, 10180,
  "Other pelagics", 496503, 167050, 391570, 35059, 28888, 61623, 133152, 392483, 114335, 373960,
  "Reef fishes", 389839, 60559, 111920, 78758, 11162, 46693, 26131, 286491, 12800, 68070,
  "Elasmobranches", 29225, 8485, 15280, 11156, 3185, 8040, 725, 60503, 7944, 7950,
  "Miscellaneous", 45631, 9549, 22600, 52335, 4480, 2592, 2765, 76193, 1719, 44582
)

df_long <- df %>%
  pivot_longer(-Resource, names_to = "Island", values_to = "Landing")

#Plot multi-pie (faceted pies)#
ggplot(df_long, aes(x = 1, y = Landing, fill = Resource)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar(theta = "y") +
  facet_wrap(~Island, ncol = 5, scales = "free") +
  theme_void() +
  theme(
    aspect.ratio = 1,
    strip.text = element_text(size = 12, face = "bold"),
    legend.position = "bottom"
  ) +
  labs(fill = "Resource",
       title = "Island-wise Landing Composition of Fishery Resources")


##Optional, show percentage labels inside pies#
df_long <- df_long %>%
  group_by(Island) %>%
  mutate(
    Percent = Landing / sum(Landing) * 100,
    Label = ifelse(Percent > 4, paste0(round(Percent, 1), "%"), "")
  )

ggplot(df_long, aes(x = 1, y = Landing, fill = Resource)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar(theta = "y") +
  facet_wrap(~Island, ncol = 5, scales = "free") +
  geom_text(aes(label = Label),
            position = position_stack(vjust = 0.5),
            size = 4) +
  theme_void() +
  theme(
    aspect.ratio = 1,
    strip.text = element_text(size = 12, face = "bold"),
    legend.position = "bottom"
  ) +
  labs(fill = "Resources",
       title = "")
ggsave("island_pie1.jpg", width = 10, height = 8, dpi = 600)


#######pie chart for total resource landing#######
df_total <- tribble(
  ~Species, ~Total,
  "Tuna", 14157515,
  "Billfishes", 275168,
  "Other pelagics", 2194623,
  "Reef fishes", 1092423,
  "Elasmobranches", 152493,
  "Miscellaneous", 262446
)

df_total <- df_total %>%
  mutate(
    Percent = Total / sum(Total) * 100,
    Label = ifelse(Percent > 2,
                   paste0(round(Percent,1), "%"),
                   "")
  )

ggplot(df_total, aes(x = 1, y = Total, fill = Species)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar(theta = "y") +
  geom_text(aes(label = Label),
            position = position_stack(vjust = 0.5),
            size = 4) +
  theme_void() +
  theme(
    aspect.ratio = 1,
    legend.position = "right"
  ) +
  labs(
    fill = "Resources",
    title = ""
  )
ggsave("Total_pie.jpg", width = 7, height = 6, dpi = 600)


#####Year wise barplot-stack#######
###create data
df_year <- tribble(
  ~Year, ~Yellowfin, ~Skipjack, ~`Other tunas`, ~Billfishes, ~`Other pelagics`, ~`Reef fishes`, ~Elasmobranchs, ~Miscellaneous,
  2014, 2431143, 8440153, 556552, 40885, 809370, 438121, 43525, 2444262,
  2015, 2275003, 9644322, 596340, 48848, 849971, 390612, 51212, 2352966,
  2016, 2347559, 20891456, 720189, 52600, 748345, 263948, 59484, 566063,
  2017, 2812112, 10017573, 1324035, 68715, 856843, 347687, 57758, 334585,
  2018, 10193067, 13167745, 1562385, 115466, 1584628, 892082, 71691, 345729,
  2019, 7265499, 8199864, 3978839, 238630, 1862333, 960336, 41539, 381789,
  2020, 3365924, 7502688, 2584834, 158088, 1916116, 730553, 34403, 107071,
  2021, 2677279, 6436862, 951601, 66655, 672676, 319056, 29442, 594077,
  2022, 2758201, 5233082, 1354476, 45418, 951477, 438447, 33086, 103720,
  2023, 3383757, 6366343, 1869768, 119472, 1518264, 855848, 100868, 215924,
  2024, 5405960, 6523936, 1356191, 266607, 2064052, 947482, 120186, 206921,
  2025, 5912797, 6882358, 1362360, 275168, 2194623, 1092423, 152493, 262446
)


#convert to long format

df_long <- df_year %>%
  pivot_longer(-Year, names_to = "Resource", values_to = "Landing_kg") %>%
  mutate(Landing_t = Landing_kg / 1000)


###plot preparation
ggplot(df_long, aes(x = factor(Year), y = Landing_t, fill = Resource)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(
    breaks = seq(0, max(df_long$Landing_t) * 1.05, by = 5000),
    expand = c(0, 0)
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right"
  ) +
  labs(
    x = "",
    y = "Landing (tonnes)",
    fill = "Resources",
    title = ""
  )

ggsave("Total_landing trends.jpg", width = 11, height = 6, dpi = 600)

######Radar plot for reef fishes########
#install.packages("ggradar")   # if not installed
#install.packages("scales")
library(tidyverse)
library(ggradar)
library(scales)
#install.packages("devtools")   # run once
library(devtools)
devtools::install_github("ricardo-bion/ggradar")
install.packages("scales")   # run once if not installed
library(scales)

df_reef <- tribble(
  ~Resources, ~Agatti, ~Amini, ~Androth, ~Bitra, ~Chetlat, ~Kadmat, ~Kalpeni, ~Kavaratti, ~Kiltan, ~Minicoy,
  "Lutjanus_gibbus", 80994, 13570, 25480, 16650, 2700, 6876, 7485, 74311, 3827, 15900,
  "Lutjanus_bohar", 22384, 11190, 12490, 9988, 530, 5960, 4735, 48825, 1443, 11470,
  "Serranidae", 104482, 7705, 21380, 30047, 1290, 3775, 5580, 45874, 2442, 9520,
  "Lethrinidae", 81215, 13440, 10710, 20013, 2660, 20343, 6990, 49821, 4292, 10200,
  "Gerridae", 21890, 7473, 9910, 1240, 630, 3765, 596, 28430, 245, 11060,
  "Mullidae", 78874, 7181, 31950, 820, 3352, 5974, 745, 39230, 551, 9920
)


df_long <- df_reef %>%
  pivot_longer(-Resources,
               names_to = "Island",
               values_to = "Landing_kg") %>%
  mutate(Landing_t = Landing_kg / 1000)

ggplot(df_long,
       aes(x = Island,
           y = Landing_t,
           fill = Resources)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar() +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 11),
    panel.grid = element_blank(),
    legend.position = "right"
  ) +
  labs(
    x = "",
    y = "Landing (tonnes)",
    fill = "Resources",
    title = ""
  )
ggsave("Reef fish_radar.jpg", width = 8, height = 6, dpi = 600)

#######monthly catch radar plot######
df_month <- data.frame(
  Resource = c("Yellowfin","Skipjack","Other tunas","Billfishes",
               "Other pelagics","Reef fishes","Elasmobranchs","Miscellaneous"),
  
  Jan = c(981.549,1259.589,224.405,12.116,137.807,60.722,9.120,8.419),
  Feb = c(641.144,1130.391,227.624,12.251,146.631,65.678,8.035,10.906),
  Mar = c(696.657,1182.681,231.471,47.870,146.964,78.701,8.696,11.451),
  Apr = c(447.272,340.389,76.134,21.283,158.554,81.072,14.261,17.519),
  May = c(371.899,277.553,81.545,15.093,138.285,89.429,14.527,23.652),
  Jun = c(414.402,366.657,70.441,19.122,173.960,86.656,11.427,16.845),
  Jul = c(400.998,328.804,65.293,16.660,171.353,92.503,10.106,16.228),
  Aug = c(408.555,359.052,76.823,23.726,202.576,104.335,11.674,27.337),
  Sep = c(420.892,390.162,86.367,25.593,242.863,114.471,17.639,27.422),
  Oct = c(344.549,341.920,72.141,26.007,225.443,105.215,17.420,27.102),
  Nov = c(417.402,397.592,76.651,25.154,222.563,99.048,14.364,32.752),
  Dec = c(367.478,507.568,73.465,30.293,227.624,114.593,15.224,42.813)
)

df_month


library(tidyverse)

# Your dataframe assumed as df_month

df_long <- df_month %>%
  pivot_longer(-Resource,
               names_to = "Month",
               values_to = "Landing_kg") %>%
  mutate(Landing_t = Landing_kg / 1000)

# Ensure month order is correct
df_long$Month <- factor(df_long$Month,
                        levels = c("Jan","Feb","Mar","Apr","May","Jun",
                                   "Jul","Aug","Sep","Oct","Nov","Dec"))

ggplot(df_long,
       aes(x = Month,
           y = Landing_t,
           fill = Resource)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar() +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 12),
    panel.grid = element_blank(),
    legend.position = "right"
  ) +
  labs(
    title = "",
    fill = "Resource"
  )
ggsave("monthly_radar.jpg", width = 8, height = 6, dpi = 600)


###############Tuna live bait#################
library(ggplot2)
library(dplyr)
library(patchwork)

# -----------------------------
# Pole and Line Fishery
# -----------------------------
pole_line <- data.frame(
  Species = c("Silver sprat","Blue sprat","Caesio spp","Pteroceasio spp",
              "Green chromis","Lepidozygous tapeinosoma","Fusiliers (others)",
              "Cardinal fishes","Trigger fishes","Anthias","Atherina",
              "Wrasse","Half beak","Others"),
  Percent = c(42,17,12.3,8.2,3.5,4.4,3,3.85,1.3,0.5,2,0.45,0.7,0.8)
)

# -----------------------------
# Handline Fishery
# -----------------------------
handline <- data.frame(
  Species = c("Caesio spp","Pterocaesio spp","Chromis caeruleus",
              "Lepidozygous tapeinosoma","Other fusiliers",
              "Trigger fishes","Cardinal fishes","Anthias",
              "Wrasses","Halfbeak","Others"),
  Percent = c(16,12,6.1,5,6,46.9,0.9,1.1,3,1.2,1.8)
)

# -----------------------------
# Pie chart function
# -----------------------------
pie_plot <- function(data, title_text){
  ggplot(data, aes(x = "", y = Percent, fill = Species)) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar(theta = "y") +
    theme_void() +
    labs(title = title_text, fill = "Species / Group") +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      legend.position = "right"
    )
}

# -----------------------------
# Create plots
# -----------------------------
p1 <- pie_plot(pole_line, "Pole and Line Tuna Fishery")
p2 <- pie_plot(handline, "Handline Tuna Fishery")

# -----------------------------
# Display side by side
# -----------------------------
p1 + p2


library(ggplot2)
library(grid)

# -----------------------------
# Pole and Line Fishery
# -----------------------------
pole_line <- data.frame(
  Species = c("Blue sprat","Silver sprat","Caesio spp","Pteroceasio spp",
              "Green chromis","Lepidozygous tapeinosoma","Other fusiliers",
              "Cardinal fishes","Trigger fishes","Anthias","Atherina",
              "Wrasse","Half beak","Others"),
  Percent = c(42,17,12.3,8.2,3.5,4.4,3,3.85,1.3,0.5,2,0.45,0.7,0.8)
)
pole_line$Species <- factor(pole_line$Species, levels = pole_line$Species)

# -----------------------------
# Handline Fishery
# -----------------------------
handline <- data.frame(
  Species = c("Caesio spp","Pterocaesio spp","Chromis caeruleus",
              "Lepidozygous tapeinosoma","Other fusiliers",
              "Trigger fishes","Cardinal fishes","Anthias",
              "Wrasses","Halfbeak","Others"),
  Percent = c(16,12,6.1,5,6,46.9,0.9,1.1,3,1.2,1.8)
)

handline$Species <- factor(handline$Species, levels = handline$Species)


# -----------------------------
# Pie function
# -----------------------------
pie_plot <- function(data, title_text){
  
  ggplot(data, aes(x = "", y = Percent, fill = Species)) +
    geom_bar(stat = "identity", width = 1, color = "white") +
    coord_polar(theta = "y") +
    geom_text(
      aes(label = ifelse(Percent >= 5, paste0(Percent, "%"), "")),
      position = position_stack(vjust = 0.5),
      size = 4
    ) +
    theme_void() +
    labs(title = title_text, fill = "Species / Group") +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      legend.position = "right"
    )
}

p1 <- pie_plot(pole_line, "Pole and Line Tuna Fishery")
p2 <- pie_plot(handline, "Handline Tuna Fishery")

p1 + p2

##ggsave("Tuna livebait.jpg", width = 11, height = 6, dpi = 600)


##########Save data###########
# Install if not already installed
install.packages("openxlsx")

library(openxlsx)

# Create a new workbook
wb <- createWorkbook() 

# Add worksheets
addWorksheet(wb, "Pole_and_Line")
addWorksheet(wb, "Handline")

# Write data to sheets
writeData(wb, "Pole_and_Line", pole_line)
writeData(wb, "Handline", handline)

# Save file
saveWorkbook(wb, "Tuna_Livebait_Catch_Composition.xlsx", overwrite = TRUE)
