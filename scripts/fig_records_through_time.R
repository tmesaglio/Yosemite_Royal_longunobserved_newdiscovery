library(tidyverse)

#read in vouchers
roy_vouch <- read_csv("data/royal_all_vouchers_curated_2024_09_23.csv")
yos_vouch <- read_csv("data/Yosemite_CCH_records_2024_09_22.csv")
yos_vouch <- filter(yos_vouch, year > 1850, keep_omit == "keep") #some erroneous data at 1800

#read in inat
roy_inat <- read_csv("data/inat_rg_27may2024.csv")
yos_inat <- read_csv("data/Yosemite_iNat_RG_2024_03.csv")
roy_inat$year <- year(dmy(roy_inat$observed_on))
yos_inat$year <- year(ymd(yos_inat$observed_on))

#accounting
roy_vouch$park <- "Royal"
roy_inat$park <- "Royal"
yos_vouch$park <- "Yosemite"
yos_inat$park <- "Yosemite"

roy_vouch$`Record type` <- "Voucher"
yos_vouch$`Record type` <- "Voucher"
roy_inat$`Record type` <- "Photograph"
yos_inat$`Record type` <- "Photograph"

#combining all data
both <- rbind(
  select(roy_vouch, year, park, `Record type`),
  select(yos_vouch, year, park, `Record type`),
  select(roy_inat, year, park, `Record type`),
  select(yos_inat, year, park, `Record type`)
)

# important event data frame
events <- data.frame(
  year = c(1890, 1864, 1879, 1842, 1860),
  event = c(
    "Park Formation",
    "Yosemite Grant",
    "Park Formation",
    "First Extant Collection",
    "First Extant Collection"
  ),
  park = c("Yosemite", "Yosemite", "Royal", "Royal", "Yosemite"),
  y = c(3400, 3400, 1950, 2400, 4300)  # Adjust the vertical position as needed
)
# Reorder 'park' factor to make sure 'Yosemite' comes before 'Royal'
both$park <- factor(both$park, levels = rev(c("Yosemite", "Royal")))
both$park <- fct_rev(both$park)
events$park <- factor(events$park, levels = levels(both$park))

# Plot
ggplot(data = both, aes(x = year)) +
  geom_bar(data = both, aes(fill = `Record type`)) +
  scale_fill_manual(values = c("#74AC00BF", "#608CB8BF")) +
  facet_grid(park ~ ., scales = "free") +
  theme_bw() +
  # Add arrows coming down from the top
  geom_segment(
    data = events,
    aes(
      x = year,
      xend = year,
      y = 700,
      yend = 0  # Arrow points down to the x-axis
    ),
    arrow = arrow(length = unit(0.2, "cm")),
    color = "blue"
  ) +
  # Add smaller labels for the events
  geom_text(
    data = events,
    aes(x = year, y = y, # Position slightly above the arrow start
        label = event),
    size = 3,
    # Smaller text size
    angle = 90,
    vjust = 0.5,
    hjust = 1,
    color = "blue"
  ) + ylab("Number of records per year") +
  geom_vline(xintercept = 1990, lty = "dashed") + xlab("")
ggsave("output/Fig1_records_per_year.pdf",
       width = 8,
       height = 5)
ggsave("output/Fig1_records_per_year.svg",
       width = 8,
       height = 5)
ggsave("output/Fig1_records_per_year.png",
       width = 8,
       height = 5)
