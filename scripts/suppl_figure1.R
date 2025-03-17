library(tidyverse)
library(lubridate)
library(patchwork)

yos<-read_csv("data/Yosemite_alltime_list_2025_03_17.csv")  %>%
  filter(final_list==1)

yos$first_year_both <- pmin(yos$year_first_voucher, yos$year_first_iNat, na.rm = TRUE)


yos$`Species discovery` <- case_when(yos$year_first_voucher<yos$year_first_iNat ~ "Voucher first",
                             is.na(yos$year_first_iNat)~ "Voucher only",
                             yos$year_first_voucher==yos$year_first_iNat ~ "Both in the same year",
                             yos$year_first_voucher>yos$year_first_iNat ~ "Photograph first",
                             is.na(yos$year_first_voucher) ~ "Photograph only")

yos$last_year_both<-pmax(yos$year_last_voucher,yos$year_last_iNat,na.rm=T)
# Reorder species by first_record and convert first_record and last_record to numeric

yos %>%
  filter(!is.na(first_year_both)) %>%
  mutate(accepted_name = fct_reorder(accepted_name, first_year_both)) %>%
  mutate(yaxis_order=as.numeric(accepted_name))->yos_keep_filtered

########### now Royal 

roy<-read_csv("data/Royal_alltime_list_2025_03_17.csv")
roy$first_year_both <- pmin(roy$year_first_voucher,  roy$date_first_iNat_obs, na.rm = TRUE)
roy$last_year_both<-pmax(roy$year_last_voucher,roy$date_last_iNat_obs,na.rm=T)


roy$`Species discovery` <- case_when(roy$year_first_voucher<roy$date_first_iNat_obs ~ "Voucher first",
                                     is.na(roy$date_first_iNat_obs)~ "Voucher only",
                                     roy$year_first_voucher==roy$date_first_iNat_obs ~ "Both in the same year",
                                     roy$year_first_voucher>roy$date_first_iNat_obs ~ "Photograph first",
                                     is.na(roy$year_first_voucher) ~ "Photograph only")

roy_keep<-filter(roy,!is.na(first_year_both))
table(roy_keep$`Species discovery`)
table(yos_keep_filtered$`Species discovery`)


# Reorder species by first_record and convert first_record and last_record to numeric

roy_keep %>%
  mutate(accepted_name = fct_reorder(accepted_name, first_year_both)) %>%
  mutate(yaxis_order=as.numeric(accepted_name))->roy_keep_filtered

########### now combine
# For Yosemite, rename columns to match Royal
yos_selected <- yos_keep_filtered %>%
  select(yaxis_order, year_first_voucher, year_last_voucher,
         year_first_iNat, year_last_iNat,
         first_year_both, `Species discovery`) %>%
  rename(
    date_first_iNat_obs = year_first_iNat,
    date_last_iNat_obs = year_last_iNat
  ) %>%
  select(yaxis_order, year_first_voucher, year_last_voucher,
         date_first_iNat_obs, date_last_iNat_obs,
         first_year_both, `Species discovery`) %>%
  mutate(Site = "Yosemite")

# For Royal, select columns directly
roy_selected <- roy_keep_filtered %>%
  select(yaxis_order, year_first_voucher, year_last_voucher,
         date_first_iNat_obs, date_last_iNat_obs,
         first_year_both, `Species discovery`) %>%
  mutate(Site = "Royal")

# Now combine
combined_df <- bind_rows(yos_selected, roy_selected) %>% mutate(Site = factor(Site, levels = c("Yosemite", "Royal")))

# Count species discovery types per site
discovery_counts <- combined_df %>%
  group_by(Site, `Species discovery`) %>%
  summarize(Count = n(), .groups = "drop")

# Create plot
combined_plot <- ggplot(combined_df, aes(y = yaxis_order)) +
  geom_segment(aes(x = year_first_voucher, xend = year_last_voucher, yend = yaxis_order), 
               col="#608CB8", size = 0.2) +
  geom_segment(aes(x = date_first_iNat_obs, 
                   xend = date_last_iNat_obs, yend = yaxis_order), 
               col = "#74AC00", size = 0.2) +
  geom_point(aes(x = first_year_both, col=`Species discovery`), size = 0.1) +
  facet_wrap(~Site) +
  theme_classic() +
  labs(x = "Year", y = "", col="Species discovery") +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.line.y = element_blank(),
    axis.line.x = element_blank()
  ) +
  scale_color_manual(values = c("Voucher first"="#D55E00",
                                "Photograph first"="#0072B2",
                                "Voucher only"="#608CB8",
                                "Photograph only"="#74AC00",
                                "Both in the same year"="#CC79A7")) +
  guides(color = guide_legend(override.aes = list(size = 3)))

# Annotate counts directly in the plot
count_labels <- discovery_counts %>%
  mutate(label = paste(`Species discovery`, "=", Count)) %>%
  group_by(Site) %>%
  summarize(label = paste(label, collapse = "\n"), .groups = "drop")

combined_plot <- combined_plot +
  geom_text(data = count_labels, aes(x = 1850, y = Inf, label = label), 
            hjust = 0, vjust = 1.1, size = 3, inherit.aes = FALSE)

# Save the combined plot
ggsave("output/combined_species_discovery.pdf", combined_plot, width = 12, height = 10)
ggsave("output/combined_species_discovery.png", combined_plot, width = 12, height = 10)
ggsave("output/combined_species_discovery.svg", combined_plot, width = 12, height = 10)
