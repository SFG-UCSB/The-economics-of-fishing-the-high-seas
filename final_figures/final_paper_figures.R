library(tidyverse)
library(sf)
library(cowplot)
library(Cairo)
library(extrafont)

#extrafont::font_import()
loadfonts(quiet = T)

source(here::here("general_project_files/ihs_functions.R"))

source(here::here("general_project_files/gfw_themes.R"))

# Load results

world_map <- rnaturalearth::ne_coastline(scale = 'small', returnclass = c("sf"))

top_14_iso3 <- c("CHN","TWN","JPN","KOR","ESP", "ECU","RUS", "IDN", "MEX", "USA", "FRA", "NOR", "SYC","PAN")

top_14_country_names <- c("China","Taiwan","Japan","South Korea","Spain", "Ecuador", "Russia","Indonesia",  "Mexico", "United States", "France", "Norway", "Seychelles","Panama")

high_seas_profits_by_vessel_and_FAO_region <- 
  read_csv(here::here("profits/saved_files/high_seas_profits_by_vessel_and_FAO_region.csv")) %>% 
  filter(year == 2016)

Indo_high_seas_profits_by_vessel_and_FAO_region <- 
  read_csv(here::here("profits/saved_files/Indo_high_seas_profits_by_vessel_and_FAO_region.csv")) %>% 
  filter(year == 2016)

theme_multi <- theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"),
                     panel.border = element_rect(colour = "black", fill =  NA, size=1),
                     plot.title = element_blank(),
                     plot.caption = element_blank(),
                     plot.subtitle = element_blank(),
                     text = element_text(family = "MyriadPro-Regular"))

# Figure 1 ----------------------------------------------------------------
# Figure 1: High seas fleet by country and gear type


gear_colors <- read_rds("../general_project_files/gear_colors") 

indo_high_seas_fleet <- read_csv("../vessel_characteristics/saved_files/complete_high_seas_indo_vms_characteristics.csv") %>% 
  filter(year == 2016)

ais_high_seas_fleet <- read_csv("../vessel_characteristics/saved_files/complete_ais_vessel_characteristics.csv") %>% 
  filter(is_high_seas, year == 2016)

complete_high_seas_fishing_fleet <- 
  bind_rows(ais_high_seas_fleet %>% 
              select(year, mmsi, flag_country_name, sovereign_flag_country_name, gear_type = gear_type_all_years, sub_gear_type = sub_gear_type_all_years, length, tonnage, engine_power, aux_engine_power, 
                     crew = rf_crew,design_speed_ihs, main_sfc, main_sfc_low, aux_sfc,  hours_on_high_seas = hours_high_seas),
            indo_high_seas_fleet %>%
              mutate(flag_country_name = "Indonesia",
                     sovereign_flag_country_name = "Indonesia") %>% 
              select(year, mmsi, flag_country_name, sovereign_flag_country_name, gear_type = gfw_gear, sub_gear_type = gfw_sub_gear, length, tonnage, engine_power, aux_engine_power, crew,
                     design_speed_ihs, main_sfc, main_sfc_low, aux_sfc, hours_on_high_seas))

top_high_seas_countries_2016 <- complete_high_seas_fishing_fleet %>% 
  filter(year == 2016) %>% 
  group_by(sovereign_flag_country_name) %>% 
  summarise(vessels = n_distinct(mmsi),
            hours_on_high_seas = sum(hours_on_high_seas)) %>% 
  top_n(20,hours_on_high_seas) %>% 
  arrange(desc(hours_on_high_seas))

complete_high_seas_fishing_fleet <- complete_high_seas_fishing_fleet %>% 
  mutate(sub_gear_type = ifelse(sub_gear_type %in% c("lift_net","drifnets","oceanic_gillnets","shrimp_net"), "other_fishing",sub_gear_type ))

complete_high_seas_fishing_fleet <- complete_high_seas_fishing_fleet %>% 
  replace_na(list(sub_gear_type = "unknown", gear_type = "unknown")) %>% 
  mutate(sub_gear_type = str_replace_all(sub_gear_type, "_", " ")) %>% 
  mutate(gear_type = str_replace_all(gear_type, "_", " "))

(vessel_by_flag_and_gear.plot <- complete_high_seas_fishing_fleet %>% 
   ungroup() %>% 
   filter(year == 2016 &
            sovereign_flag_country_name %in% top_high_seas_countries_2016$sovereign_flag_country_name & 
            !is.na(sub_gear_type) & !is.na(sovereign_flag_country_name)) %>% 
   ungroup() %>% 
   mutate(sovereign_flag_country_name = ifelse(sovereign_flag_country_name == "Federated States of Micronesia",
                                               "Micronesia",
                                               sovereign_flag_country_name)) %>% 
   ungroup() %>% 
   ggplot()+
   geom_bar(aes(x = forcats::fct_reorder(sovereign_flag_country_name, mmsi, n_distinct), 
                fill = forcats::fct_reorder(sub_gear_type, mmsi,  n_distinct)))+
   coord_flip()+
   xlab("")+
   ylab("High Seas Vessels")+
   scale_fill_manual(values = gear_colors, name = " ")+
   guides(fill = guide_legend(reverse=T))+
   theme_minimal()+
   theme(legend.text = element_text(size = 8, hjust = 3, vjust = 3),
         legend.key.size = unit(.5, "cm")))+
  theme(text = element_text(family = "MyriadPro-Regular"))+
  theme(legend.text =  element_text(size = 8),
        legend.title = element_text(size = 10),
        legend.key.size = unit(1.5, 'lines'),
        legend.margin = margin(-0.5, unit ='cm'))

# Figure 2 ----------------------------------------------------------------
# Profit estimate ranges for the top 14 countries (a) and by fishing gears (b)

summary_results_by_country <- bind_rows(
  high_seas_profits_by_vessel_and_FAO_region %>% 
    #na.omit(revenue) %>% 
    filter(!is.na(revenue)) %>% 
    group_by(year, sovereign_flag_country_name) %>% 
    summarise(catch = sum(catch),
              revenue = sum(revenue),
              subsidies = sum(subsidies),
              unscaled__costs_low_bound = sum(high_seas_total_cost__low_bound),
              unscaled__costs_high_bound = sum(high_seas_total_cost__high_bound),
              unscaled__profits_low_bound = sum(profits__low_bound),
              unscaled__profits_high_bound = sum(profits__high_bound),
              unscaled__profits_with_subsidies_low_bound = sum(profits_with_subsidies__low_bound),
              unscaled__profits_with_subsidies_high_bound = sum(profits_with_subsidies__high_bound),
              scaled__costs_low_bound = sum(high_seas_scaled_total_cost__low_bound),
              scaled__costs_high_bound = sum(high_seas_scaled_total_cost__high_bound),
              scaled__profits_low_bound = sum(scaled_profits__low_bound),
              scaled__profits_high_bound = sum(scaled_profits__high_bound),
              scaled__profits_with_subsidies_low_bound = sum(scaled_profits_with_subsidies__low_bound),
              scaled__profits_with_subsidies_high_bound = sum(scaled_profits_with_subsidies__high_bound)           
    ) %>% 
    gather(variable, value, -revenue,-sovereign_flag_country_name,-year,-catch,-subsidies) %>% 
    separate(variable, into = c("scaled", "variable"), sep =  "__") %>% 
    spread(variable, value), 
  Indo_high_seas_profits_by_vessel_and_FAO_region %>% 
    filter(!is.na(revenue)) %>% 
    group_by(year, sovereign_flag_country_name) %>% 
    summarise(catch = sum(catch),
              revenue = sum(revenue),
              subsidies = sum(subsidies),
              unscaled__costs_low_bound = sum(high_seas_total_cost__low_bound),
              unscaled__costs_high_bound = sum(high_seas_total_cost__high_bound),
              unscaled__profits_low_bound = sum(profits__low_bound),
              unscaled__profits_high_bound = sum(profits__high_bound),
              unscaled__profits_with_subsidies_low_bound = sum(profits_with_subsidies__low_bound),
              unscaled__profits_with_subsidies_high_bound = sum(profits_with_subsidies__high_bound),
              scaled__costs_low_bound = sum(high_seas_total_cost__low_bound),
              scaled__costs_high_bound = sum(high_seas_total_cost__high_bound),
              scaled__profits_low_bound = sum(profits__low_bound),
              scaled__profits_high_bound = sum(profits__high_bound),
              scaled__profits_with_subsidies_low_bound = sum(profits_with_subsidies__low_bound),
              scaled__profits_with_subsidies_high_bound = sum(profits_with_subsidies__high_bound)           
    ) %>% 
    gather(variable, value, - revenue,-sovereign_flag_country_name,-year, -catch,-subsidies) %>% 
    separate(variable, into = c("scaled", "variable"), sep =  "__") %>% 
    spread(variable, value)) %>% 
  arrange(desc(revenue)) %>% 
  mutate(scaled = ifelse(scaled == "scaled", T, F)) %>% 
  ungroup() %>% 
  select(country = sovereign_flag_country_name,scaled, catch, revenue, costs_low_bound, costs_high_bound,  profits_low_bound, profits_high_bound,subsidies, profits_with_subsidies_low_bound,profits_with_subsidies_high_bound, -year)


profits_range_by_country <- summary_results_by_country %>% 
  filter(scaled, country %in% top_14_country_names) %>% 
  select(country,scaled, revenue, profits_low_bound, profits_high_bound, profits_with_subsidies_low_bound, profits_with_subsidies_high_bound) %>% 
  filter(scaled) %>% 
  gather(var, value, -country,-revenue,-scaled) %>% 
  mutate(var = stringr::str_replace_all(var, "profits_with_subsidies", "π*"),
         var = stringr::str_replace_all(var, "profits", "π"),
         var = stringr::str_replace_all(var, "_", " "),
         value = value/10^6) %>% 
  group_by(country) %>% 
  mutate(min = min(value),
         max = max(value)) %>% 
  ggplot()+
  geom_linerange(aes(x = forcats::fct_reorder(country, value), ymin = min, ymax  = max), col = 'azure4')+
  geom_point(aes(x = forcats::fct_reorder(country, value), y = value, shape = var,  col = value > 0), size = 2)+
  geom_hline(aes(yintercept = 1), linetype = 3) +
  theme_minimal() +
  coord_flip() +
  labs(x = "", y = " Profits")+
  scale_shape_manual(name = "Estimate", 
                     values = c(1,2,16,17))+
  theme(legend.margin = margin(t = 0, unit = 'cm'),
        panel.grid.minor = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank())+
  scale_y_continuous(labels = scales::comma)+
  scale_colour_manual(values = c("#FC4E07","#2E9FDF"), guide = FALSE)

summary_results_by_gear <- bind_rows(high_seas_profits_by_vessel_and_FAO_region %>% 
                                       filter(!is.na(revenue)) %>% 
                                       group_by(year, sub_gear_type) %>% 
                                       summarise(catch = sum(catch, na.rm = T),
                                                 revenue = sum(revenue, na.rm = T),
                                                 subsidies = sum(subsidies, na.rm = T),
                                                 unscaled__costs_low_bound = sum(high_seas_total_cost__low_bound),
                                                 unscaled__costs_high_bound = sum(high_seas_total_cost__high_bound),
                                                 unscaled__profits_low_bound = sum(profits__low_bound),
                                                 unscaled__profits_high_bound = sum(profits__high_bound),
                                                 unscaled__profits_with_subsidies_low_bound = sum(profits_with_subsidies__low_bound),
                                                 unscaled__profits_with_subsidies_high_bound = sum(profits_with_subsidies__high_bound),
                                                 scaled__costs_low_bound = sum(high_seas_scaled_total_cost__low_bound),
                                                 scaled__costs_high_bound = sum(high_seas_scaled_total_cost__high_bound),
                                                 scaled__profits_low_bound = sum(scaled_profits__low_bound),
                                                 scaled__profits_high_bound = sum(scaled_profits__high_bound),
                                                 scaled__profits_with_subsidies_low_bound = sum(scaled_profits_with_subsidies__low_bound),
                                                 scaled__profits_with_subsidies_high_bound = sum(scaled_profits_with_subsidies__high_bound)           
                                       ) %>% 
                                       gather(variable, value, - revenue,-sub_gear_type,-year,-subsidies,-catch) %>% 
                                       separate(variable, into = c("scaled", "variable"), sep =  "__") %>% 
                                       spread(variable, value), 
                                     Indo_high_seas_profits_by_vessel_and_FAO_region %>% 
                                       filter(!is.na(revenue)) %>% 
                                       group_by(year, sub_gear_type) %>% 
                                       summarise(catch = sum(catch, na.rm = T),
                                                 revenue = sum(revenue, na.rm = T),
                                                 subsidies = sum(subsidies, na.rm = T),
                                                 unscaled__costs_low_bound = sum(high_seas_total_cost__low_bound),
                                                 unscaled__costs_high_bound = sum(high_seas_total_cost__high_bound),
                                                 unscaled__profits_low_bound = sum(profits__low_bound),
                                                 unscaled__profits_high_bound = sum(profits__high_bound),
                                                 unscaled__profits_with_subsidies_low_bound = sum(profits_with_subsidies__low_bound),
                                                 unscaled__profits_with_subsidies_high_bound = sum(profits_with_subsidies__high_bound),
                                                 scaled__costs_low_bound = sum(high_seas_total_cost__low_bound),
                                                 scaled__costs_high_bound = sum(high_seas_total_cost__high_bound),
                                                 scaled__profits_low_bound = sum(profits__low_bound),
                                                 scaled__profits_high_bound = sum(profits__high_bound),
                                                 scaled__profits_with_subsidies_low_bound = sum(profits_with_subsidies__low_bound),
                                                 scaled__profits_with_subsidies_high_bound = sum(profits_with_subsidies__high_bound)           
                                       ) %>% 
                                       gather(variable, value, - revenue,-sub_gear_type,-year, -subsidies,-catch) %>% 
                                       separate(variable, into = c("scaled", "variable"), sep =  "__") %>% 
                                       spread(variable, value)) %>% 
  mutate(scaled = ifelse(scaled == "scaled", T, F)) %>% 
  ungroup() %>%
  group_by(sub_gear_type, scaled) %>% 
  summarize_all(sum) %>% 
  select(gear_type = sub_gear_type,scaled, catch , revenue, costs_low_bound, costs_high_bound,  profits_low_bound, profits_high_bound, subsidies,  profits_with_subsidies_low_bound,profits_with_subsidies_high_bound, -year)

profits_range_by_gear <- summary_results_by_gear %>% 
  filter(!is.na(gear_type), gear_type != "oceanic_gillnets") %>% 
  ungroup() %>% 
  mutate(gear_type = str_to_title(gear_type)) %>% 
  select(gear_type,scaled, revenue, profits_low_bound, profits_high_bound, profits_with_subsidies_low_bound, profits_with_subsidies_high_bound) %>% 
  filter(scaled) %>% 
  gather(var, value, -gear_type,-revenue,-scaled) %>% 
  mutate(var = stringr::str_replace_all(var, "profits_with_subsidies", "π*"),
         var = stringr::str_replace_all(var, "profits", "π"),
         var = stringr::str_replace_all(var, "_", " "),
         value = value/10^6) %>% 
  group_by(gear_type) %>% 
  mutate(min = min(value),
         max = max(value)) %>% 
  ungroup() %>% 
  mutate(gear_type = stringr::str_replace_all(gear_type, "_", " ")) %>% 
  ggplot()+
  geom_linerange(aes(x = forcats::fct_reorder(gear_type, value), ymin = min, ymax  = max), col = 'azure4')+
  geom_point(aes(x = forcats::fct_reorder(gear_type, value), y = value, shape = var,  col = value > 0), size = 2, )+
  geom_hline(aes(yintercept = 1), linetype = 3) +
  theme_minimal() +
  coord_flip() +
  labs(x = "", y = " Profits")+
  scale_y_continuous(labels = scales::comma)+
  scale_shape_manual(name = "Estimate", 
                     values = c(1,2,16,17))+
  theme(legend.margin = margin(t = 0, unit = 'cm'),
        panel.grid.minor = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank())+
  scale_colour_manual(values = c("#FC4E07","#2E9FDF"), guide = FALSE)

figure2_legend <-  get_legend(profits_range_by_gear +
                                theme(legend.text =  element_text(size = 8),
                                      legend.title = element_text(size = 10),
                                      legend.key.size = unit(1.5, 'lines'),
                                      legend.margin = margin(-0.5, unit='cm')))

(figure2 <- ggdraw()+
    draw_plot(profits_range_by_country +
                labs(title = "", subtitle = "") +
                theme_multi+
                theme(legend.position ="none",
                      legend.margin = margin(-0.5, unit='cm'),
                      legend.key.height = unit(.1, "cm"),
                      legend.key.width  = unit(1.5, "cm"),
                      legend.title = element_text(size = 10),
                      legend.text =  element_text(size = 8)),
              x = 0, y = 0, width = .4, height = 1)+
    draw_plot(profits_range_by_gear+
                labs(title = "", subtitle = "")+
                theme_multi+
                theme(legend.position ="none",
                      legend.margin = margin(-0.5, unit='cm'),
                      legend.key.height = unit(.1, "cm"),
                      legend.key.width  = unit(1.5, "cm"),
                      legend.title = element_text(size = 10),
                      legend.text =  element_text(size = 8)),
              x = .4, y = 0, width = .4, height = 1)+
    draw_plot(figure2_legend, x = 0.85, y = 0, width = .1, height = 1)+
    draw_plot_label(label = c("A.", "B."), 
                    size = 9,
                    x = c(0.01, 0.42),
                    y = c(1, 1),
                    hjust = 0))

# Figure 3 ----------------------------------------------------------------
# Maps of A.effort, B.cost, C.revenue, D.profits, E.profits with subsidies, F.profits with low labor cost

half_degree_binned_results <- read_csv(
  here::here("profits/saved_files/half_degree_binned_results.csv")) %>% 
  filter(year == 2016)

limits <- half_degree_binned_results %>%
  group_by(lon_bin_center, lat_bin_center) %>% 
  summarize(mean_scaled_profits = sum(mean_scaled_profits, na.rm = T)/1000) %>% 
  ungroup() %>% 
  summarize(min = min(mean_scaled_profits), max =  max(mean_scaled_profits))

global_effort_map <- half_degree_binned_results %>%
  group_by(lon_bin_center, lat_bin_center) %>% 
  summarize(fishing_KWH = sum(fishing_KWH, na.rm = T)/1000) %>% 
  filter(fishing_KWH >= 0.9) %>% 
  mutate(fishing_KWH = ifelse(fishing_KWH > 5000, 5000, fishing_KWH),
         fishing_KWH = ifelse(fishing_KWH < 10, 10, fishing_KWH)) %>% 
  ungroup() %>% 
  ggplot() +
  geom_sf(data = world_map, size = .2) +
  geom_raster(aes(lon_bin_center, lat_bin_center, fill = fishing_KWH), 
              interpolate = F, 
              show.legend = T) +
  viridis::scale_fill_viridis(name = "fishing energy (thousand KWh)",
                              trans = "log", 
                              breaks = c(10,30,100, 300,1000, 5000),
                              labels = c("<10","30","100", "300","1,000", ">5,000"),
                              #option = "A",
                              direction = -1) +
  labs(x = "",
       y = "",
       title = "",
       subtitle = "") +
  guides(fill = guide_colourbar(title.position = "top", 
                                title.hjust = 0.5,
                                label.theme = element_text(angle = 0, size = 9)))+
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0))+
  light_gfw_theme()

global_costs_map <- half_degree_binned_results %>%
    group_by(lon_bin_center, lat_bin_center) %>% 
    summarize(mean_costs = sum(mean_costs, na.rm = T)/1000) %>% 
    filter(mean_costs >= 0.9) %>%
    mutate(mean_costs = ifelse(mean_costs < 10, 10 , mean_costs),
           mean_costs = ifelse(mean_costs > 10000, 10000 , mean_costs)) %>% 
    ungroup() %>% 
    ggplot() +
    geom_sf(data = world_map, size = .2) +
    geom_raster(aes(lon_bin_center, lat_bin_center, fill = mean_costs), 
                interpolate = F, 
                show.legend = T) +
    guides(fill = guide_colourbar(title.position = "top", 
                                  title.hjust = 0.5,
                                  label.theme = element_text(angle = 0, size = 9)))+
    scale_fill_gradientn(trans = "log",
                         name = "Costs (thousand usd)",
                         labels = c('<10',100, 1000, ">10,000"),
                         breaks = c(10,100, 1000, 10000),
                         colours = c("#fee0d2","#fcbba1","#ef3b2c","#cb181d","#67000d"))+
    scale_x_continuous(expand = c(0,0))+
    scale_y_continuous(expand = c(0,0))+
    light_gfw_theme()

global_revenue_map <- half_degree_binned_results %>%
  group_by(lon_bin_center, lat_bin_center) %>% 
  summarize(revenue = sum(revenue, na.rm = T)/1000) %>% 
  filter(revenue >= 0.9) %>% 
  mutate(revenue = ifelse(revenue < 10, 10, revenue),
         revenue = ifelse(revenue > 10000,10000, revenue)) %>% 
  ungroup() %>% 
  ggplot() +
  geom_sf(data = world_map, size = .2) +
  geom_raster(aes(lon_bin_center, lat_bin_center, fill = revenue), 
              interpolate = F, 
              show.legend = T) +
  guides(fill = guide_colourbar(title.position = "top", 
                                title.hjust = 0.5,
                                label.theme = element_text(angle = 0, size = 9)))+
  scale_fill_gradientn(trans = "log",
                       name = "Revenue (thousand usd)",
                       labels = c("<10",100, "1,000", "> 10,000"),
                       breaks = c(10,100, 1000, 10000),
                       colours = c("#c6dbef","#6baed6","#08519c","#08306b")
  )+
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0))+
  light_gfw_theme()

skimr::skim(half_degree_binned_results)

round(quantile(half_degree_binned_results$mean_scaled_profits, probs = c(0,2.5,5,10,20,30,40,50,60,70,80,90,95,97.5,100)/100, na.rm = T))

half_degree_binned_results %>% 
  mutate(g = cut(mean_scaled_profits, breaks = c(-Inf,-500000,-200000,-100000,-50000, -10000, -1000, 1000, 10000,50000,100000,200000,500000, Inf))) %>% 
  group_by(g) %>% 
  count()

discrete_breaks <- c(-Inf,-500000,-200000,-100000,-50000, -10000, -1000, 1000,10000,50000,100000,200000,500000,Inf)

discrete_labels <- c("< -500","-200","-100", -50, -10, -1, 1, 10, 50, "100", "200",">500","")

colors <-  c(rev(c("#fed976","#feb24c","#fd8d3c","#fc4e2a","#e31a1c","#bd0026")),
             "lightgray",
             "#c6dbef","#9ecae1","#6baed6","#3182bd","#08519c","#08306b")

(discrete_profits_map <- half_degree_binned_results %>%
    group_by(lon_bin_center, lat_bin_center) %>% 
    summarize(mean_scaled_profits = sum(mean_scaled_profits, na.rm = T)) %>% 
    filter(mean_scaled_profits != 0) %>% 
    mutate(int = cut(mean_scaled_profits, 
                     breaks = discrete_breaks,
                     labels = discrete_labels)) %>% 
    ungroup() %>% 
    ggplot()+
    geom_sf(data = world_map, size = .1) +
    geom_raster(aes(x = lon_bin_center, y = lat_bin_center, fill = int),
                interpolate = F, 
                show.legend = T) + 
    scale_fill_manual(values = colors)+
    labs(x = "",
         y = "",
         title = "",
         subtitle = "")+
    scale_x_continuous(expand = c(0,0))+
    scale_y_continuous(expand = c(0,0))+
    light_gfw_theme()+
    theme(panel.background = element_rect(fill = "white"),
          plot.background = element_rect(fill = "white"),
          legend.background = element_rect(fill = "white"),
          legend.title = element_text(colour = "black"),
          legend.text  = element_text(colour = "black"))+
    guides(fill = guide_legend(title.position = "top", 
                               title = "Profits (thousand $)",
                               title.hjust = 0.5,
                               keywidth = 1.5,
                               label.position = "bottom",
                               label.hjust = 1.21,
                               nrow = 1,
                               barwidth = 0.5,
                               label.theme = element_text(angle = 0, size = 9, colour = "black"))))
    
  
(discrete_profits_map_with_subsidies <- half_degree_binned_results %>%
  group_by(lon_bin_center, lat_bin_center) %>% 
  summarize(mean_scaled_profits_with_subsidies = sum(mean_scaled_profits_with_subsidies, na.rm = T)) %>% 
  filter(mean_scaled_profits_with_subsidies != 0) %>% 
  mutate(int = cut(mean_scaled_profits_with_subsidies, 
                   breaks = discrete_breaks,
                   labels = discrete_labels)) %>% 
    ungroup() %>% 
  ggplot()+
  geom_sf(data = world_map, size = .1) +
  geom_raster(aes(x = lon_bin_center, y = lat_bin_center, fill = int),
              interpolate = F, 
              show.legend = T) + 
  scale_fill_manual(values = colors)+
  labs(x = "",
       y = "",
       title = "",
       subtitle = "")+
  guides(fill = guide_colourbar(title.position = "top", 
                                title.hjust = 0.5,
                                label.theme = element_text(angle = 0, size = 9)))+
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0))+
  light_gfw_theme()+
  theme(panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"),
        legend.background = element_rect(fill = "white"),
        legend.title = element_text(colour = "black"),
        legend.text  = element_text(colour = "black"))+
  guides(fill = guide_legend(title.position = "top", 
                             title = "Profits (thousand $)",
                             title.hjust = 0.5,
                             keywidth = 1.5,
                             label.position = "bottom",
                             label.hjust = 1.21,
                             nrow = 1,
                             label.theme = element_text(angle = 0, size = 9, colour = "black"))))

(discrete_profits_map_with_subsidies_and_low_labor_costs <- half_degree_binned_results %>% 
  group_by(lon_bin_center, lat_bin_center) %>% 
  summarize(scaled_profits_low_labor_cost = sum(scaled_profits_low_labor_cost, na.rm = T)) %>% 
  filter(scaled_profits_low_labor_cost != 0) %>% 
  mutate(int = cut(scaled_profits_low_labor_cost, 
                   breaks = discrete_breaks,
                   labels = discrete_labels)) %>% 
  ungroup() %>% 
  ggplot()+
  geom_sf(data = world_map, size = .1) +
  geom_raster(aes(x = lon_bin_center, y = lat_bin_center, fill = int),
              interpolate = F, 
              show.legend = T) + 
  scale_fill_manual(values = colors)+
  labs(x = "",
       y = "",
       title = "",
       subtitle = "")+
  guides(fill = guide_colourbar(title.position = "top", 
                                title.hjust = 0.5,
                                label.theme = element_text(angle = 0, size = 9)))+
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0))+
  light_gfw_theme()+
  theme(panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"),
        legend.background = element_rect(fill = "white"),
        legend.title = element_text(colour = "black"),
        legend.text  = element_text(colour = "black"))+
  guides(fill = guide_legend(title.position = "top", 
                             title = "Profits (thousand $)",
                             title.hjust = 0.5,
                             keywidth = 1.5,
                             label.position = "bottom",
                             label.hjust = 1.21,
                             nrow = 1,
                             label.theme = element_text(angle = 0, size = 9, colour = "black"))))

(figure3_discrete <- ggdraw()+
    draw_plot(global_effort_map +
                labs(title = "", subtitle = "") +
                theme_multi+
                theme(legend.margin = margin(-0.5, unit='cm'),
                      legend.key.height = unit(.1, "cm"),
                      legend.key.width  = unit(1.5, "cm"),
                      legend.title = element_text(size = 10),
                      legend.text =  element_text(size = 8)), x = 0, y = .66, width = .5, height = .33)+
    draw_plot(global_costs_map+
                labs(title = "", subtitle = "")+
                theme_multi+
                theme(legend.margin = margin(-0.5, unit='cm'),
                      legend.key.height = unit(.1, "cm"),
                      legend.key.width  = unit(1.5, "cm"),
                      legend.title = element_text(size = 10),
                      legend.text =  element_text(size = 8)), x = .5, y = .66, width = .5, height = .33)+
    draw_plot(global_revenue_map+
                labs(title = "", subtitle = "")+
                theme_multi+
                theme(legend.margin = margin(-0.5, unit='cm'),
                      legend.key.height = unit(.1, "cm"),
                      legend.key.width  = unit(1.5, "cm"),
                      legend.title = element_text(size = 10),
                      legend.text =  element_text(size = 8)), x = 0, y = .33, width = .5, height = .33)+
    draw_plot(discrete_profits_map+
                labs(title = "", subtitle = "")+
                theme_multi+
                theme(legend.margin = margin(-0.5, unit='cm'),
                      legend.key.height = unit(.1, "cm"),
                      legend.key.width  = unit(.6, "cm"),
                      legend.title = element_text(size = 10),
                      legend.text =  element_text(size = 8)), x = .5, y = 0.33, width = .5, height = .33)+
    draw_plot(discrete_profits_map_with_subsidies+
                labs(title = "", subtitle = "")+
                theme_multi+
                theme(legend.margin = margin(-0.5, unit='cm'),
                      legend.key.height = unit(.1, "cm"),
                      legend.key.width  = unit(.6, "cm"),
                      legend.title = element_text(size = 10),
                      legend.text =  element_text(size = 8)), x = 0, y = 0, width = .5, height = .33)+
    draw_plot(discrete_profits_map_with_subsidies_and_low_labor_costs+
                theme_multi+
                theme(legend.margin = margin(-0.5, unit='cm'),
                      legend.key.height = unit(.1, "cm"),
                      legend.key.width  = unit(.6, "cm"),
                      legend.title = element_text(size = 10),
                      legend.text =  element_text(size = 8)), x = 0.5, y = 0, width = .5, height = .33) +
    draw_plot_label(label = c("A. Effort", "B. Costs", "C. Revenue", "D. Profits",
                              "E. Profits + Subsidies", "F. Profits + Subsidies (low labor cost bound)"), 
                    size = 9,
                    x = c(0.01, 0.52, 0.01, .52, 0.01, .52),
                    y = c(1, 1, .66, .66,.33, .33),
                    hjust = 0))

# Figure 4 ----------------------------------------------------------------
# Figure 4: Panel figure of maps of profits with and without subsidies for China, Taiwan, Japan, Spain and Korea

half_degree_binned_profits_by_iso3_and_fao_region <- read_csv(
  here::here("profits/saved_files/half_degree_binned_profits_by_iso3_and_fao_region.csv")) %>% 
  filter(year == 2016)

binned_avg_profits_by_iso3 <- half_degree_binned_profits_by_iso3_and_fao_region %>%
  group_by(iso3, lon_bin_center, lat_bin_center) %>% 
  summarize(profits = sum(scaled_profits__low_bound + scaled_profits__high_bound, na.rm = T)/2000,
            profits_with_subsidies = sum(scaled_profits_with_subsidies__low_bound + scaled_profits_with_subsidies__high_bound, 
                                         na.rm = T)/2000) %>% 
  filter(profits != 0) %>% 
  gather(var, value, -lon_bin_center,-lat_bin_center, -iso3) %>% 
  ungroup()

## China 

round(quantile(binned_avg_profits_by_iso3$value[binned_avg_profits_by_iso3$var == "profits" & binned_avg_profits_by_iso3$iso3 == "CHN"], 
               probs = c(0,2.5,5,10,20,30,40,50,60,70,80,90,95,97.5,100)/100, na.rm = T))


(CHN_discrete_profits <- binned_avg_profits_by_iso3 %>% 
  filter(iso3 == "CHN") %>% 
    group_by(lon_bin_center, lat_bin_center, var) %>% 
    summarize(value = sum(value, na.rm = T)) %>% 
    ungroup() %>% 
  mutate(int = cut(value, 
                   breaks = c(-Inf, -200,-100,-50,-10,-1,1,10,50,100,200,Inf),
                   labels = c('< -200',-100,-50,-10,-1,1,10,50,100,"> 200",""))) %>% 
  ggplot()+
  geom_sf(data = world_map, size = .1) +
  geom_raster(aes(x = lon_bin_center, y = lat_bin_center, fill = int),
              interpolate = F, 
              show.legend = T) +
  scale_fill_manual(values =  c(rev(c("#fed976","#feb24c","#fd8d3c","#fc4e2a","#e31a1c")),
                                          "lightgray",
                                          "#c6dbef","#9ecae1","#6baed6","#3182bd","#08519c","#08306b"))+
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  light_gfw_theme() +
    theme(text = element_text(family = "MyriadPro-Regular"))+
  facet_wrap(~var) +
  theme(legend.position = "bottom",
        strip.background = element_blank(), 
        strip.text = element_text(color = "transparent"),
        panel.spacing.y = unit(-0.8, "lines"),
        legend.margin = margin(-0.5, unit='cm'),
        legend.title = element_text(size = 12),
        legend.text =  element_text(size = 12))+
    guides(fill = guide_legend(title.position = "top", 
                               title = "Profits (thousand $)",
                               title.hjust = 0.5,
                               keywidth = 1.5,
                               label.position = "bottom",
                               label.hjust = 1.3,
                               nrow = 1,
                               barwidth = 0.5,
                               label.theme = element_text(angle = 0, size = 9, colour = "black"))))


legend <- get_legend(CHN_discrete_profits)

## Taiwan

round(quantile(binned_avg_profits_by_iso3$value[binned_avg_profits_by_iso3$var == "profits" & binned_avg_profits_by_iso3$iso3 == "TWN"], 
               probs = c(0,2.5,5,10,20,30,40,50,60,70,80,90,95,97.5,100)/100, na.rm = T))

(TWN_discrete_profits <- binned_avg_profits_by_iso3 %>% 
    filter(iso3 == "TWN") %>% 
    mutate(int = cut(value, 
                     breaks = c(-Inf, -200,-100,-50,-10,-1,1,10,50,100,200,Inf),
                     labels = c(-200,-100,-50,-10,-1,1,10,50,100,200,""))) %>% 
    ggplot()+
    geom_sf(data = world_map, size = .1) +
    geom_raster(aes(x = lon_bin_center, y = lat_bin_center, fill = int),
                interpolate = F, 
                show.legend = F) +
    scale_fill_manual(values =  c(rev(c("#fed976","#feb24c","#fd8d3c","#fc4e2a","#e31a1c")),
                                  "lightgray",
                                  "#c6dbef","#9ecae1","#6baed6","#3182bd","#08519c","#08306b"))+
    scale_x_continuous(expand = c(0,0)) +
    scale_y_continuous(expand = c(0,0)) +
    light_gfw_theme() +
    facet_wrap(~var) +
    theme(legend.position = "right",
          strip.background = element_blank(), 
          strip.text = element_text(color = "transparent"),
          panel.spacing.y = unit(-0.8, "lines"),
          legend.margin = margin(-0.5, unit='cm'),
          legend.key.height = unit(5, "cm"),
          legend.key.width  = unit(3, "cm"),
          legend.title = element_text(size = 12),
          legend.text =  element_text(size = 12)))

## Japan

round(quantile(binned_avg_profits_by_iso3$value[binned_avg_profits_by_iso3$var == "profits" & binned_avg_profits_by_iso3$iso3 == "JPN"], 
               probs = c(0,2.5,5,10,20,30,40,50,60,70,80,90,95,97.5,100)/100, na.rm = T))

(JPN_discrete_profits <- binned_avg_profits_by_iso3 %>% 
    filter(iso3 == "JPN") %>% 
    mutate(int = cut(value, 
                     breaks = c(-Inf, -200,-100,-50,-10,-1,1,10,50,100,200,Inf),
                     labels = c(-200,-100,-50,-10,-1,1,10,50,100,200,""))) %>% 
    ggplot()+
    geom_sf(data = world_map, size = .1) +
    geom_raster(aes(x = lon_bin_center, y = lat_bin_center, fill = int),
                interpolate = F, 
                show.legend = F) +
    scale_fill_manual(values =  c(rev(c("#fed976","#feb24c","#fd8d3c","#fc4e2a","#e31a1c")),
                                  "lightgray",
                                  "#c6dbef","#9ecae1","#6baed6","#3182bd","#08519c","#08306b"))+
    scale_x_continuous(expand = c(0,0)) +
    scale_y_continuous(expand = c(0,0)) +
    light_gfw_theme() +
    facet_wrap(~var) +
    theme(legend.position = "right",
          strip.background = element_blank(), 
          strip.text = element_text(color = "transparent"),
          panel.spacing.y = unit(-0.8, "lines"),
          legend.margin = margin(-0.5, unit='cm'),
          legend.key.height = unit(5, "cm"),
          legend.key.width  = unit(3, "cm"),
          legend.title = element_text(size = 12),
          legend.text =  element_text(size = 12)))

## Spain

round(quantile(binned_avg_profits_by_iso3$value[binned_avg_profits_by_iso3$var == "profits" & binned_avg_profits_by_iso3$iso3 == "ESP"], 
               probs = c(0,2.5,5,10,20,30,40,50,60,70,80,90,95,97.5,100)/100, na.rm = T))

(ESP_discrete_profits <- binned_avg_profits_by_iso3 %>% 
    filter(iso3 == "ESP") %>% 
    mutate(int = cut(value, 
                     breaks = c(-Inf, -200,-100,-50,-10,-1,1,10,50,100,200,Inf),
                     labels = c(-200,-100,-50,-10,-1,1,10,50,100,200,""))) %>% 
    ggplot()+
    geom_sf(data = world_map, size = .1) +
    geom_raster(aes(x = lon_bin_center, y = lat_bin_center, fill = int),
                interpolate = F, 
                show.legend = F) +
    scale_fill_manual(values =  c(rev(c("#fed976","#feb24c","#fd8d3c","#fc4e2a","#e31a1c")),
                                  "lightgray",
                                  "#c6dbef","#9ecae1","#6baed6","#3182bd","#08519c","#08306b"))+
    scale_x_continuous(expand = c(0,0)) +
    scale_y_continuous(expand = c(0,0)) +
    light_gfw_theme() +
    facet_wrap(~var) +
    theme(legend.position = "right",
          strip.background = element_blank(), 
          strip.text = element_text(color = "transparent"),
          panel.spacing.y = unit(-0.8, "lines"),
          legend.margin = margin(-0.5, unit='cm'),
          legend.key.height = unit(5, "cm"),
          legend.key.width  = unit(3, "cm"),
          legend.title = element_text(size = 12),
          legend.text =  element_text(size = 12))+
    guides(fill = guide_legend(title.position = "top", 
                               title.hjust = 0.5,
                               title = "",
                               title.theme = element_text(angle = 0, size = rel(8)),
                               keyheight = .6,
                               reverse = T,
                               keywidth = .2,
                               barheight = 7,
                               barwidth = 0.4,
                               label.theme = element_text(angle = 0, size = rel(8)),
                               label.vjust = 2.5)))
## Korea

round(quantile(binned_avg_profits_by_iso3$value[binned_avg_profits_by_iso3$var == "profits" & binned_avg_profits_by_iso3$iso3 == "KOR"], 
               probs = c(0,2.5,5,10,20,30,40,50,60,70,80,90,95,97.5,100)/100, na.rm = T))

(KOR_discrete_profits <- binned_avg_profits_by_iso3 %>% 
    filter(iso3 == "KOR") %>% 
    mutate(int = cut(value, 
                     breaks = c(-Inf, -200,-100,-50,-10,-1,1,10,50,100,200,Inf),
                     labels = c(-200,-100,-50,-10,-1,1,10,50,100,200,""))) %>% 
    ggplot()+
    geom_sf(data = world_map, size = .1) +
    geom_raster(aes(x = lon_bin_center, y = lat_bin_center, fill = int),
                interpolate = F, 
                show.legend = F) +
    scale_fill_manual(values =  c(rev(c("#fed976","#feb24c","#fd8d3c","#fc4e2a","#e31a1c")),
                                  "lightgray",
                                  "#c6dbef","#9ecae1","#6baed6","#3182bd","#08519c","#08306b"))+
    scale_x_continuous(expand = c(0,0)) +
    scale_y_continuous(expand = c(0,0)) +
    light_gfw_theme() +
    facet_wrap(~var) +
    theme(legend.position = "right",
          strip.background = element_blank(), 
          strip.text = element_text(color = "transparent"),
          panel.spacing.y = unit(-0.8, "lines"),
          legend.margin = margin(-0.5, unit='cm'),
          legend.key.height = unit(5, "cm"),
          legend.key.width  = unit(3, "cm"),
          legend.title = element_text(size = 12),
          legend.text =  element_text(size = 12))+
    guides(fill = guide_legend(title.position = "top", 
                               title.hjust = 0.5,
                               title = "",
                               title.theme = element_text(angle = 0, size = rel(8)),
                               keyheight = .6,
                               reverse = T,
                               keywidth = .2,
                               barheight = 7,
                               barwidth = 0.4,
                               label.theme = element_text(angle = 0, size = rel(8)),
                               label.vjust = 2.5)))

figure4_discrete <- ggdraw()+
  draw_plot(CHN_discrete_profits +
              guides(fill = FALSE)+
              theme_multi,
            x = 0, y = .81, width = 1, height = .19)+
  draw_plot(TWN_discrete_profits +
              theme_multi, 
            x = 0, y = .63, width = 1, height = .19)+
  draw_plot(JPN_discrete_profits +
              theme_multi, 
            x = 0, y = .44, width = 1, height = .19)+
  draw_plot(ESP_discrete_profits +
              theme_multi, 
            x = 0, y = .25, width = 1, height = .19)+
  draw_plot(KOR_discrete_profits +
              theme_multi ,
            x = 0, y = 0.06, width = 1, height = .19)+
  draw_label("Profits",  x = .22, y = .99, size = 10)+
  draw_label("Profits + Subsidies",  x = .7, y = .99, size = 10)+
  draw_plot_label(label = c("A. China", "B. Taiwan", "C. Japan", "D. Spain",
                            "E. South Korea"), 
                  size = 9,
                  x = c(0,0,0,0,0),
                  y = c(1,.82,.63,.44,.25),
                  hjust = 0)+
  draw_plot(legend, x = 0.33, y = 0, width = .33, height = .05)

# Figure 5 ----------------------------------------------------------------
# Figure 5: FAO regions and heat maps of profits by top 14 countries and gear type. 

summary_results_by_country_and_FAO <- bind_rows(
  high_seas_profits_by_vessel_and_FAO_region %>% 
    filter(!is.na(revenue)) %>% 
    group_by(year, sovereign_flag_country_name, FAO_region) %>% 
    summarise(catch = sum(catch),
              revenue = sum(revenue),
              subsidies = sum(subsidies),
              unscaled__costs_low_bound = sum(high_seas_total_cost__low_bound),
              unscaled__costs_high_bound = sum(high_seas_total_cost__high_bound),
              unscaled__profits_low_bound = sum(profits__low_bound),
              unscaled__profits_high_bound = sum(profits__high_bound),
              unscaled__profits_with_subsidies_low_bound = sum(profits_with_subsidies__low_bound),
              unscaled__profits_with_subsidies_high_bound = sum(profits_with_subsidies__high_bound),
              scaled__costs_low_bound = sum(high_seas_scaled_total_cost__low_bound),
              scaled__costs_high_bound = sum(high_seas_scaled_total_cost__high_bound),
              scaled__profits_low_bound = sum(scaled_profits__low_bound),
              scaled__profits_high_bound = sum(scaled_profits__high_bound),
              scaled__profits_with_subsidies_low_bound = sum(scaled_profits_with_subsidies__low_bound),
              scaled__profits_with_subsidies_high_bound = sum(scaled_profits_with_subsidies__high_bound)           
    ) %>% 
    gather(variable, value, -revenue,-sovereign_flag_country_name,-year,-catch,-subsidies, -FAO_region) %>% 
    separate(variable, into = c("scaled", "variable"), sep =  "__") %>% 
    spread(variable, value), 
  Indo_high_seas_profits_by_vessel_and_FAO_region %>% 
    filter(!is.na(revenue)) %>% 
    group_by(year, sovereign_flag_country_name, FAO_region) %>% 
    summarise(catch = sum(catch),
              revenue = sum(revenue),
              subsidies = sum(subsidies),
              unscaled__costs_low_bound = sum(high_seas_total_cost__low_bound),
              unscaled__costs_high_bound = sum(high_seas_total_cost__high_bound),
              unscaled__profits_low_bound = sum(profits__low_bound),
              unscaled__profits_high_bound = sum(profits__high_bound),
              unscaled__profits_with_subsidies_low_bound = sum(profits_with_subsidies__low_bound),
              unscaled__profits_with_subsidies_high_bound = sum(profits_with_subsidies__high_bound),
              scaled__costs_low_bound = sum(high_seas_total_cost__low_bound),
              scaled__costs_high_bound = sum(high_seas_total_cost__high_bound),
              scaled__profits_low_bound = sum(profits__low_bound),
              scaled__profits_high_bound = sum(profits__high_bound),
              scaled__profits_with_subsidies_low_bound = sum(profits_with_subsidies__low_bound),
              scaled__profits_with_subsidies_high_bound = sum(profits_with_subsidies__high_bound)           
    ) %>% 
    gather(variable, value, - revenue,-sovereign_flag_country_name,-year, -catch,-subsidies, -FAO_region) %>% 
    separate(variable, into = c("scaled", "variable"), sep =  "__") %>% 
    spread(variable, value)) %>% 
  arrange(desc(revenue)) %>% 
  mutate(scaled = ifelse(scaled == "scaled", T, F)) %>% 
  ungroup() %>% 
  select(country = sovereign_flag_country_name, FAO_region, scaled, catch, revenue, costs_low_bound, costs_high_bound,  profits_low_bound, profits_high_bound,subsidies, profits_with_subsidies_low_bound,profits_with_subsidies_high_bound, -year)

profits_by_country_and_FAO_tileplot <- summary_results_by_country_and_FAO %>% 
  filter(country %in% top_14_country_names, scaled) %>% 
  ggplot(aes( x = forcats::fct_reorder(factor(FAO_region), country, n_distinct, .desc = TRUE), 
              y = forcats::fct_reorder(country, FAO_region, n_distinct))) + 
  geom_tile(aes(fill = (profits_low_bound + profits_high_bound)/(2*10^6)), colour = "white") + 
  scale_fill_gradientn("million $", 
                       colors = c('#d73027','#f46d43','#fdae61','#fdae61','#fee090','#fee090','gray',
                                  '#e0f3f8','#abd9e9','#abd9e9','#74add1','#4575b4','#313695'),
                       labels = scales::comma,
                       breaks = c(-500,-30,0,10,200,4000),
                       trans = scales::trans_new("ihs_trans", 
                                                 transform = ihs,
                                                 inverse = ihs_inverse,
                                                 breaks = ihs_breaks(20),
                                                 minor_breaks = scales::regular_minor_breaks()))+
  theme_bw()+
  theme(panel.grid=element_blank(), panel.border=element_blank())+
  labs(x = "FAO region", y = "")+
  coord_fixed(ratio = 1)

summary_results_by_gear_and_FAO <- bind_rows(
  high_seas_profits_by_vessel_and_FAO_region %>% 
    filter(!is.na(revenue)) %>% 
    group_by(year, sub_gear_type, FAO_region) %>% 
    summarise(catch = sum(catch),
              revenue = sum(revenue),
              subsidies = sum(subsidies),
              unscaled__costs_low_bound = sum(high_seas_total_cost__low_bound),
              unscaled__costs_high_bound = sum(high_seas_total_cost__high_bound),
              unscaled__profits_low_bound = sum(profits__low_bound),
              unscaled__profits_high_bound = sum(profits__high_bound),
              unscaled__profits_with_subsidies_low_bound = sum(profits_with_subsidies__low_bound),
              unscaled__profits_with_subsidies_high_bound = sum(profits_with_subsidies__high_bound),
              scaled__costs_low_bound = sum(high_seas_scaled_total_cost__low_bound),
              scaled__costs_high_bound = sum(high_seas_scaled_total_cost__high_bound),
              scaled__profits_low_bound = sum(scaled_profits__low_bound),
              scaled__profits_high_bound = sum(scaled_profits__high_bound),
              scaled__profits_with_subsidies_low_bound = sum(scaled_profits_with_subsidies__low_bound),
              scaled__profits_with_subsidies_high_bound = sum(scaled_profits_with_subsidies__high_bound)           
    ) %>% 
    gather(variable, value, -revenue,-sub_gear_type,-year,-catch,-subsidies, -FAO_region) %>% 
    separate(variable, into = c("scaled", "variable"), sep =  "__") %>% 
    spread(variable, value), 
  Indo_high_seas_profits_by_vessel_and_FAO_region %>% 
    filter(!is.na(revenue)) %>% 
    group_by(year, sub_gear_type, FAO_region) %>% 
    summarise(catch = sum(catch),
              revenue = sum(revenue),
              subsidies = sum(subsidies),
              unscaled__costs_low_bound = sum(high_seas_total_cost__low_bound),
              unscaled__costs_high_bound = sum(high_seas_total_cost__high_bound),
              unscaled__profits_low_bound = sum(profits__low_bound),
              unscaled__profits_high_bound = sum(profits__high_bound),
              unscaled__profits_with_subsidies_low_bound = sum(profits_with_subsidies__low_bound),
              unscaled__profits_with_subsidies_high_bound = sum(profits_with_subsidies__high_bound),
              scaled__costs_low_bound = sum(high_seas_total_cost__low_bound),
              scaled__costs_high_bound = sum(high_seas_total_cost__high_bound),
              scaled__profits_low_bound = sum(profits__low_bound),
              scaled__profits_high_bound = sum(profits__high_bound),
              scaled__profits_with_subsidies_low_bound = sum(profits_with_subsidies__low_bound),
              scaled__profits_with_subsidies_high_bound = sum(profits_with_subsidies__high_bound)           
    ) %>% 
    gather(variable, value, - revenue,-sub_gear_type,-year, -catch,-subsidies, -FAO_region) %>% 
    separate(variable, into = c("scaled", "variable"), sep =  "__") %>% 
    spread(variable, value)) %>% 
  arrange(desc(revenue)) %>% 
  mutate(scaled = ifelse(scaled == "scaled", T, F)) %>% 
  ungroup() %>% 
  select(gear_type = sub_gear_type, FAO_region, scaled, catch, revenue, costs_low_bound, costs_high_bound,  profits_low_bound, profits_high_bound,subsidies, profits_with_subsidies_low_bound,profits_with_subsidies_high_bound, -year)

summary_results_by_gear_and_FAO$FAO_region <- factor(summary_results_by_gear_and_FAO$FAO_region, c(77,87, 51,34,41,47,57,71,81,27,61,21,31,48,88))

profits_by_gear_and_FAO_tileplot <- summary_results_by_gear_and_FAO %>% 
  filter(!is.na(gear_type), gear_type != "oceanic_gillnets", scaled == T) %>% 
  mutate(gear_type = str_to_title(gear_type)) %>% 
  mutate(gear_type = (stringr::str_replace_all(gear_type, "_", " "))) %>% 
  ggplot(aes( x = FAO_region,
              y = forcats::fct_reorder(gear_type, FAO_region, n_distinct))) + 
  geom_tile(aes(fill = (profits_low_bound + profits_high_bound)/(2*10^6)), colour = "white") + 
  scale_fill_gradientn("million $", 
                       colors = c('#d73027','#f46d43','#fdae61','#fdae61','#fee090','#fee090','gray',
                                  '#e0f3f8','#abd9e9','#abd9e9','#74add1','#4575b4','#313695'),
                       labels = scales::comma,
                       breaks = c(-500,-30,0,10,200,4000),
                       trans = scales::trans_new("ihs_trans", 
                                                 transform = ihs,
                                                 inverse = ihs_inverse,
                                                 breaks = ihs_breaks(20),
                                                 minor_breaks = scales::regular_minor_breaks()))+
  theme_bw()+
  theme(panel.grid=element_blank(), panel.border=element_blank())+
  labs(x = "FAO region", y = "")+
  coord_fixed(ratio = 1)

major_fao_shp <- sf::st_read(dsn = "../general_project_files/major_fao_regions/major_FAO.shp")

major_fao_shp <- rmapshaper::ms_simplify(input = as(major_fao_shp, 'Spatial')) %>%
  st_as_sf()

FAO_centroids = st_centroid(major_fao_shp) %>% 
  st_coordinates

major_fao_shp$long <- FAO_centroids[,1]
major_fao_shp$lat <- FAO_centroids[,2]

major_fao_shp <- major_fao_shp %>% 
  mutate(long = case_when(
    F_CODE == 81 ~ long - 90,
    F_CODE == 88 ~ long - 40,
    F_CODE == 71 ~ long + 35,
    F_CODE == 61 ~ long + 40,
    F_CODE == 18 ~ long + 170,
    TRUE ~ long
  ))

fao_map <- ggplot(major_fao_shp)+
  geom_sf( fill = "lightblue", color = "gray")+
  light_gfw_theme()  +
  geom_label(aes(long, lat, label = F_CODE), alpha = 0.75, size = 2)
  

figure5 <- ggdraw()+
  draw_plot(fao_map,
            x = 0, y = .70, width = 1, height = .31)+
  draw_plot(profits_by_country_and_FAO_tileplot+
              theme_multi, 
            x = 0.013 , y = .3, width = 1, height = .39)+
  draw_plot(profits_by_gear_and_FAO_tileplot+
              theme_multi, 
            x = 0, y = 0, width = 1, height = .31)+
  draw_plot_label(label = c("A.", "B.", "C. "), 
                  size = 9,
                  x = c(0,0,0),
                  y = c(1,.7,.33),
                  hjust = 0)
