require(tidyverse)
palette <- list(light = grDevices::rgb(126/256, 185/256, 212/256),
                dark = grDevices::rgb(129/256,0,82/256))

#############################
# Visualize
#############################

get_sites <- function(sites_path) {
  # Prepare sites for merge
  # sites_path <- "~/Downloads/MuSC - Sites (16.04.2020).xlsx"
  
  sites_ita <- readxl::read_excel(sites_path, sheet = 1) %>% 
    mutate(Country = "Italy",
           Site = str_pad(Site, 2, pad = "0"))
  
  sites_int <- readxl::read_excel(sites_path, sheet = 2) %>% 
    mutate(Site = str_pad(as.character(`Site number`), 2, pad = "0"),
           PROVIDER_ID = as.character(PROVIDER_ID))
  
  bind_rows(sites_ita, sites_int)
}

# manual source update
plot_recruitment <- function(sites_path, cleaned_imported_file) {
  
  sites <- get_sites(sites_path) %>% 
    mutate(Site = str_pad(Site, 2, pad = "0")) %>% 
    mutate(ifelse(is.na(PATIENTS), 0, PATIENTS))
  
  # Prepare data for merge
  # imported_file_path <- "~/Downloads/report (4).csv"
  cleaned <- cleaned_imported_file #clean(convert(imported_file_path))
  cleaned <- cleaned %>% 
    mutate(`Demography_Site Code` = str_pad(`Demography_Site Code`, 2, pad = "0"))
  
  # Merge
  plot_db <- full_join(sites, cleaned, by = c("Country" = "Demography_Country", 
                                              "Site" = "Demography_Site Code")) %>% 
    filter(Country == "Italy") %>% 
    mutate(`Patient Code` = ifelse(is.na(`Demography_Patient Code`),
                                   "00", `Demography_Patient Code`)) %>% 
    mutate_at(vars(`Patient Code`, `Site number`), as.numeric) %>% 
    mutate(fake_x = ifelse(!is.na(`Patient Code`),
                           `Patient Code` - PATIENTS,
                           0)) %>% 
    arrange(`Site number`) %>% 
    mutate(fake_y = sprintf("%s-%s %s", Country, Site, `Reference person`)) %>% 
    mutate(fake_y_position = as.numeric(as.factor(fake_y)))
  
  # Set up coord
  labels_y <- plot_db %>% 
    filter(!is.na(`Reference person`)) %>% 
    count(`Reference person`, Country, Site) %>% 
    arrange(Country, Site) %>% 
    {sprintf("%s %s-%s", .$`Reference person`, .$Country, .$Site)}
  
  # Plot
  ggplot(filter(plot_db, !is.na(`Reference person`)), 
         aes(fake_x, fake_y_position, label = `Patient Code`)) +
    geom_rect(aes(xmin = -PATIENTS+0.5, xmax = 0.5,
                  ymin = fake_y_position - 0.5, 
                  ymax = fake_y_position + 0.5),
              fill = "#E26D5C") +
    geom_tile(col = "#417B5A", fill = "#A1CCA5") +
    # geom_tile(aes(x = as.numeric(PATIENTS)), fill = 2) +
    geom_text(size = 3, col = "#417B5A") +
    geom_vline(xintercept = 0.5) +
    scale_y_continuous("", breaks = (1:length(labels_y)), labels = labels_y) +
    scale_x_continuous("Patients from launch", breaks = -58:40) +
    theme_light() +
    theme(panel.grid.major = element_blank(),
          axis.text.x = element_text(angle = 45))
  # facet_wrap(~ Country, scales = "free")
  
  # ggsave("~/Downloads/patients.pdf", width = 18, height = 12)
}


#############################
# map
#############################
plot_map <- function(site_path, geocoding_path) {
  
  # tool
  get_geo <- function(site_path, geocoding_path) {
    sites <- get_sites(sites_path)
    geocoding <- readxl::read_excel(geocoding_path)
    
    left_join(sites, geocoding)
  }
  
  center_coord <- function(coord, margin = 5) {
    xs <- range(coord, na.rm = TRUE)
    xs <- xs + sign(xs)*margin
    return(xs)
  }
  
  #
  # sites <- get_geo(site_path = "~/Downloads/MuSC - Sites (14.04.2020).xlsx", 
  #                geocoding_path = "~/OneDrive/Documenti/SM/musc19/export_scripts/manual_geocoding.xlsx")
  sites <- get_geo(site_path, geocoding_path)
  
  require(sf)
  require("rnaturalearth")
  require("rnaturalearthdata")
  require(ggrepel)
  
  world <- ne_countries(scale = "medium", returnclass = "sf")
  
  palette <- list(light = grDevices::rgb(126/256, 185/256, 212/256),
                  dark = grDevices::rgb(129/256,0,82/256))
  
  ggplot_map <- ggplot(data = world) + 
    geom_sf(fill= "white", col = "lightgray", size = 0.1) + 
    coord_sf(xlim = center_coord(sites$lat),
             ylim = center_coord(sites$lon),
             # xlim = c(5, 20),
             # ylim = c(36,48)
             expand = FALSE) +
    theme_light() +
    geom_point(data = sites, alpha = 0.5,
               col = palette$dark,
               aes(x = lat, y = lon, text = short_name)) +
    labs(x = "", y = "") +
    theme(panel.background = element_rect(fill = palette$light),
          panel.grid.major = element_blank()) +
    ggtitle("")
  
  # ggsave(ggplot_map, "~/Downloads/map.pdf")
  
  ggplotly_map <- plotly::ggplotly(ggplot_map, tooltip = "text")
  # htmlwidgets::saveWidget(plotly::as_widget(ggplotly_map), "~/Downloads/map.html")
  
  return(ggplotly_map)
}

# # static
# sites$int_names <- ifelse(sites$Country == "Italy", NA, sites$short_name)
# sites$ita_names <- ifelse(sites$Country == "Italy", sites$short_name, NA)
# 
# ggplot_map +
#   geom_point(data = filter(sites, sites$Country == "Italy"), alpha = 0.5, 
#              col = palette$dark,
#              aes(x = lat, y = lon, text = short_name))
#   # geom_text_repel(data = sites, alpha = 0.8, 
#   #            col = palette$dark, size = 0.9,
#   #            aes(x = lat, y = lon, label = ita_names))
# ggsave("~/Downloads/ita_sites.pdf")

#############################
# enrollment
#############################

# f<-clean(convert("~/Downloads/report (26).csv"))

real_pts <- f[!(f$Base_Provider %in% MPS_format$provider_blacklist),]

real_pts$visit_date <- lubridate::dmy(real_pts$`Demography_Date of Visit`)

# ggplot(real_pts %>%
#          mutate(site = paste(Demography_Country, "-", `Demography_Site Code`)) %>%
#          group_by(site) %>% mutate(count = n()) %>% ungroup() %>%
#          filter(count > 2),
#        aes(visit_date, fct_reorder(`Demography_Patient Code`, visit_date))) +
#   geom_point() +
#   # facet_wrap(~ site) +
#   scale_x_date(breaks = "week") +
#   theme(axis.text.y = element_blank())

date_range <- min(real_pts$visit_date):max(real_pts$visit_date)

ggplot(real_pts %>%
         mutate(site = paste(Demography_Country, "-", `Demography_Site Code`)) %>%
         group_by(site) %>% mutate(count = n()) %>% ungroup() %>%
         filter(count > 2) %>% group_by(visit_date) %>% arrange(visit_date) %>% 
         summarise(n = n(), site = first(site), Demography_Country = first(Demography_Country)) %>% 
         mutate(cums = cumsum(n)),
       aes(visit_date, cums)) +
  geom_line() +
  theme_light() +
  scale_x_date(breaks = "week") +
  labs(y = "N. of patients\n") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# ggsave("~/Downloads/enrollment.pdf")

single_center <- real_pts[real_pts$Base_Provider == "Cinzia Cordioli",]
date_range <- min(single_center$visit_date):max(single_center$visit_date)

ggplot(single_center %>%
         group_by(visit_date) %>% arrange(visit_date) %>% 
         summarise(n = n()) %>% 
         mutate(cums = cumsum(n)),
       aes(visit_date, cums)) +
  geom_line() +
  geom_point(alpha = 0.5) +
  theme_light() +
  scale_x_date(breaks = "week", date_labels = "%d %b") +
  labs(x = "\nEnrollment date", y = "N. of patients\n") +
  coord_fixed(ratio = 0.35) +
  ggtitle(paste("Site:",
                first(single_center$Demography_Country), "-",
                first(single_center$`Demography_Site Code`)))

# ggsave("~/Downloads/enrollment_sc.pdf")

#############################
# centers
#############################

# f<-clean(convert("~/Downloads/report (27).csv"))

real_pts <- f[!(f$Base_Provider %in% MPS_format$provider_blacklist),]

real_pts$visit_date <- lubridate::dmy(real_pts$`Demography_Date of Visit`)

sites_source <- "https://www.dropbox.com/s/l53kagy99vm30hk/MuSC%20-%20Sites.xlsx?raw=1"

httr::GET(sites_source, httr::write_disk(last_sites <- "~/Downloads/temp.xlsx", overwrite = TRUE))

sites_ita <- readxl::read_excel(last_sites, sheet = 1) %>% 
  mutate(Country = "Italy")

sites_int <- readxl::read_excel(last_sites, sheet = 2) %>% 
  mutate(PROVIDER_ID = as.character(PROVIDER_ID),
         Site = `Site number`) %>% 
  filter(is.na(`Date of block`))

sites <- bind_rows(sites_ita, sites_int) %>% 
  mutate(`Demography_Site Code` = str_pad(Site, 2, pad = "0"),
         Demography_Country = Country)

# real_pts$label = str_remove(real_pts$upid, "-\\d*?$")
recl_db <- left_join(real_pts, sites)
recl_db$label = paste(recl_db$`Demography_Site Code`, 
                       recl_db$City, 
                       sep = " - ")

recl_db[recl_db$label == "01 - NA",]$upid

recl_db <- recl_db %>% 
  group_by(label) %>% 
  mutate(n = n()) %>% 
  group_by(Demography_Country) %>% 
  arrange(desc(n)) %>% 
  mutate(ord = row_number()) %>% 
  ungroup()

ita_recl <- recl_db %>% 
  # count(label, ord, n, Demography_Country) %>% 
  filter(n > 3 & Demography_Country == "Italy") %>%
  ggplot(aes(fct_reorder(label, ord))) +
  geom_bar(aes(fill = "Italy")) +
  geom_text(aes(label = n, y = n + 2), size = 5,
            col = 1, alpha = 0.8, check_overlap = TRUE) +
  theme_light() +
  scale_fill_manual(values = palette$light) +
  scale_y_continuous(breaks = seq(0, 1000, 10)) +
  labs(x = "", y = "Count\n", fill = "Country") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 15),
        panel.grid = element_blank(),
        legend.position = c(.908, .92))

row_recl <- recl_db %>% 
  # count(label, ord, n, Demography_Country) %>% 
  filter(Demography_Country != "Italy") %>%
  ggplot(aes(fct_reorder(label, ord), alpha = Demography_Country )) +
  geom_bar(fill = palette$dark) +
  geom_text(aes(label = n, y = n + 0.5), size = 5,
            col = 1, alpha = 0.8, check_overlap = TRUE) +
  theme_light() +
  # scale_fill_manual(values = c())
  scale_y_continuous(breaks = seq(0, 100, 1)) +
  labs(x = "", y = "", alpha = "") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 15),
        panel.grid.major.x = element_blank())

# Plot insets
#' A special case of layouts is where one of the plots is to 
#' be placed within another, typically as an inset of the plot 
#' panel. In this case, grid.arrange() cannot help, as it only 
#' provides rectangular layouts with non-overlapping cells. 
#' Instead, a simple solution is to convert the plot into a grob,
#'  and place it using annotation_custom() within the plot panel. 
#'  Note the related geom_custom() function, suitable when different 
#'  facets should display different annotations.

require(grid)

# cairo_pdf("~/Downloads/centers.pdf", width = 16, height = 10)
ita_recl +
  annotation_custom(
    grob = ggplotGrob(row_recl + theme(plot.background = element_blank())),
    xmin = 7,
    xmax = 27,
    ymin = 10,
    ymax = 85)
# dev.off()

require(ggwordcloud)
count(recl_db, Demography_Country, label) %>% 
  # mutate(angle = 45 * sample(-2:2, n(), replace = TRUE, prob = c(1, 1, 4, 1, 1)))  %>% 
  ggplot(aes(label = label, size = sqrt(n), col = Demography_Country)) +
  geom_text_wordcloud(eccentricity = 1, area_corr_power = 1) +
  scale_radius(range = c(0, 7), limits = c(0, NA)) +
  theme_minimal()
ggsave("~/Downloads/wordcloud.pdf")

count(recl_db, Demography_Country, sort = TRUE)

count(recl_db, Demography_Country,`Demography_Site Code`, City, sort = TRUE) %>%  View()

sites$Country
