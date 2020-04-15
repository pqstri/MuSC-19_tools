require(tidyverse)

#############################
# Visualize
#############################

get_sites <- function(sites_path) {
  # Prepare sites for merge
  # sites_path <- "~/Downloads/MuSC - Sites (10.04.2020).xlsx"
  
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