require(tidyverse)
palette <- list(light = grDevices::rgb(126/256, 185/256, 212/256),
                dark = grDevices::rgb(129/256,0,82/256))

#############################
# Visualize
#############################

# get_sites <- function(sites_path) {
#   # Prepare sites for merge
#   # sites_path <- "~/Downloads/MuSC - Sites (16.04.2020).xlsx"
#   
#   sites_ita <- readxl::read_excel(sites_path, sheet = 1) %>% 
#     mutate(Country = "Italy",
#            Site = str_pad(Site, 2, pad = "0"))
#   
#   sites_int <- readxl::read_excel(sites_path, sheet = 2) %>% 
#     mutate(Site = str_pad(as.character(`Site number`), 2, pad = "0"),
#            PROVIDER_ID = as.character(PROVIDER_ID))
#   
#   bind_rows(sites_ita, sites_int)
# }

get_sites <- function(sites_source = "https://www.dropbox.com/s/l53kagy99vm30hk/MuSC%20-%20Sites.xlsx?raw=1") {
  
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
plot_map <- function(geocoding_path) {
  
  # tool
  get_geo <- function(geocoding_path) {
    sites <- get_sites()
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
  sites <- get_geo(geocoding_path)
  
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

time_enroll <- function(f) {
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
  
  time_enr_plot <- ggplot(real_pts %>%
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
  return(time_enr_plot)
  
  # single_center <- real_pts[real_pts$Base_Provider == "Cinzia Cordioli",]
  # date_range <- min(single_center$visit_date):max(single_center$visit_date)
  # 
  # ggplot(single_center %>%
  #          group_by(visit_date) %>% arrange(visit_date) %>% 
  #          summarise(n = n()) %>% 
  #          mutate(cums = cumsum(n)),
  #        aes(visit_date, cums)) +
  #   geom_line() +
  #   geom_point(alpha = 0.5) +
  #   theme_light() +
  #   scale_x_date(breaks = "week", date_labels = "%d %b") +
  #   labs(x = "\nEnrollment date", y = "N. of patients\n") +
  #   coord_fixed(ratio = 0.35) +
  #   ggtitle(paste("Site:",
  #                 first(single_center$Demography_Country), "-",
  #                 first(single_center$`Demography_Site Code`)))
}
# ggsave("~/Downloads/enrollment_sc.pdf")

#############################
# centers
#############################

# f<-clean(convert("~/Downloads/report (27).csv"))
rec_plot <- function(f, separated = FALSE) {
  real_pts <- f[!(f$Base_Provider %in% MPS_format$provider_blacklist),]
  
  real_pts$visit_date <- lubridate::dmy(real_pts$`Demography_Date of Visit`)
  
  # real_pts$label = str_remove(real_pts$upid, "-\\d*?$")
  recl_db <- left_join(real_pts, sites)
  recl_db$label = paste(recl_db$`Demography_Site Code`, 
                         ifelse(trimws(recl_db$City) == "" | is.na(recl_db$City),
                                recl_db$Demography_Country,
                                recl_db$City), 
                         sep = " - ")
  
  recl_db[recl_db$label == "01 - NA",]$upid
  
  recl_db <- recl_db %>% 
    group_by(label) %>% 
    mutate(n = n()) %>% 
    group_by(Demography_Country) %>% 
    mutate(contrib = n() %>% max()) %>% 
    ungroup() %>% 
    arrange(contrib, desc(n)) %>% 
    mutate(ord = row_number())
  
  unique <- recl_db %>% 
    # count(label, ord, n, Demography_Country) %>% 
    ggplot(aes(fct_reorder(label, ord), fill = Demography_Country)) +
    geom_bar() +
    geom_text(aes(label = n, y = n + 2), size = 2,
              col = 1, alpha = 0.8, check_overlap = TRUE) +
    theme_light() +
    scale_y_continuous(breaks = seq(0, 1000, 10)) +
    labs(x = "", y = "Count\n", fill = "Country") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 8),
          panel.grid.major.x = element_blank())
  
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
          legend.position = c(.908, .91))
  
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
  gathered <- ita_recl +
    annotation_custom(
      grob = ggplotGrob(row_recl + theme(plot.background = element_blank())),
      xmin = 12,
      xmax = 34.8,
      ymin = 20,
      ymax = 95)
  # dev.off()
  
  if (separated) {return(gathered)} else {return(unique)}
}

# require(ggwordcloud)
# count(recl_db, Demography_Country, label) %>% 
#   # mutate(angle = 45 * sample(-2:2, n(), replace = TRUE, prob = c(1, 1, 4, 1, 1)))  %>% 
#   ggplot(aes(label = label, size = sqrt(n), col = Demography_Country)) +
#   geom_text_wordcloud(eccentricity = 1, area_corr_power = 1) +
#   scale_radius(range = c(0, 7), limits = c(0, NA)) +
#   theme_minimal()
# ggsave("~/Downloads/wordcloud.pdf")

drug_or_plot <- function() {
  real_pts <- f[!(f$Base_Provider %in% MPS_format$provider_blacklist),]
  
  pharm_source <- "https://www.dropbox.com/s/0bf8cxekq0fls4p/Dati%20Farmaci%202019%20IQVIA.xlsx?dl=0&raw=1"
  
  httr::GET(pharm_source, httr::write_disk(last_pharm <- "~/Downloads/temp.xlsx", overwrite = TRUE))
  
  pharm_head <- readxl::read_excel(last_pharm, sheet = 2, guess_max = 16)[0,]
  pharm_ita <- readxl::read_excel(last_pharm, sheet = 2, skip = 7, 
                                  col_names = FALSE, n_max = 9)
  names(pharm_ita) <- c("tp", names(pharm_head)[-1])
  rm(pharm_head)
  
  pharm_ita$w.prop <- pharm_ita$`STIMA PESATA`/100
  
  
  # oss tot = 457;	pos = 133
  	
  pharm_ita$tot.oss <- 457
  pharm_ita$tot.pos <- 133
  
  pharm_ita$oss.n <- pharm_ita$`oss tot` * pharm_ita$tot.oss
  pharm_ita$pos.n <- pharm_ita$pos * pharm_ita$tot.pos
  
  pharm_ita$n.oss.weighted <- round(pharm_ita$w.prop * pharm_ita$tot.oss, 0)
  pharm_ita$n.pos.weighted <- round(pharm_ita$w.prop * pharm_ita$tot.pos, 0)

  pharm_ita_mini <- select(pharm_ita, tp, tot.oss:pos.n, w.prop, uw.prop = `Consumo Italia 2020`)
  
  pharm_ita_vert <- pharm_ita_mini %>% 
    unite("tot", tot.oss, tot.pos) %>% 
    unite("exp", n.oss.weighted, n.pos.weighted) %>% 
    unite("n", oss.n, pos.n) %>% 
    gather("par", "val", -tp, -w.prop, -uw.prop) %>% 
    separate("val", c("Suspected", "Positives"), convert = TRUE) %>% 
    gather("inclusion", "val", Suspected, Positives) %>% 
    mutate(inclusion = factor(inclusion, 
                              labels = c("Suspected", "Positives"), 
                              levels = c("Suspected", "Positives"))) %>% 
    spread(par, val) %>% 
    rename(exp_p = w.prop) %>% 
    mutate(exp_pu = uw.prop/100) %>% 
    # mutate(exp_p = exp/tot) %>% 
    # gather("group", "n", exp, n) %>% 
    
    # or
    rowwise() %>% 
    mutate(or_ = list(epitools::oddsratio(matrix(c(n, tot, exp_p*50000, 50000), ncol = 2)))) %>% 
    mutate(est = or_$measure[2,1],
           lwr = or_$measure[2,2],
           upr = or_$measure[2,3]) %>% 
    ungroup()
    
    # Prop test
    # rowwise() %>% 
    # mutate(prop_ = list(prop.test(n, tot))) %>% 
    # mutate(est = prop_$estimate,
    #        lwr = prop_$conf.int[1],
    #        upr = prop_$conf.int[2]) %>% 
    # ungroup()
  
  pharm_ita_vert <- pharm_ita_vert %>% 
    mutate(readable = sprintf("%.1f (%.1f-%.1f)", est, lwr, upr))
  

  pharm_ita_vert <- pharm_ita_vert %>% 
    filter(inclusion == "Positives") %>% 
    arrange(est) %>% 
    rowid_to_column("id") %>% 
    select(tp, id) %>% 
    right_join(pharm_ita_vert)
  
  pharm_ita_vert <- 
    mutate(pharm_ita_vert, 
           incl_label = ifelse(inclusion == "Positives", 
                              "OR (95%CI)        Positives                    ",
                              "OR (95%CI)        Suspected                    "))
  

  ggplot(pharm_ita_vert, aes(est, fct_reorder(tp, id), col = id)) +
    geom_vline(xintercept = 1, col = "darkgray", lty = 2) +
    geom_point(position = position_dodge(width = 0.5)) +
    geom_text(aes(label = readable), x = -2, col = 1, size = 2.8, hjust = 0) +
    geom_linerange(aes(xmin = lwr, xmax = upr)) +
    facet_grid(~ incl_label, scales = "free_y") +
    theme_light() +
    # scale_color_manual(values = sort(colorRampPalette(c("black", "white"))(9))) +
    # scale_colour_gradient2(low = palette$light, high = palette$dark,
    #                        mid = colorRampPalette(c(palette$light, palette$dark))(3)[2],
    #                        midpoint = 0) +
    scale_colour_gradient(low = palette$light, high = palette$dark) +
    geom_hline(yintercept = seq(0.5, 15, 1), col = "lightgray", size = 0.1) +
    scale_x_continuous(breaks = seq(1, 4, 1), limits = c(-2, 4)) +
    theme(strip.text.y = element_blank(),
          panel.grid.major.y = element_blank(),
          strip.background.x = element_rect(fill = "white"),
          strip.text.x = element_text(colour = "black", face = "bold")) +
    guides(col = FALSE) +
    labs(x = "\nOdds Ratio", y = "")#Treatment\n")
  
  # table_or <- pharm_ita_vert %>% 
  #   mutate(readable = sprintf("%.1f (%.1f-%.1f)", est, lwr, upr)) %>% 
  #   select(`\nTreatment\n` = tp, inclusion, readable, id) %>% 
  #   spread(inclusion, readable) %>% 
  #   rename(`Suspected\nOR (95%CI)` = Suspected) %>% 
  #   rename(`Positives\nOR (95%CI)` = Positives) %>% 
  #   arrange(id) %>% 
  #   select(-id) %>% 
  #   ggpubr::ggtexttable(theme = ggpubr::ttheme("minimal", rownames.style = rownames_style(size = 25)), 
  #                       rows = NULL)
    
  # ggplot(pharm_ita_vert, aes(est, fct_reorder(inclusion, est), col = tp)) +
  #   geom_vline(aes(xintercept = 1), lty = 2, col = "darkgray") +
  #   geom_point(position = position_dodge(width = 0.5), aes(shape = inclusion), size = 2) +
  #   geom_linerange(aes(xmin = lwr, xmax = upr, lty = inclusion)) +
  #   facet_wrap(~ tp) +
  #   theme_light() +
  #   scale_shape_manual(values = c(19, 5)) +
  #   theme(strip.background = element_rect(fill = "white"),
  #         strip.text = element_text(colour = "black", face = "bold")) +
  #   guides(col = FALSE) +
  #   scale_y_discrete(label = c()) +
  #   labs(x = "\nOdds Ratio", y = "")
  
  
  # ggplot(pharm_ita_vert, aes(est, tp, col = tp)) +
  #   geom_point(position = position_dodge(width = 0.5)) +
  #   geom_vline(aes(xintercept = exp_p, col = tp), lty = 2) +
  #   geom_linerange(aes(xmin = lwr, xmax = upr)) +
  #   facet_grid(tp ~ inclusion, scales = "free_y") +
  #   theme_light() +
  #   theme(strip.text.y = element_blank(),
  #         strip.background.x = element_rect(fill = "white"),
  #         strip.text.x = element_text(colour = "black", face = "bold")) +
  #   guides(col = FALSE) +
  #   labs(x = "\nObserved proportion", y = "Treatment\n")

  # ggplot(pharm_ita_vert, aes(est/exp_p, tp, col = tp)) +
  #   geom_point(position = position_dodge(width = 0.5)) +
  #   geom_vline(aes(xintercept = exp_p/exp_p, col = tp), lty = 2) +
  #   geom_linerange(aes(xmin = lwr/exp_p, xmax = upr/exp_p)) +
  #   facet_grid(tp ~ inclusion, scales = "free_y") +
  #   theme_light() +
  #   theme(strip.text.y = element_blank(),
  #         strip.background.x = element_rect(fill = "white"),
  #         strip.text.x = element_text(colour = "black", face = "bold")) +
  #   guides(col = FALSE) +
  #   labs(x = "\nObserved/Expected", y = "Treatment\n")
  
  # ggplot(pharm_ita_vert, aes(est, as.numeric(tp), col = tp)) +
  #   geom_point(position = position_dodge(width = 0.5)) +
  #   # geom_vline(aes(xintercept = exp_p, col = tp), lty = 2) +
  #   geom_point(aes(x = exp_p, y = abs(as.numeric(tp)-0.4), col = tp), shape = 18, size = 2) +
  #   geom_point(aes(x = est, y = abs(as.numeric(tp)-0.6)), col = "transparent") +
  #   geom_point(aes(x = est, y = abs(as.numeric(tp)+0.3)), col = "transparent") +
  #   geom_linerange(aes(xmin = lwr, xmax = upr)) +
  #   # facet_grid(tp ~ inclusion, scales = "free_y") +
  #   theme_light() +
  #   scale_y_continuous(breaks = seq(1, length(pharm_ita_vert$tp)/2, 1), labels = levels(pharm_ita_vert$tp)) +
  #   facet_wrap(~ inclusion) +
  #   theme(strip.text.y = element_blank(),
  #         strip.background.x = element_rect(fill = "white"),
  #         strip.text.x = element_text(colour = "black", face = "bold")) +
  #   guides(col = FALSE) +
  #   labs(x = "\nObserved proportion", y = "Treatment\n")
  # 
  # ggsave("~/Downloads/pharm_or.pdf", width = 6.5, height = 4)
  
}





