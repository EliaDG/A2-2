
# DEPENDENCIES -----------------------------------------------------------------
source("./02_Code/__packages.R")

# LOADING DATA -----------------------------------------------------------------
literacy <- read_dta("./01_Data-input/literacy_Arg-Bra-Par.dta", encoding = "latin1")

# National
shp_arg_0 <- st_read("./01_Data-input/ARG/gadm41_ARG_0.shp")
shp_bra_0 <- st_read("./01_Data-input/BRA/gadm41_BRA_0.shp")
shp_pry_0 <- st_read("./01_Data-input/PRY/gadm41_PRY_0.shp")
shp_ury_0 <- st_read("./01_Data-input/URY/gadm41_URY_0.shp")

# Regional
shp_arg_1 <- st_read("./01_Data-input/ARG/gadm41_ARG_1.shp")
shp_bra_1 <- st_read("./01_Data-input/BRA/gadm41_BRA_1.shp")
shp_pry_1 <- st_read("./01_Data-input/PRY/gadm41_PRY_1.shp")

#Provincial
shp_arg_2 <- st_read("./01_Data-input/ARG/gadm41_ARG_2.shp")
shp_bra_2 <- st_read("./01_Data-input/BRA/Brazil_province.shp")
shp_pry_2 <- st_read("./01_Data-input/PRY/gadm41_PRY_2.shp")

# PREPPING ----------------------------------------------------------------------
transform_NA <- function(df, except_col) {
  # Iterate through columns
  for (col in names(df)) {
    # Skip the 'geometry' column
    if (col != except_col) {
      df[[col]][df[[col]] == "NA"] <- NA
    }
  }
  return(df)
}

Country <- rbind(shp_arg_0, shp_bra_0, shp_pry_0, shp_ury_0) %>% 
  transform_NA(., "geometry")

Region <- rbind(shp_arg_1, shp_bra_1, shp_pry_1) %>% 
  transform_NA(., "geometry") %>% 
  mutate(NAME_1 = if_else(NAME_1 == "Misiones" & GID_0 == "PRY", "Misiones (PRY)", NAME_1))

Municipality <- rbind(shp_arg_2, shp_bra_2, shp_pry_2) %>% 
  transform_NA(., "geometry") %>% 
  mutate(NAME_1 = if_else(NAME_1 == "Misiones" & GID_0 == "PRY", "Misiones (PRY)", NAME_1))

# FIXING
if (any(!st_is_valid(Country))) {
  Country <- st_make_valid(Country)
}

if (any(!st_is_valid(Region))) {
  Region <- st_make_valid(Region)
}

if (any(!st_is_valid(Municipality))) {
  Municipality <- st_make_valid(Municipality)
}

# CENTROIDS FOR LABELS
centroid <- st_centroid(st_as_sf(Country, wkt = "geometry"))
Country <- cbind(Country, st_coordinates(centroid))

centroid <- st_centroid(st_as_sf(Region, wkt = "geometry"))
Region <- cbind(Region, st_coordinates(centroid))

# DATASET ----------------------------------------------------------------------
ds <- literacy %>% 
  mutate(state = ifelse(state == "Misiones", "Misiones (PRY)", state),
         state = ifelse(state == "Misiones1", "Misiones", state),
         muni = case_when(
           muni == "Cambyretá" ~ "Cambyreta",
           muni == "Leandro Oviedo" ~ "José Leandro Oviedo",
           muni == "Mayor Otaño" ~ "Mayor Julio D. Otaño",
           muni == "San Juan Bautista" ~ "San Juan Bautista de las Misione",
           muni == "25 de Mayo" ~ "Veinticinco de Mayo",
           muni == "Restinga Seca" ~ "Restinga Sêca",
           muni == "Sant' Ana do Livramento" ~ "Sant'Ana do Livramento",
           muni == "Vespasiano Correa" ~ "Vespasiano Corrêa",  
           muni == "Westfalia" ~ "Westfália",
           TRUE ~ muni),
         country = ifelse(country == "BRA","Brazil", country),
         state = ifelse(state == "RS","Rio Grande do Sul", state),
         mesorregi = as.factor(mesorregi)) %>% 
  rename(COUNTRY = country,
         NAME_1 = state,
         NAME_2 = muni,
         mis_pry = mis,
         mis = mis1)

Jesuit <- c("Misiones", "Misiones (PRY)", "Corrientes", "Rio Grande do Sul", "Itapúa")
df <- merge(Municipality[Municipality$NAME_1 %in% Jesuit, ], ds, by = c("COUNTRY", "NAME_1", "NAME_2"), y.all = TRUE)

# Cleaning environment
rm(list = setdiff(ls(), c("df", "ds", "Country", "Region", "Municipality", "Jesuit")))

# VISUALIZATION ----------------------------------------------------------------
## B.1 -----------------------------------------------------------------------
palette <- c("lightblue", "lightgreen", "yellow", "lightgray")

# plot_00 <- ggplot() +
#   geom_sf(data = Country, aes(geometry = geometry, fill = COUNTRY), color = "darkgray") +
#   geom_sf(data = Region[Region$NAME_1 %in% Jesuit, ], aes(geometry = geometry), color = "black", size = 1, fill = NA) +
#   geom_text(data = Country, 
#             aes(x = X, y = Y, label = COUNTRY), 
#             size = 3, color = "black", fontface = "bold") +
#   scale_fill_manual(values = palette) +
#   theme_light() +
#   labs(x = NULL, y = NULL,
#        title = "Location of the Guaraní Jesuit Missions",
#        subtitle = "South America Map") +
#   theme(plot.title = element_text(size = 12, face = "bold"),
#         plot.subtitle = element_text(size = 11, face = "italic")) +
#   guides(fill = "none") +
#   annotate("rect", xmin = -60, xmax = -49.5, ymin = -34, ymax = -25,
#            fill = NA, color = "black", alpha = 0.8)

plot_01 <- ggplot() +
  geom_sf(data = Country, aes(geometry = geometry, fill = COUNTRY)) +
  geom_sf(data = Municipality[Municipality$NAME_1 %in% Jesuit, ], aes(geometry = geometry), color = "#737373", fill = NA) +
  geom_sf(data = Region, aes(geometry = geometry, size = 1.2), color = "black", fill = NA) +
  scale_fill_manual(values = palette, name = "Country:") +
  theme_light() +
  scale_size_identity() +
  labs(x = NULL, y = NULL,
       title = "Location of the Guaraní Jesuit Missions",
       subtitle = "Municipality Level") +
  theme(plot.title = element_text(size = 12, face = "bold"),
        plot.subtitle = element_text(size = 11, face = "italic"),
        legend.position = c(0.95, 0.05),
        legend.justification = c("right", "bottom"),
        legend.box.just = "right",
        legend.box.background = element_rect(color = "black", linetype = "solid")) +
  geom_label(data = Region[Region$NAME_1 %in% Jesuit, ], aes(x = X, y = Y, label = NAME_1),
                   size = 4, color = "black",fill = "white", fontface = "bold",
                   label.padding = unit(0.5, "lines")) +
  guides(fill = "legend") +
  coord_sf(xlim = c(-59.5, -49.7), ylim = c(-33.5, -25.75))

df$illiteracy_bins <- cut(df$illiteracy, breaks = 5)
custom_palette <- c("#4dac26", "#b8e186", "#ffffbf", "#f1b6da", "#d01c8b")

plot_02 <- ggplot() +
  geom_sf(data = df, aes(geometry = geometry, fill = illiteracy_bins), color = "gray") +
  geom_sf(data = Region, aes(geometry = geometry), color = "#737373", fill = NA) +
  geom_sf(data = Country, aes(geometry = geometry), color = "black", fill = NA) +
  scale_fill_manual(values = custom_palette, name = "Illiteracy Level:") +
  theme_light() +
  scale_size_identity() +
  labs(x = NULL, y = NULL,
       title = "Illiteracy at Municipal Level",
       subtitle = "Former Location of the Guaraní Jesuit Missions") +
  theme(plot.title = element_text(size = 12, face = "bold"),
        plot.subtitle = element_text(size = 11, face = "italic"),
        legend.position = "inside",
        legend.position.inside = c(0.15, 0.20),
        legend.box.just = "left",
        legend.box.background = element_rect(color = "black", linetype = "solid")) +
  geom_label(data = Region[Region$NAME_1 %in% Jesuit, ], aes(x = X, y = Y, label = NAME_1),
             size = 4, color = "black",fill = "white", fontface = "bold",
             label.padding = unit(0.5, "lines")) +
  guides(fill = "legend") +
  coord_sf(xlim = c(-59.5, -49.7), ylim = c(-33.5, -25.75))


df_filtered <- df[!is.na(df$preci), ]
breaks_seq <- seq(min(df_filtered$preci), max(df_filtered$preci), length.out = 10)
labels_seq <- sprintf("%.1f to %.1f", breaks_seq, c(breaks_seq[-1], max(df_filtered$preci)))

plot_03 <- ggplot() +
  geom_sf(data = df, aes(geometry = geometry, fill = preci), color = "gray") +
  geom_sf(data = Region, aes(geometry = geometry), color = "#737373", fill = NA) +
  geom_sf(data = Country, aes(geometry = geometry), color = "black", fill = NA) +
  scale_fill_viridis(option = "plasma",
                     direction = -1, 
                     name = "Precipitation Level:",
                     breaks = breaks_seq,
                     labels = labels_seq) +
  theme_light() +
  scale_size_identity() +
  labs(x = NULL, y = NULL,
       title = "Precipitation at Municipal Level",
       subtitle = "Former Location of the Guaraní Jesuit Missions") +
  theme(plot.title = element_text(size = 12, face = "bold"),
        plot.subtitle = element_text(size = 11, face = "italic"),
        legend.position = "inside",
        legend.position.inside = c(0.15, 0.20),
        legend.box.just = "left",
        legend.box.background = element_rect(color = "black", linetype = "solid"),
        legend.text = element_text(size = 11)) +
  geom_label(data = Region[Region$NAME_1 %in% Jesuit, ], aes(x = X, y = Y, label = NAME_1),
             size = 4, color = "black",fill = "white", fontface = "bold",
             label.padding = unit(0.5, "lines")) +
  guides(fill = "legend") +
  coord_sf(xlim = c(-59.5, -49.7), ylim = c(-33.5, -25.75))

plot_04 <- tm_shape(df) + 
  tm_fill("popd", title = "Population Density:", palette = "viridis", style = "log10_pretty", n = 5) + 
  tm_borders(col = "gray", lwd = 0.5) + 
  tm_shape(Region) + 
  tm_borders(col = "black", lwd = 1) +
  tm_shape(Region[Region$NAME_1 %in% Jesuit, ]) +
  tm_text("NAME_1", size = 0.9, bg.color = "white") +
  tm_shape(Country) + 
  tm_borders(col = "black", lwd = 1.5) +
  tm_layout(panel.show = TRUE,
            panel.labels = "Population Density at Municipal Level",
            panel.label.fontface = "bold",
            legend.bg.color = "white",
            legend.position = c("left", "bottom"),
            legend.frame = TRUE,
            legend.text.size = 1)

#REPLICATION -------------------------------------------------------------------
model_1 <- lm(illiteracy ~ distmiss + lati + longi + corr + ita + mis_pry + mis, data = df)
model_2 <- lm(illiteracy ~ distmiss + lati + longi + area + tempe + alti + preci + rugg + river + coast + corr + ita + mis_pry+ mis, data = df)

df_Brazil <- subset(df, COUNTRY == "Brazil")
model_3 <- lm(illiteracy ~ distmiss + lati + longi + mesorregi, data = df_Brazil)
model_4 <- lm(illiteracy ~ distmiss + lati + longi + area + tempe + alti + preci + rugg + river + coast + mesorregi, data = df_Brazil)

df_arg <- subset(df, COUNTRY == "Argentina")
model_5 <- lm(illiteracy ~ distmiss + lati + longi + mis, data = df_arg)
model_6 <- lm(illiteracy ~ distmiss + lati + longi + area + tempe + alti + preci + rugg + river + coast + corr, data = df_arg)

df_pry <- subset(df, COUNTRY == "Paraguay")
model_7 <- lm(illiteracy ~ distmiss + ita, data = df_pry)
model_8 <- lm(illiteracy ~ distmiss + area + tempe + alti + preci + rugg + river + coast + ita, data = df_pry)

## CONLEY -----------------------------------------------------------------------
# Multiply the degrees of separation of longitude and latitude by 110575 to get the corresponding linear distances in meters.
# https://msi.nga.mil/api/publications/download?key=16920949/SFH00000/Calculators/degree.html&type=view

conley_1 <- conleyreg(illiteracy ~ distmiss + lati + longi + corr + ita + mis_pry + mis, data = df,
                      dist_cutoff = 11.0575, lat = "lati", lon = "longi")
conley_2 <- conleyreg(illiteracy ~ distmiss + lati + longi + area + tempe + alti + preci + rugg + river + coast + corr + ita + mis_pry + mis, data = df,
                      dist_cutoff = 11.0575, lat = "lati", lon = "longi")

conley_3 <- conleyreg(illiteracy ~ distmiss + lati + longi + mesorregi, data = df_Brazil, 
                      dist_cutoff = 11.0575, lat = "lati", lon = "longi")
conley_4 <- conleyreg(illiteracy ~ distmiss + lati + longi + area + tempe + alti + preci + rugg + river + coast + mesorregi, data = df_Brazil,
                      dist_cutoff = 11.0575, lat = "lati", lon = "longi")

conley_5 <- conleyreg(illiteracy ~ distmiss + lati + longi + corr, data = df_arg,
                      dist_cutoff = 11.0575, lat = "lati", lon = "longi")
conley_6 <- conleyreg(illiteracy ~ distmiss + lati + longi + area + tempe + alti + preci + rugg + river + coast + corr, data = df_arg,
                      dist_cutoff = 11.0575, lat = "lati", lon = "longi")

conley_7 <- conleyreg(illiteracy ~ distmiss + ita, data =  df_pry,
                      dist_cutoff = 11.0575, lat = "lati", lon = "longi")
conley_8 <- conleyreg(illiteracy ~ distmiss + area + tempe + alti + preci + rugg + river + coast + ita, data =  df_pry,
                      dist_cutoff = 11.0575, lat = "lati", lon = "longi")

# B.3 ---------------------------------------------------------------------
df_dist <- df %>%
  filter(complete.cases(distmiss)) %>% 
  mutate(distmiss_log = log(distmiss+1),
         distmiss_dec = exp(-0.01 * distmiss),
         distmiss_root = sqrt(distmiss))

breaks <- seq(min(df_dist$distmiss), max(df_dist$distmiss), length.out = 10)
labels <- sprintf("%.1f to %.1f", breaks, c(breaks[-1], max(df_dist$distmiss)))

Dist_1 <- ggplot() +
  geom_sf(data = df_dist, aes(geometry = geometry, fill = distmiss), color = "gray") +
  geom_sf(data = Region, aes(geometry = geometry), color = "#737373", fill = NA) +
  geom_sf(data = Country, aes(geometry = geometry), color = "black", fill = NA) +
  scale_fill_gradient(name = "Distance in Km:",
                      low = "#081d58", high = "#deebf7",
                      breaks = breaks,
                      labels = labels) +
  theme_light() +
  scale_size_identity() +
  labs(x = NULL, y = NULL,
       title = "Linear Distance from Guaraní Jesuit Missions",
       subtitle = "Municipal Level") +
  theme(plot.title = element_text(size = 12, face = "bold"),
        plot.subtitle = element_text(size = 11, face = "italic"),
        legend.position = "inside",
        legend.position.inside = c(0.09, 0.20),
        legend.box.just = "left",
        legend.box.background = element_rect(color = "black", linetype = "solid")) +
  geom_label(data = Region[Region$NAME_1 %in% Jesuit, ], aes(x = X, y = Y, label = NAME_1),
             size = 4, color = "black", fill = "white", fontface = "bold",
             label.padding = unit(0.5, "lines")) +
  guides(fill = "legend") +
  coord_sf(xlim = c(-59.5, -49.7), ylim = c(-33.5, -25.75))

breaks_log <- seq(min(df_dist$distmiss_log), max(df_dist$distmiss_log), length.out = 10)
labels_log <- sprintf("%.1f to %.1f", breaks_log, c(breaks_log[-1], max(df_dist$distmiss_log)))

Dist_2 <- ggplot() +
  geom_sf(data = df_dist, aes(geometry = geometry, fill = distmiss_log), color = "gray") +
  geom_sf(data = Region, aes(geometry = geometry), color = "#737373", fill = NA) +
  geom_sf(data = Country, aes(geometry = geometry), color = "black", fill = NA) +
  scale_fill_gradient(name = "Distance:",
                      low = "#3f007d", high = "#d4b9da",
                      breaks = breaks_log,
                      labels = labels_log) +
  theme_light() +
  scale_size_identity() +
  labs(x = NULL, y = NULL,
       title = "Logarithmic Distance from Guaraní Jesuit Missions",
       subtitle = "Municipal Level") +
  theme(plot.title = element_text(size = 12, face = "bold"),
        plot.subtitle = element_text(size = 11, face = "italic"),
        legend.position = "inside",
        legend.position.inside = c(0.09, 0.20),
        legend.box.just = "left",
        legend.box.background = element_rect(color = "black", linetype = "solid")) +
  geom_label(data = Region[Region$NAME_1 %in% Jesuit, ], aes(x = X, y = Y, label = NAME_1),
             size = 4, color = "black", fill = "white", fontface = "bold",
             label.padding = unit(0.5, "lines")) +
  guides(fill = "legend") +
  coord_sf(xlim = c(-59.5, -49.7), ylim = c(-33.5, -25.75))

breaks_dec <- seq(min(df_dist$distmiss_dec), max(df_dist$distmiss_dec), length.out = 10)
labels_dec <- sprintf("%.1f to %.1f", breaks_dec, c(breaks_dec[-1], max(df_dist$distmiss_dec)))

Dist_3 <- ggplot() +
  geom_sf(data = df_dist, aes(geometry = geometry, fill = distmiss_dec), color = "gray") +
  geom_sf(data = Region, aes(geometry = geometry), color = "#737373", fill = NA) +
  geom_sf(data = Country, aes(geometry = geometry), color = "black", fill = NA) +
  scale_fill_gradient(name = "Distance",
                      low = "#f7fcb9", high = "#004529",
                      breaks = breaks_dec,
                      labels = labels_dec) +
  theme_light() +
  scale_size_identity() +
  labs(x = NULL, y = NULL,
       title = "Distance from Guaraní Jesuit Missions",
       subtitle = "Municipal Level") +
  theme(plot.title = element_text(size = 12, face = "bold"),
        plot.subtitle = element_text(size = 11, face = "italic"),
        legend.position = "inside",
        legend.position.inside = c(0.09, 0.20),
        legend.box.just = "left",
        legend.box.background = element_rect(color = "black", linetype = "solid")) +
  geom_label(data = Region[Region$NAME_1 %in% Jesuit, ], aes(x = X, y = Y, label = NAME_1),
             size = 4, color = "black", fill = "white", fontface = "bold",
             label.padding = unit(0.5, "lines")) +
  guides(fill = "legend") +
  coord_sf(xlim = c(-59.5, -49.7), ylim = c(-33.5, -25.75))

breaks_root <- seq(min(df_dist$distmiss_root), max(df_dist$distmiss_root), length.out = 10)
labels_root <- sprintf("%.1f to %.1f", breaks_root, c(breaks_root[-1], max(df_dist$distmiss_root)))

Dist_4 <- ggplot() +
  geom_sf(data = df_dist, aes(geometry = geometry, fill = distmiss_root), color = "gray") +
  geom_sf(data = Region, aes(geometry = geometry), color = "#737373", fill = NA) +
  geom_sf(data = Country, aes(geometry = geometry), color = "black", fill = NA) +
  scale_fill_gradient(name = "Root Distance:",
                      low = "#800035", high = "#ffffcc",
                      breaks = breaks_root,
                      labels = labels_root) +
  theme_light() +
  scale_size_identity() +
  labs(x = NULL, y = NULL,
       title = "Distance from Guaraní Jesuit Missions",
       subtitle = "Municipal Level") +
  theme(plot.title = element_text(size = 12, face = "bold"),
        plot.subtitle = element_text(size = 11, face = "italic"),
        legend.position = "inside",
        legend.position.inside = c(0.09, 0.20),
        legend.box.just = "left",
        legend.box.background = element_rect(color = "black", linetype = "solid")) +
  geom_label(data = Region[Region$NAME_1 %in% Jesuit, ], aes(x = X, y = Y, label = NAME_1),
             size = 4, color = "black", fill = "white", fontface = "bold",
             label.padding = unit(0.5, "lines")) +
  guides(fill = "legend") +
  coord_sf(xlim = c(-59.5, -49.7), ylim = c(-33.5, -25.75))

## ALTERNATIVE DISTANCE--------------
model_a <- lm(illiteracy ~ distmiss + lati + longi + corr + ita + mis_pry + mis, data = df_dist)
model_b <- lm(illiteracy ~ distmiss_log + lati + longi + corr + ita + mis_pry + mis, data = df_dist)
model_c <- lm(illiteracy ~ distmiss_dec + lati + longi + corr + ita + mis_pry + mis, data = df_dist)
model_d <- lm(illiteracy ~ distmiss_root + lati + longi + corr + ita + mis_pry + mis, data = df_dist)

## STARGAZER --------------------------------------------------------------------
list_1 <- list(model_1, model_2, model_3, model_4, model_5, model_6, model_7, model_8)
stargazer(list_1, title = "Replication Baseline Table", digits = 4,  type = "latex")

list_2 <- list(conley_1, conley_2, conley_3, conley_4, conley_5, conley_6, conley_7, conley_8)
stargazer(list_2, title = "Models with Conley SE", digits = 3, type = "latex")

stargazer(model_a, model_b, model_c, model_d, title = "Models with different distances", digits = 4, type = "latex")

# SAVING -----------------------------------------------------------------------
## PNG ---------
# ggsave("plot_00.png", plot = plot_00, device = "png", path = "./03_Plots/.png")
# ggsave("plot_01.png", plot = plot_01, device = "png", path = "./03_Plots/.png")
# ggsave("plot_02.png", plot = plot_02, device = "png", path = "./03_Plots/.png")
# ggsave("plot_03.png", plot = plot_03, device = "png", path = "./03_Plots/.png")
# tmap_save(plot_04, filename = "plot_04.png")
# ggsave("Dist_01.png", plot = Dist_1, device = "png", path = "./03_Plots/.png")
# ggsave("Dist_02.png", plot = Dist_2, device = "png", path = "./03_Plots/.png")
# ggsave("Dist_03.png", plot = Dist_3, device = "png", path = "./03_Plots/.png")
# ggsave("Dist_04.png", plot = Dist_4, device = "png", path = "./03_Plots/.png")
# 
# ## PDF -------------------
# ggsave("plot_00.pdf", plot = plot_00, device = "pdf", path = "./03_Plots/.pdf")
# ggsave("plot_01.pdf", plot = plot_01, device = "pdf", path = "./03_Plots/.pdf")
# ggsave("plot_02.pdf", plot = plot_02, device = "pdf", path = "./03_Plots/.pdf")
# ggsave("plot_03.pdf", plot = plot_03, device = "pdf", path = "./03_Plots/.pdf")
# tmap_save(plot_04, filename = "plot_04.pdf")
# ggsave("Dist_01.pdf", plot = Dist_1, device = "pdf", path = "./03_Plots/.pdf")
# ggsave("Dist_02.pdf", plot = Dist_2, device = "pdf", path = "./03_Plots/.pdf")
# ggsave("Dist_03.pdf", plot = Dist_3, device = "pdf", path = "./03_Plots/.pdf")
# ggsave("Dist_04.pdf", plot = Dist_4, device = "pdf", path = "./03_Plots/.pdf")

# WORKSPACE --------------------------------------------------------------------
#save.image("Workspace_B")
