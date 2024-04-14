
# DEPENDENCIES -----------------------------------------------------------------
source("./02_Code/__packages.R")
getwd()
# LOADING DATA -----------------------------------------------------------------
shp <- st_read("./01_Data-input/EU/EU27.shp")

head(shp)
st_crs(shp)
class(shp)
class(shp$geometry)

#countries of interest
shp_6 <- shp %>%
  filter(grepl("^AT|^DE|^IT|^FR|^ES|^PT", Id))

#load productivity data and check
load(file="./01_Data-input/data1.rda")
glimpse(data1)

#subset of original data set
#pr80b and pr103b = region’s productivity in 1980 and 2013
#lninv1b = the log of investment
#lndens.empb = the log of density of employment

selected_vars <- c("IDb", "pr80b","pr103b", "lninv1b", "lndens.empb")
data2 <- data1[, selected_vars]

#merge data and check
shp_6merged <- left_join(shp_6, data2, by=c("Id"="IDb"))

str(shp_6merged)

#growth rate of productivity from 1980 to 2013
shp_6merged$gr <- (shp_6merged$pr103b-shp_6merged$pr80b)/shp_6merged$pr80b
summary(shp_6merged$gr)
shp_6mergedc <- na.exclude(shp_6merged)
shp_6mergedc <- shp_6mergedc %>%
  mutate(gr.quant= cut(gr, 
                          breaks = c(quantile(shp_6mergedc$gr, probs = seq(0,1, by=0.2))), 
                          include.lowest = TRUE))
summary(shp_6mergedc$gr.quant)

mapA1 <- ggplot(data = shp_6mergedc) +
  geom_sf(aes(fill = gr.quant), colour = "black") +
  scale_fill_viridis_d(option = "viridis", direction = 1) +  
  theme_void() +
  theme(
    plot.margin = margin(0, 1, 0, 1, "cm"),  # Adjust the plot margins
    legend.position = "right",
    legend.key.size = unit(0.5, "cm"),  # Adjust the size of the legend key
    legend.text = element_text(size = 10)  # Adjust the size of legend text
  ) +
  labs(title = "Productivity Growth",
       subtitle = "NUTS2", 
       fill = "Quantiles")

print(mapA1)

#spatial weights matrices

#centroids
coords <- st_coordinates(st_centroid(shp_6mergedc))

##Queen contiguity
queen_nb <- poly2nb(shp_6mergedc, row.names=shp_6mergedc$Id, queen=TRUE)
W.list.queen <- nb2listw(queen_nb, style = "W", zero.policy=TRUE) #row-standardized with style=B
W.queen <- listw2mat(W.list.queen)
##Distance band
distw <- dnearneigh(coords, 0, 100, row.names=shp_6mergedc$Id)
summary(distw)
k1 <- knearneigh(coords, k=1)
k1 <- knn2nb(k1)
link.max <- max(unlist(nbdists(k1, coords=coords)))
link.max
distw <- dnearneigh(coords, 0, link.max, row.names=shp_6mergedc$Id)
W.list <- nb2listw(distw, style="W", zero.policy=FALSE)
W.dist <- listw2mat(W.list)

#Distance decay
dnbdists <- nbdists(distw, coords)
gl <- lapply(dnbdists, function(x) {
  1/x
  })
W.list.gl <- nb2listw(distw, glist=gl, zero.policy=FALSE)
W.distdec <- listw2mat(W.list.gl)

#Comparing weights matrices: W.queen, W.list, W.list.gl
degree_queen <- rowSums(W.queen)
degree_dist <- rowSums(W.dist)
degree_distdec <- rowSums(W.distdec)
degree0_ids <- names(degree_queen)[degree_queen == 0]
names_for_degree0 <- shp_6mergedc$Name[shp_6mergedc$Id %in% degree0_ids]
print(names_for_degree0)
###dist and distdec no 0 degree

#Plot weight matrics
# Saving the image for W.queen
png("./03_Plots/.png/W_queen.png")
image(W.queen, main = "Adjacency Matrix - Queen Contiguity", axes = FALSE)
dev.off()

# Saving the image for W.dist
png("./03_Plots/.png/W_dist.png")
image(W.dist, main = "Adjacency Matrix - Distance", axes = FALSE)
dev.off()

# Saving the image for W.distdec
png("./03_Plots/.png/W_distdec.png")
image(W.distdec, main = "Adjacency Matrix - Distance Decay", axes = FALSE)
dev.off()


#Visualizing the network
##function to plot a network
plot_network <- function(W, title) {
  G <- graph.adjacency(as.matrix(W), mode = "undirected", weighted = TRUE)
  plot(G, main = title, vertex.label = NA, vertex.size = 5, vertex.color = "lightblue")
}

# Plot Queen Contiguity network
png("./03_Plots/.png/Queen_Network.png")
plot_network(W.queen, "Queen Contiguity Network")
dev.off()

# Plot Distance Band network
png("./03_Plots/.png/Distance_Network.png")
plot_network(W.dist, "Distance Band Network")
dev.off()

# Plot Distance Decay network
png("./03_Plots/.png/Distance_Dec_Network.png")
plot_network(W.distdec, "Distance Decay Network")
dev.off()

#Spatial autocorrelation

##Evidence for spatial dependence being present: Global Moran's I
###Queen contiguity
moran.test(shp_6mergedc$gr, listw = W.list.queen, alternative = "greater")
###Distance threshold
moran.test(shp_6mergedc$gr, listw = W.list, alternative = "greater")
###Distance decay
moran.test(shp_6mergedc$gr, listw = W.list.gl, alternative = "greater")


#OLS estimation & spatial autocorrelation
ols_fit <- lm(gr ~ pr80b + lninv1b + lndens.empb, data = shp_6mergedc)
summary(ols_fit)
stargazer(ols_fit,
          title = "Linear regression of productivity growth",
          type = "latex",
          out = "table_ols_fit.tex")

#check the errors of the OLS model for spatial dependence 
lm.morantest(ols_fit, W.list.queen)
lm.morantest(ols_fit, W.list)
lm.morantest(ols_fit, W.list.gl)

#test is significant and thus we reject the H0 of uncorrelated errors
#plot the errors and the fitted values:
shp_6mergedc$u_ols <- ols_fit$residuals
shp_6mergedc$y_hat <- ols_fit$fitted.values

shp_6mergedc <- shp_6mergedc %>%
  mutate(u_ols.quant = cut(u_ols, 
                           breaks = c(quantile(shp_6mergedc$u_ols, probs = seq(0,1, by=0.2))), 
                           include.lowest = TRUE), 
         y_hat.quant = cut(y_hat, 
                           breaks = c(quantile(shp_6mergedc$y_hat, probs = seq(0,1, by=0.2))), 
                           include.lowest = TRUE))

mapA2 <- ggplot(data = shp_6mergedc) +
  geom_sf(aes(fill = y_hat.quant), colour = "black") +
  scale_fill_viridis_d(option = "viridis") +  
  theme_void() +
  theme(
    plot.margin = margin(0, 1, 0, 1, "cm"),  # Adjust the plot margins
    legend.position = "right",  # Move legend to the bottom
    legend.key.size = unit(0.5, "cm"),  # Adjust the size of the legend key
    legend.text = element_text(size = 10)  # Adjust the size of legend text
  ) +
  labs(title = "Fitted values", fill = "Quantiles")
print(mapA2)

mapA3 <- ggplot(data = shp_6mergedc) +
  geom_sf(aes(fill = u_ols.quant), colour = "black") +
  scale_fill_viridis_d(option = "viridis") +  
  theme_void() +
  theme(
    plot.margin = margin(0, 1, 0, 1, "cm"),  # Adjust the plot margins
    legend.position = "right",  # Move legend to the bottom
    legend.key.size = unit(0.5, "cm"),  # Adjust the size of the legend key
    legend.text = element_text(size = 10)  # Adjust the size of legend text
  ) +
  labs(title = "Residuals", fill = "Quantiles")
print(mapA3)


# SAVING -----------------------------------------------------------------------
ggsave("mapA1.png", plot = mapA1, device = "png", path = "./03_Plots/.png")
ggsave("mapA2.png", plot = mapA2, device = "png", path = "./03_Plots/.png")
ggsave("mapA3.png", plot = mapA3, device = "png", path = "./03_Plots/.png")
